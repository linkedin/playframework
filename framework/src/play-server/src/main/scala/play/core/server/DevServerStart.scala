/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package play.core.server

import java.io._
import java.util.Properties
import play.api._
import play.api.mvc._
import play.api.libs.concurrent.ActorSystemProvider
import play.core._
import play.utils.Threads
import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

/**
 * Used to start servers in 'dev' mode, a mode where the application
 * is reloaded whenever its source changes.
 */
object DevServerStart {

  /**
   * Provides an HTTPS-only server for the dev environment.
   *
   * <p>This method uses simple Java types so that it can be used with reflection by code
   * compiled with different versions of Scala.
   */
  def mainDevOnlyHttpsMode(
    buildLink: BuildLink,
    buildDocHandler: BuildDocHandler,
    httpsPort: Int,
    httpAddress: String): ServerWithStop = {
    mainDev(buildLink, buildDocHandler, None, Some(httpsPort), httpAddress)
  }

  /**
   * Provides an HTTP server for the dev environment
   *
   * <p>This method uses simple Java types so that it can be used with reflection by code
   * compiled with different versions of Scala.
   */
  def mainDevHttpMode(
    buildLink: BuildLink,
    buildDocHandler: BuildDocHandler,
    httpPort: Int,
    httpAddress: String): ServerWithStop = {
    mainDev(buildLink, buildDocHandler, Some(httpPort), Option(System.getProperty("https.port")).map(Integer.parseInt(_)), httpAddress)
  }

  private def mainDev(
    buildLink: BuildLink,
    buildDocHandler: BuildDocHandler,
    httpPort: Option[Int],
    httpsPort: Option[Int],
    httpAddress: String): ServerWithStop = {
    val classLoader = getClass.getClassLoader
    Threads.withContextClassLoader(classLoader) {
      try {
        val process = new RealServerProcess(args = Seq.empty)
        val path: File = buildLink.projectPath

        val dirAndDevSettings: Map[String, String] = ServerConfig.rootDirConfig(path) ++ buildLink.settings.asScala.toMap

        // Use plain Java call here in case of scala classloader mess
        {
          if (System.getProperty("play.debug.classpath") == "true") {
            System.out.println("\n---- Current ClassLoader ----\n")
            System.out.println(this.getClass.getClassLoader)
            System.out.println("\n---- The where is Scala? test ----\n")
            System.out.println(this.getClass.getClassLoader.getResource("scala/Predef$.class"))
          }
        }

        // First delete the default log file for a fresh start (only in Dev Mode)
        try {
          new File(path, "logs/application.log").delete()
        } catch {
          case NonFatal(_) =>
        }

        // Configure the logger for the first time.
        // This is usually done by Application itself when it's instantiated, which for other types of ApplicationProviders,
        // is usually instantiated along with or before the provider.  But in dev mode, no application exists initially, so
        // configure it here.
        Logger.init(path, Mode.Dev)

        println(play.utils.Colors.magenta("--- (Running the application, auto-reloading is enabled) ---"))
        println()

        // Create reloadable ApplicationProvider
        val appProvider = new ApplicationProvider {
          // Use a stamped lock over a synchronized block so we can better control concurrency and avoid
          // blocking.  This improves performance from 4851.53 req/s to 7133.80 req/s and fixes #7614.
          // Arguably performance shouldn't matter because load tests should be run against a production
          // configuration, but there's no point in making it slower than it has to be...
          val sl = new java.util.concurrent.locks.StampedLock

          var lastState: Try[Application] = Failure(new PlayException("Not initialized", "?"))
          var currentWebCommands: Option[WebCommands] = None

          override def current: Option[Application] = lastState.toOption

          def get: Try[Application] = {
            // reload checks if anything has changed, and if so, returns an updated classloader.
            // This returns a boolean and changes happen asynchronously on another thread.
            val reloaded = buildLink.reload match {
              case NonFatal(t) => Failure(t)
              case cl: ClassLoader => Success(Some(cl))
              case null => Success(None)
            }

            // Blocking happens in the Netty IO thread.  There is no good way to avoid this -- even
            // in the previous implementation, there was an Await.result(Future(), Duration.Inf),
            // so we can avoid the flow of control switch and (hopefully) make it a little faster by
            // blocking directly.
            reloaded.flatMap {
              case Some(projectClassloader) =>
                // After this point we are actively changing the state of the application, so
                // block until we can grab a write lock...
                val writeStamp = sl.writeLock()
                try {
                  reload(projectClassloader)
                } finally {
                  sl.unlockWrite(writeStamp)
                }
              case None =>
                // Returning an unchanged application happens frequently in dev mode, and
                // so we want to return as fast as possible using an optimistic read:
                //
                // http://www.javaspecialists.eu/archive/Issue215.html
                // http://www.jfokus.se/jfokus13/preso/jf13_PhaserAndStampedLock.pdf
                // http://www.dre.vanderbilt.edu/~schmidt/cs892/2017-PDFs/L5-Java-ReadWriteLocks-pt3-pt4.pdf
                // http://blog.takipi.com/java-8-stampedlocks-vs-readwritelocks-and-synchronized/

                var readLock = sl.tryOptimisticRead()
                var tryApp = lastState
                if (!sl.validate(readLock)) {
                  // if a write occurred since the optimistic read, try again with a blocking read lock.
                  // it will take multiple seconds for the reload to complete, so it's better to block
                  // than spin-wait here.
                  readLock = sl.readLock()
                  try {
                    tryApp = lastState
                  } finally {
                    sl.unlockRead(readLock)
                  }
                }
                tryApp
            }
          }

          override def handleWebCommand(request: play.api.mvc.RequestHeader): Option[Result] = {
            buildDocHandler.maybeHandleDocRequest(request).asInstanceOf[Option[Result]].orElse(
              currentWebCommands.flatMap(_.handleWebCommand(request, buildLink, path))
            )

          }

          def reload(projectClassloader: ClassLoader): Try[Application] = {
            try {
              if (lastState.isSuccess) {
                println()
                println(play.utils.Colors.magenta("--- (RELOAD) ---"))
                println()
              }

              val reloadable = this

              // First, stop the old application if it exists
              lastState.foreach(Play.stop)

              // Create the new environment
              val environment = Environment(path, projectClassloader, Mode.Dev)
              val sourceMapper = new SourceMapper {
                def sourceOf(className: String, line: Option[Int]) = {
                  Option(buildLink.findSource(className, line.map(_.asInstanceOf[java.lang.Integer]).orNull)).flatMap {
                    case Array(file: java.io.File, null) => Some((file, None))
                    case Array(file: java.io.File, line: java.lang.Integer) => Some((file, Some(line)))
                    case _ => None
                  }
                }
              }

              val webCommands = new DefaultWebCommands
              currentWebCommands = Some(webCommands)

              val newApplication = Threads.withContextClassLoader(projectClassloader) {
                val context = ApplicationLoader.createContext(environment, dirAndDevSettings, Some(sourceMapper), webCommands)
                val loader = ApplicationLoader(context)
                loader.load(context)
              }

              Play.start(newApplication)
              lastState = Success(newApplication)
              lastState
            } catch {
              case e: PlayException => {
                lastState = Failure(e)
                lastState
              }
              case NonFatal(e) => {
                lastState = Failure(UnexpectedException(unexpected = Some(e)))
                lastState
              }
              case e: LinkageError => {
                lastState = Failure(UnexpectedException(unexpected = Some(e)))
                lastState
              }
            }
          }
        }

        // Start server with the application
        val serverConfig = ServerConfig(
          rootDir = path,
          port = httpPort,
          sslPort = httpsPort,
          address = httpAddress,
          mode = Mode.Dev,
          properties = process.properties,
          configuration = Configuration.load(classLoader, System.getProperties, dirAndDevSettings, allowMissingApplicationConf = true)
        )
        val (actorSystem, actorSystemStopHook) = ActorSystemProvider.start(classLoader, serverConfig.configuration)
        val serverContext = ServerProvider.Context(serverConfig, appProvider, actorSystem, actorSystemStopHook)
        val serverProvider = ServerProvider.fromConfiguration(classLoader, serverConfig.configuration)
        serverProvider.createServer(serverContext)
      } catch {
        case e: ExceptionInInitializerError => throw e.getCause
      }

    }
  }

}