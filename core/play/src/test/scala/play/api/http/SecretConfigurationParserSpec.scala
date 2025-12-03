/*
 * Copyright (C) Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.http

import org.specs2.mutable.Specification
import play.api.Configuration
import play.api.Environment
import play.api.Mode
import play.api.PlayException

class ActualKeySecretConfigurationParserSpec extends SecretConfigurationParserSpec {
  override def secretKey: String = "play.http.secret.key"
}

class DeprecatedKeySecretConfigurationParserSpec extends SecretConfigurationParserSpec {
  override def secretKey: String = "play.crypto.secret"

  override def parseSecret(mode: Mode, secret: Option[String] = None, flashJWTAlgorithm: Option[String] = None) = {
    HttpConfiguration
    .fromConfiguration(
        Configuration.reference
        ++ Configuration.from(
          secret.map(secretKey -> _).toMap ++ flashJWTAlgorithm.map(flashCookieAlgorithm -> _).toMap ++ Map(
            "play.http.secret.key" -> null
          )
        ),
        Environment.simple(mode = mode)
      )
      .secret
      .secret
  }

}

trait SecretConfigurationParserSpec extends Specification {

  def secretKey: String
  def flashCookieAlgorithm: String = "play.http.flash.jwt.signatureAlgorithm"

  val Secret32Bytes = "abcdefghijklmnopqrstuvwxyz123456" // => 256 bits, required for HS256 (the default algorithm)
  val Secret31Bytes = "abcdefghijklmnopqrstuvwxyz12345"  // => 248 bits, too short for HS256

  val Secret = Secret32Bytes

  def parseSecret(mode: Mode, secret: Option[String] = None, flashJWTAlgorithm: Option[String] = None): String = {
    HttpConfiguration
    .fromConfiguration(
        Configuration.reference
        ++ Configuration
          .from(secret.map(secretKey -> _).toMap ++ flashJWTAlgorithm.map(flashCookieAlgorithm -> _).toMap),
        Environment.simple(mode = mode)
      )
      .secret
      .secret
  }

  "Secret config parser" should {
    "parse the secret" in {

      "load a configured secret in prod" in {
        parseSecret(Mode.Prod, Some(Secret)) must_== Secret
      }
      "load a configured secret in dev" in {
        parseSecret(Mode.Dev, Some(Secret)) must_== Secret
      }
      "throw an exception if secret is changeme in prod" in {
        parseSecret(Mode.Prod, Some("changeme")) must throwA[PlayException]
      }
      "throw an exception if no secret in prod" in {
        parseSecret(Mode.Prod, Some(null)) must throwA[PlayException]
      }
      "throw an exception if secret is blank in prod" in {
        parseSecret(Mode.Prod, Some("  ")) must throwA[PlayException]
      }
      "throw an exception if secret is empty in prod" in {
        parseSecret(Mode.Prod, Some("")) must throwA[PlayException]
      }
      "generate a secret if secret is changeme in dev" in {
        parseSecret(Mode.Dev, Some("changeme")) must_!= "changeme"
      }
      "generate a secret if no secret in dev" in {
        parseSecret(Mode.Dev) must_!= ""
      }
      "generate a secret if secret is blank in dev" in {
        parseSecret(Mode.Dev, Some("  ")) must_!= "  "
      }
      "generate a secret if secret is empty in dev" in {
        parseSecret(Mode.Dev, Some("")) must_!= ""
      }
      "generate a stable secret in dev" in {
        parseSecret(Mode.Dev, Some("changeme")) must_!= "changeme"
      }
      "throw an exception if secret is too short in prod" in {
        parseSecret(Mode.Prod, Some(Secret31Bytes)) must throwA[PlayException].like {
          case e =>
            e.getMessage must beEqualTo(
              """Configuration error[
                |The application secret is too short and does not have the recommended amount of entropy for algorithm HS256 defined at play.http.session.jwt.signatureAlgorithm.
                |Current application secret bits: 248, minimal required bits for algorithm HS256: 256.
                |To set the application secret, please read https://playframework.com/documentation/latest/ApplicationSecret
                |]""".stripMargin
            )
        }
      }
      "throw an exception if secret is too short in dev" in {
        parseSecret(Mode.Dev, Some(Secret31Bytes)) must throwA[PlayException].like {
          case e =>
            e.getMessage must beEqualTo(
              """Configuration error[
                |The application secret is too short and does not have the recommended amount of entropy for algorithm HS256 defined at play.http.session.jwt.signatureAlgorithm.
                |Current application secret bits: 248, minimal required bits for algorithm HS256: 256.
                |To set the application secret, please read https://playframework.com/documentation/latest/ApplicationSecret
                |]""".stripMargin
            )
        }
      }
      "throw an exception if secret is ok for session cookie but too short for flash cookie in prod" in {
        System.out.println("running with 512")
        parseSecret(Mode.Prod, Some(Secret32Bytes), flashJWTAlgorithm = Some("HS512")) must throwA[PlayException].like {
          case e =>
            e.getMessage must beEqualTo(
              """Configuration error[
                |The application secret is too short and does not have the recommended amount of entropy for algorithm HS512 defined at play.http.flash.jwt.signatureAlgorithm.
                |Current application secret bits: 256, minimal required bits for algorithm HS512: 512.
                |To set the application secret, please read https://playframework.com/documentation/latest/ApplicationSecret
                |]""".stripMargin
            )
        }
      }
      "throw an exception if secret is ok for session cookie but too short for flash cookie in dev" in {
        parseSecret(Mode.Dev, Some(Secret32Bytes), flashJWTAlgorithm = Some("HS512")) must throwA[PlayException].like {
          case e =>
            e.getMessage must beEqualTo(
              """Configuration error[
                |The application secret is too short and does not have the recommended amount of entropy for algorithm HS512 defined at play.http.flash.jwt.signatureAlgorithm.
                |Current application secret bits: 256, minimal required bits for algorithm HS512: 512.
                |To set the application secret, please read https://playframework.com/documentation/latest/ApplicationSecret
                |]""".stripMargin
            )
        }
      }
    }
  }
}
