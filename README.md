# Publish Locally

```
;set every publishMavenStyle := false;set every publishTo := Some(Resolver.file("local-repo", file(System.getProperty("user.home")) / "local-repo")(Patterns(Vector("[orgPath]/[module]/[revision]/[module]-[revision].ivy"),Vector("[orgPath]/[module]/[revision]/[artifact]-[revision](-[classifier]).[ext]"), true, false, false)));clean;++2.12.15 publish
```

## Update local-repo ivy files

```
find ~/local-repo -type f -name "*<version>.ivy" -exec sed -i.bak 's/\<conf name=\"compile\" visibility=\"public\" description=\"\"\/\>/\<conf name=\"compile\" visibility=\"public\" description=\"\"\/>\
\<conf name=\"default\" visibility=\"public\" description=\"\" extends=\"compile\"\/\>/' "{}" +;"
# ^replace <version> with the version you just published
```

# Publish to internal JFrog

1. Create Jfrog account by requesting access through IT.
2. Get access to the 'playframework' repo by contacting the Jfrog admin.
3. Generate a token by clicking your username > Set Me Up > Maven > Generate Token.
4. Create a credentials file at `~/.sbt/.credentials` with the following content:
```
realm=Artifactory Realm
host=linkedin.jfrog.io
user=<USERNAME>
password=<TOKEN>
```
5. Run `sbt` and execute the following commands:
```
set every publishTo := Some("Artifactory Realm" at "https://linkedin.jfrog.io/artifactory/playframework");
set every credentials := List(Credentials(Path.userHome / ".sbt" / ".credentials"))
publish
```

[![Gitter](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/playframework/playframework?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [<img src="https://img.shields.io/travis/playframework/playframework.svg"/>](https://travis-ci.org/playframework/playframework) [![Maven](https://img.shields.io/maven-central/v/com.typesafe.play/play_2.11.svg)](http://mvnrepository.com/artifact/com.typesafe.play/play_2.11)


## Play Framework - The High Velocity Web Framework

The Play Framework combines productivity and performance making it easy to build scalable web applications with Java and Scala.  Play is developer friendly with a "just hit refresh" workflow and built-in testing support.  With Play, applications scale predictably due to a stateless and non-blocking architecture.  By being RESTful by default, including assets compilers, JSON & WebSocket support, Play is a perfect fit for modern web & mobile applications.

### Learn More

- [www.playframework.com](http://www.playframework.com)
- [Download](http://www.playframework.com/download)
- [Install](http://www.playframework.com/documentation/latest/Installing)
- [Create a new application](http://www.playframework.com/documentation/latest/NewApplication)
- [Play for Scala developers](http://www.playframework.com/documentation/latest/ScalaHome)
- [Play for Java developers](http://www.playframework.com/documentation/latest/JavaHome)
- [Build from source](http://www.playframework.com/documentation/latest/BuildingFromSource)
- [Search or create issues](https://github.com/playframework/playframework/issues)
- [Get help](http://stackoverflow.com/questions/tagged/playframework)
- [Contribute](https://www.playframework.com/contributing)

### License

Copyright (C) Lightbend Inc. (https://www.lightbend.com).

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this project except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
