#!/bin/bash
TEST_SOURCES_PATH=scala-trace-plugin/src/test/scala/ 
#scalac -Xprint:wrapper -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}MyObject.scala ${TEST_SOURCES_PATH}MyClass.scala ${TEST_SOURCES_PATH}MyTrait.scala
#scalac -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}Array.scala
#scalac -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}Regex.scala
#scalac -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}Iterator.scala 
#scalac -Xprint:cleanup -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}hello.scala
#scalac -Xprint:cleanup -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}Singleton.scala
scalac -Xprint:wrapper -Xplugin:scala-trace-plugin/target/scala-2.11/scala-trace-plugin_2.11-1.0.0.jar -cp scala-trace-runtime/target/scala-2.11/scala-trace-runtime_2.11-1.0.0.jar ${TEST_SOURCES_PATH}Compiler.scala ${TEST_SOURCES_PATH}SubComponent.scala
