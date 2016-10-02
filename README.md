# scala-trace
This is a runtime code analyzing tool set for Scala programs. Two approaches of code analyzing are availalbe in this tool set.

1. Compiler Plugin 
We develop a compiler plugin and a runtime analyzing module. The compiler plugin will insert method calls to the analyzing module into the AST of the Scala program. Data flow information in the program are passed to the analyzing module through arguments of these method calls. For more details of the implementation please check our report. Here we only provide the usage of this plugin.

Usage:

1. Clone our project, switch to plugin-options branch.

2. Run sbt package.

3. The plugin jar will be generated in directory scala-trace-plugin/target/scala-2.11/

4. Add "-Xplugin:<path to the plugin jar> -P:scalatrace:targetline:<the source code line that writes the final result of the Scala program>"

Note that the target source code line in step 4 needs to specify the full path of the source file. The source file and the line number should be separated by a #, for example /Users/shiyu/Try.scala#line-14
