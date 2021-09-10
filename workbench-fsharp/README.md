# todo

Read most of the topics in this page https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/

Recite this log analysis thing
http://jyliao.blogspot.com/2011/03/log-analysis-with-f.html

# How to run fsharp?

repl: `dotnet fsi` -> You can send selection of code to the repl in Jetbrains Rider!

## As a script

`dotnet fsi ****.fsx` -> no need for `[<EntryPoint>]` at all

or
```
#!/usr/bin/env dotnet fsi
+things
```

## As a program

+ Single Entry Point (aka main function)
+ use the `dotnet` cli to create libraries or things

# Tooling tips

`dotnet tools` => fun additional programs
