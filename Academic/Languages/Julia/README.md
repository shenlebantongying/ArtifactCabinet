# Julia


## Term rewriting

https://github.com/JuliaSymbolics/SymbolicUtils.jl

https://github.com/JuliaSymbolics/Metatheory.jl

see Software Design for Flexibility's pattern matcher

see e-graphs https://egraphs-good.github.io/

# Pkg.jl Notes

```sh
activate
# uses an Environment with Project.toml & Manifest.toml

develop
# add a package to the current active Environment.
# similar to `add` but auto tracking changes.
# for local packages, the path of pkg is recorded in the Project.toml
```

The `LOAD_PATH` variable in Julia is the Loading path magic.

- `@` -> active env
- `"@v#.#"` -> version specific env (aka the global main env)
