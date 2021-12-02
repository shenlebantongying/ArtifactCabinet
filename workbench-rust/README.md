# Type system

Ownership (aka _affine_ or _substructural_ type system)

## Easy things that are supposed to be done easily, but in rust you have to do it in a CRAP way without actual benefit since they are so obvious.

+ Compare enum type
  + Most simple way `#[derive(Copy, Clone, PartialEq, Eq)]`
  + Pattern matching
+ Construct a tree
  + `Rc<RefCell<T>>`?
  + `Box<T>`?

# SuckLess libraries
+ Rayon -> data-parallelism
+ StructOpt -> faster cli interface
+ pyo3 -> create python things

+ Rouille -> quick web

+ cargo-edit

+ Single file for Code runner of vscode => `"rust": "cd $dir && rustc $fileName -o $fileName.exe && $dir$fileName.exe"` (add .exe to .gitignore)