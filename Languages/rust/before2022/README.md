# Type system

Ownership (aka *affine* or *substructural* type system)

- Compare enum type
  - Most simple way `#[derive(Copy, Clone, PartialEq, Eq)]`
  - Pattern matching
- Construct a tree
  - `Rc<RefCell<T>>`?
  - `Box<T>`?

# Links

## Tour of Rust's Standard Library Traits

<https://github.com/pretzelhammer/rust-blog/blob/master/posts/tour-of-rusts-standard-library-traits.md>

## Traits: Rust's unifying concept

<https://darkcoding.net/software/traits-rusts-unifying-concept/>

## The Little Book of Rust Macros

<https://veykril.github.io/tlborm/>
