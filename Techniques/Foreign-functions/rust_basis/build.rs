fn main() {
    println!("cargo:rustc-link-lib=dylib=pointer-types");
    println!("cargo:rustc-link-search=../ftype");
}
