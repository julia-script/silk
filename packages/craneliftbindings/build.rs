use std::env;
use std::path::PathBuf;

fn main() {
    // Notify Cargo to re-run the build script if these files change
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=cbindgen.toml");
    println!("cargo:rerun-if-changed=src/lib.rs");

    // Get the directory containing Cargo.toml of this crate
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    // Load cbindgen configuration
    let config = cbindgen::Config::from_file(PathBuf::from(&crate_dir).join("cbindgen.toml"))
        .expect("Failed to load cbindgen configuration");

    // Generate bindings
    let bindings = cbindgen::Builder::new()
        .with_crate(&crate_dir)
        .with_config(config)
        .generate()
        .expect("Unable to generate bindings");

    // Specify the output path within the crate's directory
    let out_path = PathBuf::from(&crate_dir)
        .join("headers")
        .join("cranelift.h");

    // Write the bindings to the specified path
    bindings.write_to_file(&out_path);
    // .expect("Failed to write bindings");

    println!("Generated header at {}", out_path.display());
}
