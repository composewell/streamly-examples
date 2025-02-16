use walkdir::WalkDir;
use std::env;

fn main() {
    dir = ".";

    for entry in WalkDir::new(dir).into_iter().filter_map(|e| e.ok()) {
        println!("{}", entry.path().display());
    }
}
