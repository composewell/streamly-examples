use walkdir::WalkDir;
use std::env;

fn main() {
    // Get the directory path from command-line arguments or default to the current directory
    let args: Vec<String> = env::args().collect();
    let dir = args.get(1).map_or(".", |s| s.as_str());

    // Traverse the directory recursively
    for entry in WalkDir::new(dir).into_iter().filter_map(|e| e.ok()) {
        println!("{}", entry.path().display());
    }
}
