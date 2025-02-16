use std::fs;
use std::path::Path;

fn list_dir(path: &Path) {
    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.filter_map(|e| e.ok()) {
            let entry_path = entry.path();
            println!("{}", entry_path.display());

            if let Ok(metadata) = entry.metadata() {
                if metadata.is_dir() && !metadata.file_type().is_symlink() {
                    list_dir(&entry_path);
                }
            }
        }
    } else {
        eprintln!("Could not access: {}", path.display());
    }
}

fn main() {
    let dir = ".";
    let path = Path::new(dir);
    list_dir(path);
}
