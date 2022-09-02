use std::{env, fs, io};

struct Path {
    raw_path: String,
    is_dir: bool,
}

impl Path {
    fn new(path: String) -> Self {
        let is_dir = fs::metadata(&path).unwrap().is_dir();
        Self {
            raw_path: path,
            is_dir,
        }
    }
}

fn analyzer_main() -> {
    let jack_files = get_jack_files()?;
}

fn get_jack_files() -> Result<Vec<String>, &'static str> {
    let mut path = Path::new(String::new());
    path.raw_path = env::args().into_iter().next().unwrap();

    let mut jack_files: Vec<String> = Vec::new();

    if path.is_dir {
        jack_files = fs::read_dir(path.raw_path)
            .unwrap()
            .filter(|dir_entry| {
                dir_entry.as_ref().unwrap().path().ends_with(".jack")
            })
            .map(|dir_entry| {
                dir_entry.unwrap().path().to_str().unwrap().to_owned()
            })
            .collect();
    } else {
        if path.raw_path.ends_with(".jack") {
            jack_files.push(path.raw_path);
        } else {
            return Err("File provided has incorrect file type.");
        }
    }

    Ok(jack_files)
}

fn generate_xml() {}
