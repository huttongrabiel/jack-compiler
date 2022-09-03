use crate::{lexer, parser};
use std::{env, fs};

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

#[derive(Debug)]
pub struct FileData {
    pub file_name: String,
    pub path: String,
    pub file_contents: String,
}

impl FileData {
    fn new(file_name: String, path: String, file_contents: String) -> Self {
        Self {
            file_name,
            path,
            file_contents,
        }
    }
}

pub fn analyzer_main() -> Result<(), &'static str> {
    let jack_files = get_jack_files()?;

    let parse_tree = generate_xml(&jack_files)?;

    println!("{parse_tree}");

    Ok(())
}

fn generate_xml(jack_files: &Vec<String>) -> Result<String, &'static str> {
    let mut parse_tree = String::new();
    parse_tree.push_str("<tokens>\n");

    for jack_file in jack_files {
        let file_contents = fs::read_to_string(jack_file).expect(
            &format!("Unable to open file \"{}\".", jack_file).to_string(),
        );

        let path = std::path::Path::new(&jack_file);
        let file_name = path.file_name().unwrap().to_str().unwrap();

        let file_data = FileData::new(
            path.to_str().unwrap().to_owned(),
            file_name.to_string(),
            file_contents,
        );

        let tokens = lexer::lex(file_data)?;

        // Parser::parse() returns a an XML parse tree for that specific stream
        // of tokens.
        let current_parse_tree = parser::parse(tokens)?;
        parse_tree.push_str(&current_parse_tree);
    }

    parse_tree.push_str("</tokens>");

    Ok(parse_tree)
}

fn get_jack_files() -> Result<Vec<String>, &'static str> {
    let mut args = env::args().into_iter();
    args.next();

    let path = match args.next() {
        Some(path) => path.trim().to_string(),
        None => {
            return Err("Provide a .jack file or directory of .jack files.")
        }
    };

    if fs::metadata(&path).is_err() {
        // FIXME: Print the filename.
        return Err("File does not exist.");
    }

    let path = Path::new(path);

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
