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

pub fn analyzer_main() -> Result<(), &'static str> {
    let jack_files = get_jack_files().expect("Failed to gather .jack files");

    let parse_tree = match generate_xml(&jack_files) {
        Ok(parse_tree) => parse_tree,
        Err(e) => return Err(e),
    };

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

        let tokens = lexer::lex(file_contents)?;

        // Parser::parse() returns a an XML parse tree for that specific stream
        // of tokens.
        let current_parse_tree = parser::parse(tokens)?;
        parse_tree.push_str(&current_parse_tree);
    }

    parse_tree.push_str("</tokens>");

    Ok(parse_tree)
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
