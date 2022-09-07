use crate::{
    error::{ErrorType, JackError},
    lexer::Lexer,
    parser,
};
use std::{env, fs};

pub const DEBUG: bool = true;

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
    pub line: u64,
    pub column: u16,
}

impl FileData {
    fn new(file_name: String, path: String, file_contents: String) -> Self {
        Self {
            file_name,
            path,
            file_contents,
            line: 1,
            column: 1,
        }
    }
}

pub fn analyzer_main() -> Result<(), JackError> {
    let jack_files = get_jack_files()?;

    let parse_tree = generate_xml(&jack_files)?;

    println!("{parse_tree}");

    Ok(())
}

fn generate_xml(jack_files: &Vec<String>) -> Result<String, JackError> {
    let mut parse_tree = String::new();

    for jack_file in jack_files {
        parse_tree.push_str("<tokens>\n");

        let file_contents =
            fs::read_to_string(jack_file).unwrap_or_else(|_| {
                panic!("{}", format!("Unable to open file \"{}\".", jack_file))
            });

        let path = std::path::Path::new(&jack_file);
        let file_name = path.file_name().unwrap().to_str().unwrap();

        if DEBUG {
            eprintln!("File path: {:?}", path);
            eprintln!("File name: {}", file_name);
        }

        let file_data = FileData::new(
            path.to_str().unwrap().to_owned(),
            file_name.to_string(),
            file_contents,
        );

        let mut lexer = Lexer::new(file_data);
        let tokens = lexer.lex()?;

        // Parser::parse() returns a an XML parse tree for that specific stream
        // of tokens.
        let current_parse_tree = parser::parse(tokens)?;
        parse_tree.push_str(&current_parse_tree);
        parse_tree.push_str("</tokens>");

        let mut ppath = match path.parent() {
            Some(ppath) => ppath.to_str().unwrap(),
            None => ".",
        };

        if ppath.is_empty() {
            ppath = ".";
        }

        if DEBUG {
            ppath = "/home/hutton/fun/jack_compiler/testjack/";
        }

        if DEBUG {
            eprintln!(
                "Built path: {}",
                &format!(
                    "{}/{}.xml",
                    ppath,
                    file_name.trim_end_matches(".jack"),
                )
            )
        }

        fs::write(
            std::path::Path::new(&format!(
                "{}/{}.xml",
                ppath,
                file_name.trim_end_matches(".jack")
            )),
            parse_tree.clone(),
        )
        .expect("Failed to write to output file");

        parse_tree.clear();
    }

    Ok(parse_tree)
}

fn get_jack_files() -> Result<Vec<String>, JackError> {
    let mut args = env::args();
    args.next();

    let path = match args.next() {
        Some(path) => path.trim().to_string(),
        None => {
            return Err(JackError::new(
                ErrorType::IOError,
                "Provide a .jack file or directory of .jack files.",
                None,
                None,
                None,
            ))
        }
    };

    if fs::metadata(&path).is_err() {
        return Err(JackError::new(
            ErrorType::IOError,
            "File does not exist.",
            Some(path),
            None,
            None,
        ));
    }

    let path = Path::new(path);

    let mut jack_files: Vec<String> = Vec::new();

    if path.is_dir {
        jack_files = fs::read_dir(path.raw_path)
            .unwrap()
            .filter(|dir_entry| {
                dir_entry.as_ref().unwrap().path().extension().unwrap()
                    == "jack"
            })
            .map(|dir_entry| {
                dir_entry.unwrap().path().to_str().unwrap().to_owned()
            })
            .collect();
    } else if path.raw_path.ends_with(".jack") {
        jack_files.push(path.raw_path);
    } else {
        return Err(JackError::new(
            ErrorType::IOError,
            "File provided has incorrect file type.",
            Some(path.raw_path),
            None,
            None,
        ));
    }

    Ok(jack_files)
}
