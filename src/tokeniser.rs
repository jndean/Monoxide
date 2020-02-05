
use regex;

#[derive(Debug)]
pub struct Token {
    type_: String,
    string_: String,
    line: usize,
    col: usize
}


pub fn tokenise(data: &String) -> Vec<Token> {

    let name_regex = regex::Regex::new(r"^[a-zA-Z_][a-zA-Z_0-9\.]+").unwrap();
    let return_regex = regex::Regex::new(r"^\n").unwrap();
    let ignore_regex = regex::Regex::new(r"([$][^$]*[$])|([ \t\r\f\v]+)").unwrap();

    let mut ret = Vec::new();
    let mut pos = 0;
    let mut line = 1;
    let mut col = 0;
    while pos < data.len() {
        match return_regex.find(&data[pos..]) {
            Some(m) => {
                ret.push(Token{
                    type_: String::from("NEWLINE"), 
                    string_: String::from(&data[pos .. pos + m.end()]),
                    line, col
                });
                pos += m.end();
                line += 1;
                col = 0;
                continue;
            }
            None => ()
        };
        
        match name_regex.find(&data[pos..]) {
            Some(m) => {
                ret.push(Token{
                    type_: String::from("NAME"), 
                    string_: String::from(&data[pos .. pos + m.end()]),
                    line, col
                });
                pos += m.end();
                col += m.end();
                continue;
            }
            None => {
                println!("{}: No match", pos);
                pos += 1;
            }
        };

        match ignore_regex.find(&data[pos..]) {
            Some(m) => {
                pos += m.end();
                let newlines = data[m.start()..m.end()].matches("\n").count();
                line += newlines;
                if newlines > 0 {
                    //data[m.start()..m.end()].match_indices(r"\n").count()
                }
                col += m.end();
                continue;
            }
            None => {
                println!("{}: No match", pos);
                pos += 1;
            }
        };
    }

    ret
}