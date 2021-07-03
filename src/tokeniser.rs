
use regex;


#[derive(Debug, Clone)]
pub struct Token {
    pub type_: String,
    pub string_: String,
    pub line: usize,
    pub col: usize
}


pub fn tokenise(data: &String) -> Vec<Token> {

    let name_regex = regex::Regex::new(r"^[a-zA-Z_][a-zA-Z_0-9\.]*").unwrap();
    let number_regex = regex::Regex::new(r"^\d+(/\d+)?").unwrap();
    let string_regex = regex::Regex::new(r"^'[^']*'").unwrap();
    let ignore_regex = regex::Regex::new(r"^(([$][^$]*[$])|([ \t\r\f\v]+))").unwrap();
    let newline_regex = regex::Regex::new(r"^\n").unwrap();
    let symbol_regex = regex::Regex::new(&(String::from(r"^(")
    + r"\+=|\-=|\*=|/="
    + r"|<=|>=|!=|=="
    + r"|~=|=>|//|\*\*"
    + r"|\+|\-|\*|/"
    + r"|=|<|>"
    + r"|\[|\]|\(|\)|\{|\}"
    + r"|;|~|#|,|&|!|%|\||\^|\."
    + r")")).unwrap();

    let mut ret = Vec::new();
    let mut pos = 0;
    let mut line = 1;
    let mut col = 0;
    while pos < data.len() {
        
        if let Some(m) = name_regex.find(&data[pos..]) {
            ret.push(Token{
                type_: String::from("NAME"), 
                string_: String::from(&data[pos .. pos + m.end()]),
                line, col
            });
            pos += m.end();
            col += m.end();
            continue;
        };

        if let Some(m) = symbol_regex.find(&data[pos..]) {
            ret.push(Token{
                type_: String::from("SYMBOL"), 
                string_: String::from(&data[pos .. pos + m.end()]),
                line, col
            });
            pos += m.end();
            col += m.end();
            continue;
        };
        
        if let Some(m) =  number_regex.find(&data[pos..]) {
            ret.push(Token{
                type_: String::from("NUMBER"), 
                string_: String::from(&data[pos .. pos + m.end()]),
                line, col
            });
            pos += m.end();
            col += m.end();
            continue;
        }

        if let Some(m) = ignore_regex.find(&data[pos..]) {
            let newlines:Vec<_> = data[pos .. pos + m.end()].match_indices("\n").collect();
            pos += m.end();
            line += newlines.len();
            if let Some(idx) = newlines.last() {
                col = m.end() - 1 - idx.0;
            } else {
                col += m.end();
            }
            continue;
        }

        if let Some(m) = newline_regex.find(&data[pos..]) {
            pos += m.end();
            line += 1;
            col = 0;
            continue;
        }

        if let Some(m) = string_regex.find(&data[pos..]) {
            ret.push(Token{
                type_: String::from("STRING"), 
                string_: String::from(&data[pos + 1 .. pos + m.end() - 1]),
                line, col
            });
            let newlines:Vec<_> = data[pos .. pos + m.end()].match_indices("\n").collect();
            pos += m.end();
            line += newlines.len();
            if let Some(idx) = newlines.last() {
                col = m.end() - 1 - idx.0;
            } else {
                col += m.end();
            }
            continue;
        }
        
        println!("pos {}", pos);
        panic!("Unhandled input characters")
    }

    ret.push(Token {
        string_: String::from(""),
        type_: String::from("END_MARKER!"),
        line, col
    });
    ret
}