
use std::collections::HashSet;
use regex;


#[derive(Debug)]
pub struct Token {
    pub type_: String,
    pub string_: String,
    pub line: usize,
    pub col: usize
}


pub fn tokenise(data: &String) -> Vec<Token> {

    let name_regex = regex::Regex::new(r"^[a-zA-Z_][a-zA-Z_0-9\.]*").unwrap();
    //let return_regex = regex::Regex::new(r"^\n").unwrap();
    let ignore_regex = regex::Regex::new(r"^([$][^$]*[$])|([ \t\r\n\f\v]+)").unwrap();
    let number_regex = regex::Regex::new(r"^\d+(/\d+)?").unwrap();
    /*let mut keywords = HashSet::new();
    keywords.insert("import");
    keywords.insert("as");
    keywords.insert("global");
    keywords.insert("let");
    keywords.insert("unlet");
    keywords.insert("func");
    keywords.insert("return");
    keywords.insert("println");
    keywords.insert("print");
    keywords.insert("if");
    keywords.insert("fi");
    keywords.insert("else");
    keywords.insert("loop");
    keywords.insert("pool");
    keywords.insert("for");
    keywords.insert("rof");
    keywords.insert("call");
    keywords.insert("uncall");
    keywords.insert("do");
    keywords.insert("undo");
    keywords.insert("yield");
    keywords.insert("swap");
    keywords.insert("push");
    keywords.insert("pop");
    keywords.insert("try");
    keywords.insert("catch");
    keywords.insert("yrt");
    keywords.insert("promote");
    keywords.insert("in");
    keywords.insert("to");
    keywords.insert("by");
    keywords.insert("tensor");
    keywords.insert("barrier");
    keywords.insert("mutex");
    keywords.insert("xetum");
    keywords.insert("TID");*/

    let mut ret = Vec::new();
    let mut pos = 0;
    let mut line = 1;
    let mut col = 0;
    //let mut skip_newline = true;
    while pos < data.len() {
        
        match name_regex.find(&data[pos..]) {
            Some(m) => {
                let string_ = &data[pos .. pos + m.end()];
                ret.push(Token{
                    type_: String::from("NAME"), 
                    string_: String::from(string_),
                    line, col
                });
                pos += m.end();
                col += m.end();
                continue;
            }
            None => ()
        };
        
        match number_regex.find(&data[pos..]) {
            Some(m) => {
                ret.push(Token{
                    type_: String::from("NUMBER"), 
                    string_: String::from(&data[pos .. pos + m.end()]),
                    line, col
                });
                pos += m.end();
                col += m.end();
                continue;
            }
            None => ()
        };

        match ignore_regex.find(&data[pos..]) {
            Some(m) => {
                pos += m.end();
                let mut matches: Vec<(usize, &str)> = 
                    data[m.start()..m.end()].match_indices(r"\n").collect();
                line += matches.len();
                if matches.len() > 0 {
                    let lastpos = matches.pop().unwrap().0;
                    col = m.end() - lastpos;
                } else {
                    col += m.end();
                }
                continue;
            }
            None => ()
        };

        panic!("Unhandled input characters")
    }

    ret
}