

struct Token {
    type_: String,
    string_: String,
    line: usize,
    col: usize
}

fn tokenise(data: &str) -> [Token] {

    let bytes = data.as_bytes();
    let mut pos = 0;
    while pos < bytes.len() {
        print!("{}", bytes[pos]);
        pos += 1
    }

    Vec::new()
}