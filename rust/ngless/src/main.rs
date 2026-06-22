use std::process::exit;

fn main() {
    let code = ngless::run(std::env::args().skip(1));
    exit(code);
}
