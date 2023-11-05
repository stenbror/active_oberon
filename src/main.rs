mod scanner_active_oberon;

extern crate clap;
use clap::{Arg, App};

fn main() {

    let matches = App::new("Active-Oberon compiler (2019 Version) 64 bits native compiler.")
        .version("Build: '2023-11-05', Version: '0.1'")
        .author("Richard Magnor Stenbro. stenbror@hotmail.com")
        .about("A native code compiler for Active Oberon language for use on Linux machines.")
        .arg(Arg::with_name("file")
            .short('f')
            .long("file")
            .takes_value(true)
            .help("main source file 'name'[.mod]"))
        .get_matches();
}
