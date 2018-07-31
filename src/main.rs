#![feature(extern_prelude)]

#[macro_use]
extern crate nom;
extern crate clap;
extern crate base64;

mod common;
mod select;
mod ddql;

use clap::{Arg, App, SubCommand};
use nom::types::CompleteByteSlice;

fn main() {
    let exec_args = Arg::with_name("command").help("ddql command").required(true);
    let matches = App::new("DDQL")
        .version("0.1.0")
        .author("Chifung Cheung <chifung.cheung@gmail.com>")
        .about("A SQL-like DynamoDB tool")
        .subcommand(SubCommand::with_name("exec")
            .about("execute a ddql command")
            .arg(exec_args))
        .get_matches();

    match matches.subcommand() {
        ("exec", Some(m)) => {
            if let Some(v) = m.value_of("command") {
                println!("{}", ddql::ddql(CompleteByteSlice(v.as_bytes())).unwrap().1);
            }
        }
        _ => println!("Hello, world!"),
    }
}
