#[macro_use]
extern crate nom;
#[macro_use]
extern crate prettytable;

extern crate clap;
extern crate base64;
extern crate lazy_static;

extern crate rusoto_core;
extern crate rusoto_dynamodb;

mod common;
mod select;
mod schema;
mod parser;
mod executor;
mod ddql;

use clap::{Arg, App, SubCommand};
use nom::types::CompleteByteSlice;

use rusoto_core::Region;
use rusoto_dynamodb::{DynamoDb, DynamoDbClient, ListTablesInput};

fn main() {
    let matches = App::new("DDQL")
        .version("0.1.0")
        .author("Chifung Cheung <chifung.cheung@gmail.com>")
        .about("A SQL-like DynamoDB tool")
        .subcommand(SubCommand::with_name("exec")
            .about("execute a ddql command")
            .arg(Arg::with_name("command").help("ddql command").required(true))
            .arg(Arg::with_name("region").help("aws region")))
        .get_matches();

    match matches.subcommand() {
        ("exec", Some(m)) => {
            if let Some(v) = m.value_of("command") {
                let region = m.value_of("region")
                    .and_then(|rn| rn.parse::<Region>().ok())
                    .unwrap_or(Region::default());

                let q = ddql::parse(CompleteByteSlice(v.as_bytes())).unwrap().1;
                println!("query: {}", &q);

                let client = DynamoDbClient::simple(region.clone());
                let exec = executor::Executor::new(DynamoDbClient::simple(region.clone()));
                let res = exec.execute(q);
                match res {
                    Ok(qr) => {
                        qr.print_table();
                    }
                    Err(err) => {
                        println!("{:?}", err);
                    }
                }
                // let list_tables_input: ListTablesInput = Default::default();

                // match client.list_tables(&list_tables_input).sync() {
                //     Ok(output) => {
                //         match output.table_names {
                //             Some(table_name_list) => {
                //                 exec.execute_query(Query::None);
                //                 println!("Tables in database: {}", region);

                //                 for table_name in table_name_list {
                //                     println!("{}", table_name);
                //                 }
                //             },
                //             None => println!("No tables in database!"),
                //         }
                //     },
                //     Err(error) => {
                //         println!("Error: {:?}", error);
                //     },
                // }
            }
        }
        _ => println!("Hello, world!"),
    }
}
