use std::str;
use std::fmt;

use select::{SelectStatement, select_statement};
use common::{table_name};
use nom::types::CompleteByteSlice;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Query {
    Select(SelectStatement),
    ShowTables,
    None
}

impl fmt::Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self{
            Query::Select(s) => write!(f, "{}", s),
            Query::ShowTables => write!(f, "SHOW TABLES"),
            _ => write!(f, "unknown"),
        }
    }
}

named!(pub query<CompleteByteSlice, Query>,
    ws!(alt!(
        show_tables_statement |
        map!(select_statement, |s| Query::Select(s))
    ))
);

named!(pub show_tables_statement<CompleteByteSlice, Query>,
    do_parse!(
        ws!(tag_no_case!("show")) >>
        ws!(tag_no_case!("tables")) >>
        (Query::ShowTables)
    )
);
