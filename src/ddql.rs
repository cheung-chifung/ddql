use nom::types::CompleteByteSlice;
use select;
use schema;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Query {
    Select(select::SelectStatement),
    ShowTables,
    None,
}

named!(pub parse<CompleteByteSlice, Query>,
    alt!(
        map!(schema::show_tables_statement, |s| Query::ShowTables) |
        map!(select::select_statement, |s| Query::Select(s))
    )
);

impl fmt::Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Query::Select(s) => write!(f, "{}", s),
            Query::ShowTables => write!(f, "SHOW TABLES"),
            _ => write!(f, "unknown"),
        }
    }
}
