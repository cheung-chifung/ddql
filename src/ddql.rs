use nom::types::CompleteByteSlice;
use select;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DdqlStatement {
    Select(select::SelectStatement),
}

named!(pub ddql<CompleteByteSlice, DdqlStatement>,
    alt!(
        map!(select::select_statement, |s| DdqlStatement::Select(s))
    )
);

impl fmt::Display for DdqlStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DdqlStatement::Select(s) => write!(f, "{}", s),
        }
    }
}
