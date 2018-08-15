use std::str;
use std::fmt;
use nom::types::CompleteByteSlice;

use common::table_name;

named!(pub show_tables_statement<CompleteByteSlice, ()>,
       do_parse!(
           ws!(tag_no_case!("show")) >>
               ws!(tag_no_case!("tables")) >>
               ()
       )
);
