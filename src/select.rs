use std::str;
use std::fmt;
use common::{literal, attrname, table_name, condition};
use common::{AttrName, Condition};
use nom::types::CompleteByteSlice;

/// select * from my_table;
/// select customer_id, account_id from my_table;
/// select customer_id, account_id, `string` from `from``;
/// select customer_id, account_id, `string` from `from`` where customer_id = "customer_id" and account_id = 1;
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SelectStatement {
    pub from_clause: FromClause,
    pub fields: FieldExpression,
    pub where_clause: Option<WhereClause>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FieldExpression {
    All,
    Fields(Vec<AttrName>),
}

named!(pub field_expr<CompleteByteSlice, FieldExpression>, ws!(alt!(field_all | field_fields)));

named!(pub field_all<CompleteByteSlice, FieldExpression>,
       map!(tag!("*"), |_| FieldExpression::All)
);

named!(pub field_fields<CompleteByteSlice, FieldExpression>,
       map!(
           ws!(separated_list!(tag!(","), attrname)),
           |s| FieldExpression::Fields(s)
       )
);

impl fmt::Display for FieldExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FieldExpression::All => write!(f, "*"),
            FieldExpression::Fields(fs) => {
                write!(f,
                       "{}",
                       fs.iter().map(|x| format!("{}", x)).collect::<Vec<String>>().join(","))
            }
        }
    }
}

#[test]
fn parse_select_fields() {
    let select_fields_cases = vec![(b"customer_id, account_id", "hello")];
    for v in &select_fields_cases {
        println!("{:?}", field_expr(CompleteByteSlice(v.0)));
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FromClause {
    pub table: String,
}

named!(pub from_clause<CompleteByteSlice, FromClause>,
   map!(
       pair!(
           ws!(tag_no_case!("from")),
           ws!(table_name)
       ),
       |(_, t):(_, String)| FromClause {table: t}
   )
);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhereClause {
    pub cond: Condition,
}

named!(pub where_clause<CompleteByteSlice, WhereClause>,
       map!(
           pair!(
               ws!(tag_no_case!("where")),
               ws!(condition)
           ),
           |(_, cond):(_, Condition)| WhereClause {cond}
       )
);

impl fmt::Display for WhereClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "WHERE {}", self.cond)
    }
}

#[test]
fn parse_from_clause() {
    let from_clause_cases: Vec<(&[u8], _)> =
        vec![(b"from `customers`", FromClause { table: "customers".to_string() }),
             (b"from customers", FromClause { table: "customers".to_string() })];
    for c in &from_clause_cases {
        assert_eq!(from_clause(CompleteByteSlice(c.0)).unwrap().1, c.1);
    }
}

impl fmt::Display for FromClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FROM `{}`", self.table)
    }
}

named!(pub select_statement<CompleteByteSlice, SelectStatement>,
    do_parse!(
        ws!(tag_no_case!("select")) >>
        fields: ws!(field_expr) >>
        from_clause: ws!(from_clause) >>
        where_clause: opt!(ws!(where_clause)) >>
        tag!(";") >>
            (SelectStatement{fields, from_clause, where_clause})
    )
);

impl fmt::Display for SelectStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut out = format!("SELECT {} {}", self.fields, self.from_clause).to_string();
        if let Some(ref w) = self.where_clause {
            out.push_str(&format!(" {}", w));
        }
        out.push_str(";");
        write!(f, "{}", out)
    }
}

#[test]
fn parse_select_statement() {
    let select_statement_cases =
        vec![b"select customer_id from customers where customer_id > a and account_id = b;"];
    for v in &select_statement_cases {
        println!("{}", select_statement(CompleteByteSlice(&v[..])).unwrap().1);
    }
}
