use nom::{digit, alphanumeric};
use nom::types::CompleteByteSlice;
use std::fmt;
use std::str;
use std::collections::{HashMap, HashSet};
use base64::{decode, encode};

//named!(not_space<CompleteByteSlice, CompleteByteSlice>, );

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal {
    Null,
    Boolean(bool),
    Number(String),
    String(String),
    Binary(Vec<u8>),
    Map(HashMap<String, Literal>),
    NumberSet(Vec<Literal>),
    StringSet(Vec<Literal>),
    BinarySet(Vec<Literal>),
    List(Vec<Literal>),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Null => write!(f, "null"),
            Literal::Boolean(b) => write!(f, "{}", (if *b { "true" } else { "false" })),
            Literal::Number(num) => write!(f, "{}", num),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Binary(bin) => write!(f, "#{}", encode(bin)),
            Literal::Map(m) => {
                write!(f, "{{");
                let l = m.len();
                let mut i = 1;
                for (k, v) in m {
                    if i == l {
                        write!(f, "\"{}\": {}", k, v);
                    } else {
                        write!(f, "\"{}\": {},", k, v);
                    }
                    i = i + 1;
                }
                write!(f, "}}")
            }
            Literal::List(l) => {
                write!(f, "[");
                if let Some((last, rest)) = l.as_slice().split_last() {
                    for v in rest {
                        write!(f, "{},", v);
                    }
                    write!(f, "{}", last);
                }
                write!(f, "]")
            }
            Literal::StringSet(s) |
            Literal::NumberSet(s) |
            Literal::BinarySet(s) => {
                write!(f, "{{");
                if let Some((last, rest)) = s.as_slice().split_last() {
                    for v in rest {
                        write!(f, "{},", v);
                    }
                    write!(f, "{}", last);
                }
                write!(f, "}}")
            }
        }
    }
}

named!(number_literal<CompleteByteSlice, Literal>,
       map_res!(
        recognize!(
            do_parse!(
                sign: opt!(alt!(tag!("+") | tag!("-"))) >>
                num: alt!(
                    delimited!(digit, tag!("."), opt!(digit)) |
                    delimited!(opt!(digit), tag!("."), digit) |
                    digit
                ) >>
                (sign, num)
            )
        ),
       |s: CompleteByteSlice| String::from_utf8(s.0.to_vec()).map(|x| Literal::Number(x))
    )
);

named!(pub string_literal<CompleteByteSlice, Literal>,
       map_res!(
       do_parse!(
           val: alt_complete!(
               delimited!(tag!("\""), opt!(take_until!("\"")), tag!("\""))|
               delimited!(tag!("'"), opt!(take_until!("'")), tag!("'"))
           ) >>
               ({val.unwrap_or(CompleteByteSlice(b""))})
       ),
           |s: CompleteByteSlice| String::from_utf8(s.0.to_vec()).map(|x| Literal::String(x))
      )
);

named!(pub null_literal<CompleteByteSlice, Literal>, map!(tag_no_case!("null"), |_| Literal::Null));

named!(pub true_literal<CompleteByteSlice, Literal>, map!(tag_no_case!("true"), |_| Literal::Boolean(true)));

named!(pub false_literal<CompleteByteSlice, Literal>, map!(tag_no_case!("false"), |_| Literal::Boolean(false)));

named!(pub boolean_literal<CompleteByteSlice, Literal>, alt!(true_literal | false_literal));

named!(pub binary_literal<CompleteByteSlice, Literal>,
       map_res!(
       pair!(
           tag!("#"),
           recognize!(pair!(alphanumeric, tag!("=")))
       ),
        |(_, b):(_, CompleteByteSlice)| {
            decode(b.0).map(|x| Literal::Binary(x))
        }
      )
);

named!(pub number_set_literal<CompleteByteSlice, Literal>,
   ws!(map!(delimited!(tag!("{"), separated_list!(tag!(","), number_literal), tag!("}")), |v: Vec<Literal>| (Literal::NumberSet(v))))
);

named!(pub string_set_literal<CompleteByteSlice, Literal>,
       ws!(map!(delimited!(tag!("{"), separated_list!(tag!(","), string_literal), tag!("}")), |v: Vec<Literal>| (Literal::StringSet(v))))
);

named!(pub binary_set_literal<CompleteByteSlice, Literal>,
       ws!(map!(delimited!(tag!("{"), separated_list!(tag!(","), binary_literal), tag!("}")), |v: Vec<Literal>| (Literal::BinarySet(v))))
);

named!(pub list_literal<CompleteByteSlice, Literal>,
       ws!(map!(delimited!(tag!("["), separated_list!(tag!(","), literal), tag!("]")), |v: Vec<Literal>| (Literal::List(v))))
);

named!(pub map_literal_key<CompleteByteSlice, String>,
       ws!(
        map_res!(
            do_parse!(
                val: alt_complete!(
                    delimited!(tag!("\""), opt!(take_until!("\"")), tag!("\""))|
                    delimited!(tag!("'"), opt!(take_until!("'")), tag!("'"))
                ) >>
                    ({val.unwrap_or(CompleteByteSlice(b""))})
            ),
            |s: CompleteByteSlice| String::from_utf8(s.0.to_vec())
        )
      )
);

named!(pub map_literal<CompleteByteSlice, Literal>,
       ws!(map!(
           delimited!(
               tag!("{"),
               separated_list!(tag!(","), ws!(separated_pair!(map_literal_key, tag!(":"), literal))),
               tag!("}")),
           |tuple_vec| Literal::Map(tuple_vec.into_iter().collect())
       ))
);

named!(pub literal<CompleteByteSlice, Literal>,
        ws!(alt!(
            number_literal |
            null_literal |
            string_literal |
            boolean_literal |
            binary_literal |
            string_set_literal |
            number_set_literal |
            binary_set_literal |
            list_literal |
            map_literal
        ))
);

pub enum DDType {
    String,
    Number,
    Binary,
    Boolean,
    Null,
    List,
    Map,
    Set,
}

#[test]
fn parse_literal() {
    let num_cases = vec!["123.12", "123.0", "123.", ".0", "+12", "-12", "+12.0", "-12.0", "+12.0"];
    for v in &num_cases {
        assert_eq!(Literal::Number(v.to_string()),
                   literal(CompleteByteSlice(v.as_bytes())).unwrap().1);
    }

    let fail_num_cases = vec!["abc", "*12"];
    for v in &fail_num_cases {
        assert_eq!(true, literal(CompleteByteSlice(v.as_bytes())).is_err());
    }

    let string_cases =
        vec![("'hello'", "hello"), ("\"hello\"", "hello"), ("''", ""), ("'中文'", "中文")];
    for v in &string_cases {
        assert_eq!(Literal::String(v.1.to_string()),
                   literal(CompleteByteSlice(v.0.as_bytes())).unwrap().1);
    }

    let binary_cases = vec![("#aGVsbG8=", b"hello")];
    for v in &binary_cases {
        assert_eq!(Literal::Binary(v.1.to_vec()),
                   literal(CompleteByteSlice(v.0.as_bytes())).unwrap().1);
    }

    let set_cases = vec![("{'hello', 3.5, TRUE, null}",
                          Literal::Set(vec![Literal::String("hello".to_string()),
                                            Literal::Number("3.5".to_string()),
                                            Literal::Boolean(true),
                                            Literal::Null]))];
    for c in &set_cases {
        let res = literal(CompleteByteSlice(c.0.as_bytes())).unwrap();
        assert_eq!(res.1, c.1);
    }

    let map_cases =
        vec![("{\"num\": 3.5, \"bool\": FalSE, \"null\": null, \"str\": \"string\"}",
              Literal::Map(vec![("num".to_string(), Literal::Number("3.5".to_string())),
                                ("bool".to_string(), Literal::Boolean(false)),
                                ("null".to_string(), Literal::Null),
                                ("str".to_string(), Literal::String("string".to_string()))]
                  .into_iter()
                  .collect()))];
    for c in &map_cases {
        let res = literal(CompleteByteSlice(c.0.as_bytes())).unwrap();
        assert_eq!(res.1, c.1);
        // println!("{:?}", res.1);
    }
    // println!("{:?}", literal(CompleteByteSlice(b"{'aaa', 'bbb', 'ccc'}")));

    assert_eq!(Literal::Null,
               literal(CompleteByteSlice(b"null")).unwrap().1);

    assert_eq!(Literal::Boolean(true),
               literal(CompleteByteSlice(b"true")).unwrap().1);

    assert_eq!(Literal::Boolean(false),
               literal(CompleteByteSlice(b"false")).unwrap().1);
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Path {
    pub paths: Vec<AttrName>,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{}",
               self.paths
                   .iter()
                   .map(|x| format!("{}", x).to_string())
                   .collect::<Vec<String>>()
                   .join("."))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AttrNameKind {
    Normal,
    Reserved,
    Expected,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AttrName {
    pub kind: AttrNameKind,
    pub name: String,
}

impl fmt::Display for AttrName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            AttrNameKind::Normal => write!(f, "{}", self.name),
            _ => write!(f, "`{}`", self.name),
        }
    }
}

fn normal_attr(s: &str) -> AttrName {
    AttrName {
        kind: AttrNameKind::Normal,
        name: s.to_string(),
    }
}

fn reserved_attr(s: &str) -> AttrName {
    AttrName {
        kind: AttrNameKind::Reserved,
        name: s.to_string(),
    }
}

named!(pub backquoted_string<CompleteByteSlice, String>,
   map_res!(
       delimited!(tag!("`"), take_until!("`"), tag!("`")),
       |s: CompleteByteSlice| String::from_utf8(s.0.to_vec())
   )
);

named!(pub non_space_string<CompleteByteSlice, String>,
       map_res!(
           is_not_s!(CompleteByteSlice(b" \t\r\n.,;`\"'[](){}")),
           |s: CompleteByteSlice| String::from_utf8(s.0.to_vec())
       )
);

named!(pub attrname<CompleteByteSlice, AttrName>, alt!(attrname_normal | attrname_reserved));

named!(pub attrname_normal<CompleteByteSlice, AttrName>,
       map!(non_space_string, |x| AttrName {kind: AttrNameKind::Normal, name: x})
);

named!(pub attrname_reserved<CompleteByteSlice, AttrName>,
       map!(backquoted_string, |x| AttrName {kind: AttrNameKind::Reserved, name: x})
);

named!(pub path<CompleteByteSlice, Path>,
       map!(
           ws!(separated_list!(tag!("."), alt!(attrname_reserved | attrname_normal))),
           |s| Path {paths: s}
       )
);

#[test]
fn parse_path() {
    let path_cases = vec![(b"help.`test`.string", "string")];
    for v in &path_cases {
        println!("{:?}", path(CompleteByteSlice(v.0)));
        println!("{}",
                 Path {
                     paths: vec![normal_attr("help"), reserved_attr("test"), normal_attr("string")],
                 });
    }
}

named!(pub table_name<CompleteByteSlice, String>, alt!(non_space_string | backquoted_string));

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Condition {
    Comparation {
        op: ConditionComparator,
        left: Operand,
        right: Operand,
    },
    Between {
        opd: Operand,
        lower: Operand,
        upper: Operand,
    },
    In { opd: Operand, list: Vec<Operand> },
    And {
        left: Box<Condition>,
        right: Box<Condition>,
    },
    Or {
        left: Box<Condition>,
        right: Box<Condition>,
    },
    Parenthesis { cond: Box<Condition> },
    Not { cond: Box<Condition> },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    Path(Path),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ConditionComparator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqualTo,
    GreaterThan,
    GreaterThanEqualTo,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ConditionLogicalOperator {
    And,
    Or,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Condition::Comparation { op, left, right } => write!(f, "{} {} {}", left, op, right),
            Condition::Between { opd, lower, upper } => {
                write!(f, "{} BETWEEN {} AND {}", opd, lower, upper)
            }
            Condition::In { opd, list } => {
                write!(f,
                       "{} IN ( {} )",
                       opd,
                       list.iter()
                           .map(|x| format!("{}", x).to_string())
                           .collect::<Vec<String>>()
                           .join(","))
            }
            Condition::And { left, right } => write!(f, "({} AND {})", left, right),
            Condition::Or { left, right } => write!(f, "({} OR {})", left, right),
            Condition::Not { cond } => write!(f, "!({})", cond),
            Condition::Parenthesis { cond } => write!(f, "({})", cond),
        }
    }
}

impl fmt::Display for ConditionComparator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConditionComparator::Equal => write!(f, "="),
            ConditionComparator::NotEqual => write!(f, "<>"),
            ConditionComparator::LessThan => write!(f, "<"),
            ConditionComparator::LessThanEqualTo => write!(f, "<="),
            ConditionComparator::GreaterThan => write!(f, ">"),
            ConditionComparator::GreaterThanEqualTo => write!(f, ">="),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Path(p) => write!(f, "{}", p),
        }
    }
}

named!(pub operand<CompleteByteSlice, Operand>,
       ws!(alt!(operand_path))
);

named!(pub operand_path<CompleteByteSlice, Operand>,
       map!(path, |p| Operand::Path(p))
);

//  A and B -> A and B or C / A and B and C
//  A or B -> A or B or C / A or B and C

named!(pub condition<CompleteByteSlice, Condition>,
       do_parse!(
           init: condition_single >>
           rest: fold_many0!(
               tuple!(
                   logical_operator,
                   condition_single
               ),
               init,
               |left: Condition, (op, right) : (ConditionLogicalOperator, Condition)|{
                   // println!("fuck {:?}, {:?}, {:?}", left, op, right);
                   match (&left, op) {
                       (&Condition::Or{left: ref or_l, right: ref or_r}, ConditionLogicalOperator::And) => {
                           Condition::Or{
                               left: or_l.to_owned(),
                               right: Box::new(Condition::And{
                                   left: or_r.to_owned(),
                                   right: Box::new(right),
                               })
                           }
                       },
                       (_, ConditionLogicalOperator::And) => {
                           Condition::And{left: Box::new(left.to_owned()), right: Box::new(right)}
                       },
                       (_, ConditionLogicalOperator::Or) => {
                           Condition::Or{left: Box::new(left.to_owned()), right: Box::new(right)}
                       }
                   }
               }
               // |left: Condition, (op, right) : (ConditionLogicalOperator, Condition)|{
               //     let k = &left;
               //     println!("{:?}", k);
               //     match k {
               //         Condition::Or{left: or_l, right: or_r} => Condition::And {
               //             left: Box::new(k.to_owned()), right: or_r.to_owned(),
               //             },
               //         _ => Condition::Comparation {
               //                 op: ConditionComparator::GreaterThan,
               //                 left: Operand::Path(Path { paths: vec![normal_attr("a"), normal_attr("b")] }),
               //                 right: Operand::Path(Path { paths: vec![normal_attr("c")] }),
               //             }
               //     }
               // }
           ) >>
           (rest)
       )
);

named!(pub condition_single<CompleteByteSlice, Condition>,
    ws!(alt!(
        condition_parenthesis |
        condition_not |
        condition_comparation | condition_between | condition_in
    ))
);

named!(pub condition_box<CompleteByteSlice, Box<Condition>>,
       map!(condition, |c| Box::new(c))
);

named!(pub comparator<CompleteByteSlice, ConditionComparator>,
       alt!(
           map!(tag_no_case!("="), |_| ConditionComparator::Equal) |
           map!(tag_no_case!("<>"), |_| ConditionComparator::NotEqual) |
           map!(tag_no_case!("<="), |_| ConditionComparator::LessThanEqualTo) |
           map!(tag_no_case!(">="), |_| ConditionComparator::GreaterThanEqualTo) |
           map!(tag_no_case!("<"), |_| ConditionComparator::LessThan) |
           map!(tag_no_case!(">"), |_| ConditionComparator::GreaterThan)
       )
);

named!(pub logical_operator<CompleteByteSlice, ConditionLogicalOperator>,
       alt!(
           map!(alt!(tag_no_case!("and") | tag!("&&")), |_| ConditionLogicalOperator::And) |
           map!(alt!(tag_no_case!("or") | tag!("||")), |_| ConditionLogicalOperator::Or)
       )
);

named!(pub condition_comparation<CompleteByteSlice, Condition>,
       do_parse!(
           left: operand >>
           op: comparator >>
           right: operand >>
           ({Condition::Comparation{left: left, op: op, right: right}})
       )
);


named!(pub condition_between<CompleteByteSlice, Condition>,
       do_parse!(
           opd: operand >>
            ws!(tag_no_case!("between")) >>
            lower: operand >>
            ws!(tag_no_case!("and")) >>
            upper: operand >>
            ({Condition::Between{upper: upper, opd: opd, lower: lower}})
       )
);

named!(pub condition_in<CompleteByteSlice, Condition>,
       do_parse!(
           opd: operand >>
               ws!(tag_no_case!("in")) >>
               list: separated_list!(tag!(","), operand) >>
               ({Condition::In{opd: opd, list: list}})
       )
);

named!(pub condition_and<CompleteByteSlice, Condition>,
       do_parse!(
           left: ws!(condition) >>
           alt!(tag_no_case!("and") | tag!("&&")) >>
           right: ws!(condition) >>
               ({Condition::And{left: Box::new(left), right: Box::new(right)}})
       )
);

named!(pub condition_or<CompleteByteSlice, Condition>,
       do_parse!(
           left: ws!(condition) >>
               alt!(tag_no_case!("or") | tag!("||")) >>
               right: ws!(condition) >>
               ({Condition::Or{left: Box::new(left), right: Box::new(right)}})
       )
);

named!(pub condition_not<CompleteByteSlice, Condition>,
       do_parse!(
           alt!(tag_no_case!("not") | tag!("!")) >>
           cond: condition >>
           ({Condition::Not{cond: Box::new(cond)}})
       )
);

named!(pub condition_parenthesis<CompleteByteSlice, Condition>,
       do_parse!(
           cond: delimited!(tag!("("), condition, tag!(")")) >>
           ({Condition::Parenthesis{cond: Box::new(cond)}})
       )
);

#[test]
fn parse_condition() {
    let operand_cases: Vec<(&[u8], _)> =
        vec![(b" a ", Operand::Path(Path { paths: vec![normal_attr("a")] }))];
    for c in &operand_cases {
        assert_eq!(operand(CompleteByteSlice(c.0)).unwrap().1, c.1);
    }

    assert_eq!(condition_comparation(CompleteByteSlice(b"a.b > c")).unwrap().1,
               Condition::Comparation {
                   op: ConditionComparator::GreaterThan,
                   left: Operand::Path(Path { paths: vec![normal_attr("a"), normal_attr("b")] }),
                   right: Operand::Path(Path { paths: vec![normal_attr("c")] }),
               });

    let condition_cases = vec![(b"(g = k) && g between a and b or !(d < k) and `k`.d > a566", "")];
    for v in &condition_cases {
        println!("{}", condition(CompleteByteSlice(v.0)).unwrap().1);
    }
}
