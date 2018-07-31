use std::str;
use std::fmt;

use select::

#[derive(Clone, Debug, Eq)]
pub enum DDQuery {
    Select(SelectStatement),
}
