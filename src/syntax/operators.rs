use super::ast::*;

use phf::phf_map;
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Fixity {
    None,
    Prefix,
    Infix(Assoc),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Assoc {
    // Right,
    Left,
}
impl Assoc {
    pub fn is_left(&self) -> bool {
        self == &Assoc::Left
    }
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operator<'a> {
    pub sym: &'a str,
    pub fixity: Fixity,
    pub prec: u8,
}
impl<'a> Operator<'a> {
    pub fn has_bigger_prec(&self, op: &Operator<'_>) -> bool {
        self.prec > op.prec || (self.prec == op.prec && op.is_left_assoc())
    }

    pub fn is_infix(&self) -> bool {
        self.fixity != Fixity::Prefix
    }

    pub fn is_prefix(&self) -> bool {
        self.fixity == Fixity::Prefix
    }

    pub fn is_left_assoc(&self) -> bool {
        match self.fixity {
            Fixity::Infix(assoc) => assoc.is_left(),
            _ => false,
        }
    }
}
impl<'a> TryFrom<Operator<'a>> for BinOp {
    type Error = ();
    fn try_from(op: Operator<'a>) -> Result<BinOp, ()> {
        Ok(match op.sym {
            "+" => match op.fixity {
                Fixity::Prefix | Fixity::None => return Err(()),
                Fixity::Infix(_) => BinOp::Add,
            },
            "-" => match op.fixity {
                Fixity::Prefix | Fixity::None => return Err(()),
                Fixity::Infix(_) => BinOp::Sub,
            },
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "&&" => BinOp::And,
            "||" => BinOp::Or,
            ">=" => BinOp::GTE,
            ">" => BinOp::GT,
            "<" => BinOp::LT,
            "<=" => BinOp::LTE,
            "==" => BinOp::EqEq,
            "!=" => BinOp::NotEq,
            _ => unreachable!(),
        })
    }
}
impl<'a> TryFrom<Operator<'a>> for UnOp {
    type Error = ();
    fn try_from(op: Operator<'a>) -> Result<UnOp, ()> {
        Ok(match op.sym {
            "+" => match op.fixity {
                Fixity::Prefix => UnOp::Pos,
                Fixity::Infix(_) | Fixity::None => return Err(()),
            },
            "-" => match op.fixity {
                Fixity::Prefix => UnOp::Neg,
                Fixity::Infix(_) | Fixity::None => return Err(()),
            },
            "!" => UnOp::Neg,
            _ => return Err(()),
        })
    }
}

pub const BINARY_OPERATOR_TABLE: phf::Map<&'static str, Operator> = phf_map! {
    "+"  => Operator {
        prec: 10,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "+",
    },
    "-" => Operator {
        prec: 10,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "-",
    },
    "/" => Operator {
        prec: 20,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "/",
    },
    "*" => Operator {
        prec: 20,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "*",
    },
    "==" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "==",
    },
    "!=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "!=",
    },
    ">" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: ">",
    },
    "<" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "<",
    },
    ">=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: ">=",
    },
    "<=" => Operator {
        prec: 8,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "<=",
    },
    "&&" => Operator {
        prec: 7,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "&&",
    },
    "||" => Operator {
        prec: 6,
        fixity: Fixity::Infix(Assoc::Left),
        sym: "||",
    },
};

pub const UNARY_OPERATOR_TABLE: phf::Map<&'static str, Operator> = phf_map! {
    "+" => Operator {
        prec: 25,
        fixity: Fixity::Prefix,
        sym: "+",
    },
    "-" => Operator {
        prec: 25,
        fixity: Fixity::Prefix,
        sym: "-",
    },
    "!" => Operator {
        prec: 30,
        fixity: Fixity::Prefix,
        sym: "!",
    },
};
