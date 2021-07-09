use num_traits::FromPrimitive;
use strum_macros::Display;
use variant_count::VariantCount;

#[derive(PartialEq, PartialOrd, FromPrimitive, Clone, Copy, VariantCount, Display, Debug)]
#[repr(u8)]
pub enum Precedence {
    None,
    Assign,
    Or,
    And,
    Equal,
    Comp,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

#[derive(Clone)]
pub struct Prec {
    pub precedence: Precedence,
    extra_precedence: u32,
}

impl Prec {
    pub fn new(prec: Precedence) -> Prec {
        Prec {
            precedence: prec,
            extra_precedence: 0,
        }
    }

    pub fn less(&self, other: &Prec) -> bool {
        let highest_prec = FromPrimitive::from_usize(Precedence::VARIANT_COUNT - 1).unwrap();

        if self.precedence == highest_prec && other.precedence == highest_prec {
            self.extra_precedence < other.extra_precedence
        } else {
            self.precedence < other.precedence
        }
    }

    pub fn next(mut self) -> Prec {
        let highest_prec = FromPrimitive::from_usize(Precedence::VARIANT_COUNT - 1).unwrap();

        if self.precedence == highest_prec {
            self.extra_precedence += 1;
        }

        let prec = FromPrimitive::from_u8(self.precedence as u8 + 1).unwrap_or(highest_prec);

        Prec {
            precedence: prec,
            extra_precedence: self.extra_precedence,
        }
    }
}
