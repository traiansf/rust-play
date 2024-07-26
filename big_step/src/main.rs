use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::rc::Rc;

type Store = BTreeMap<String,i128>;


#[derive(Debug)]
enum AExp {
    Nu(i128),
    Id(String),
    PlusE(Rc<AExp>, Rc<AExp>),
    MinusE(Rc<AExp>, Rc<AExp>),
    TimesE(Rc<AExp>, Rc<AExp>),
    DivE(Rc<AExp>, Rc<AExp>),
}

impl AExp {
    fn plus_e(a1: AExp, a2: AExp) -> AExp {
        PlusE(Rc::new(a1), Rc::new(a2))
    }

    fn id(s : &str) -> AExp {
        Id(s.to_string())
    }

    fn eval(&self, store : &Store) -> i128 {
        match self {
            Nu(i) => *i,
            Id(x) => *store.get(x).expect("variable {x} not found"),
            PlusE(e1, e2) => e1.eval(store) + e2.eval(store),
            TimesE(e1, e2) => e1.eval(store) * e2.eval(store),
            _ => todo!(),
        }
    }

    fn term_from_str(s: &str) -> (AExp, &str) {
        let (mut t, mut rest) = AExp::factor_from_str(s);
        while !rest.is_empty() {
            let op = rest.chars().next().unwrap();
            if !(op == '*' || op == '/') {
                return (t, rest)
            }
            let (t2, rest1) = AExp::factor_from_str(rest[1..].trim_start());
            t = AExp::mk_bin(t, op, t2);
            rest = rest1;
        }
        (t, "")
    }

    fn exp_from_str(s: &str) -> (AExp, &str) {
        let (mut t, mut rest) = AExp::term_from_str(s);
        while !rest.is_empty() {
            let op = rest.chars().next().unwrap();
            if !(op == '+' || op == '-') {
                return (t, rest)
            }
            let (t2, rest1) = AExp::term_from_str(rest[1..].trim_start());
            t = AExp::mk_bin(t, op, t2);
            rest = rest1;
        }
        (t, "")
    }

    fn from_str(s: &str) -> AExp {
        let (t, rest) = AExp::exp_from_str(s.trim());
        assert!(rest.is_empty());
        t
    }

    fn mk_bin(t1: AExp, op: char, t2: AExp) -> AExp {
        match op {
            '+' => PlusE(Rc::new(t1), Rc::new(t2)),
            '-' => MinusE(Rc::new(t1), Rc::new(t2)),
            '*' => TimesE(Rc::new(t1), Rc::new(t2)),
            '/' => DivE(Rc::new(t1), Rc::new(t2)),
            _ => todo!()
        }
    }

    fn factor_from_str(s: &str) -> (AExp, &str) {
        let fst = s.chars().next().unwrap();
        match fst {
            '(' => {
                let (t, rest) = AExp::exp_from_str(&s[1..].trim_start());
                assert_eq!(rest.chars().next().unwrap(), ')');
                (t, rest[1..].trim_start())
            }
            '0'..='9' => {
                let (n, rest) = AExp::nat_from_str(s);
                (Nu(n), rest)
            }
            'a'..='z' | 'A'..='Z' => {
                let (id, rest) = AExp::id_from_str(s);
                (Id(id.to_string()), rest)
            }
            _ => todo!()
        }
    }

    fn nat_from_str(s: &str) -> (i128, &str) {
        for (idx, c) in s.chars().enumerate() {
            if !c.is_ascii_digit() {
                return (s[..idx].parse().unwrap(), s[idx..].trim_start());
            }
        }
        (s.parse().unwrap(), "")
    }

    fn id_from_str(s: &str) -> (&str, &str) {
        for (idx, c) in s[1..].chars().enumerate() {
            if !c.is_ascii_alphanumeric() {
                return (&s[..idx+1], s[idx+1..].trim_start());
            }
        }
        (s, "")
    }
}


#[derive(Debug)]
enum BExp {
    Bo(bool),
    LE(Rc<AExp>, Rc<AExp>),
    EqE(Rc<AExp>, Rc<AExp>),
    NotE(Rc<BExp>),
    AndE(Rc<BExp>, Rc<BExp>),
}

impl BExp {
    fn le(a1: AExp, a2: AExp) -> BExp {
        LE(Rc::new(a1), Rc::new(a2))
    }

    fn eval(&self, store : &Store) -> bool {
        match self {
            Bo(b) => *b,
            LE(a1, a2) => a1.eval(store) <= a2.eval(store),
            EqE(a1, a2) => a1.eval(store) == a2.eval(store),
            NotE(b) => ! b.eval(store),
            AndE(b1, b2) => b1.eval(store) && b2.eval(store),
        }
    }
}

#[derive(Debug)]
enum Stmt {
    Skip,
    AtrE(String, Rc<AExp>),
    Seq(Rc<Stmt>, Rc<Stmt>),
    IfE(Rc<BExp>, Rc<Stmt>, Rc<Stmt>),
    WhileE(Rc<BExp>, Rc<Stmt>),
}

impl Stmt {
    fn atr_e(x: &str, a: AExp) -> Stmt {
        AtrE(x.to_string(), Rc::new(a))
    }

    fn seq (s1 : Stmt , s2: Stmt ) -> Stmt {
        Seq(Rc::new(s1), Rc::new(s2))
    }

    fn while_e(b : BExp , s2: Stmt ) -> Stmt {
        WhileE(Rc::new(b), Rc::new(s2))
    }

    fn if_e(b : BExp, s1 : Stmt, s2 : Stmt) -> Stmt {
        IfE(Rc::new(b), Rc::new(s1), Rc::new(s2))
    }

    fn if_then(b : BExp, s : Stmt) -> Stmt {
        Stmt::if_e(b, s, Skip)
    }

    fn big_step(&self, mut store : Store) -> Store {
        match self {
            Skip => store,
            AtrE(x, e) => {store.insert(x.clone(), e.eval(&store)); store},
            Seq(s1, s2) => s2.big_step(s1.big_step(store)),
            IfE(b, s1, s2) =>
                if b.eval(&store) {
                    s1.big_step(store)
                } else {
                    s2.big_step(store)
                },
            w @ WhileE(b, s) =>
                if b.eval(&store) {
                    w.big_step(s.big_step(store))

                } else {
                    store
                },
        }
    }
}

#[derive(Debug)]
struct Config {
    stmt : Rc<Stmt>,
    store : Store,
}

impl Clone for Config{
    fn clone(&self) -> Self {
        Self { stmt: self.stmt.clone(), store: self.store.clone() }
    }
}

impl Config {
    fn one_step(self) -> Option<Config> {
        let Config {stmt, mut store} = self;
        match stmt.borrow() {
            AtrE(x, e) =>
                { store.insert(x.clone(), e.eval(&store)); Some(Config {stmt: Rc::new(Skip), store}) }
            Seq(s1, s2) =>
                if let Skip = s1.borrow() {
                    Some(Config {stmt: s2.clone(), store})
                } else {
                    if let Some(cfg1) = (Config {stmt: s1.clone(), store }).one_step() {
                        Some(Config{ stmt: Rc::new(Seq(cfg1.stmt, s2.clone())), store: cfg1.store})
                    } else { None }
                }
            IfE(b, s1, s2) =>
                if b.eval(&store) {
                    Some (Config{ stmt: s1.clone(), store })
                } else {
                    Some (Config{ stmt: s2.clone(), store })
                },
            WhileE(b, s) =>
                Some (Config { stmt: Rc::new(IfE(b.clone(),
                                    Rc::new(Seq(s.clone(), Rc::new(WhileE(b.clone(), s.clone())))),
                                    Rc::new(Skip))), store }),
            _ => None,
        }
    }

    fn small_step(self) -> Config {
        let mut cfg = self;
        while let Some(cfg1) = cfg.clone().one_step() {
            cfg = cfg1;
        }
        cfg
    }

    fn trace(self) -> Vec<Config> {
        let mut output = Vec::new();
        let mut cfg = self;
        output.push(cfg.clone());
        while let Some(cfg1) = cfg.clone().one_step() {
            cfg = cfg1;
            output.push(cfg.clone());
        }
        output
    }

}

use crate::AExp::*;
use crate::BExp::*;
use crate::Stmt::*;

fn main() {
    println!("{:#?}", AExp::from_str(" x  -  3  -  y  -  2  "));
    let sum_no : Stmt =
        Stmt::seq(Stmt::atr_e("s", Nu(0)),
        Stmt::seq(Stmt::atr_e("i", Nu(0)),
        Stmt::while_e(BExp::le(AExp::id("i"), AExp::id("n")),
            Stmt::seq(Stmt::atr_e("s", AExp::plus_e(AExp::id("s"), AExp::id("i"))),
                 Stmt::atr_e("i", AExp::plus_e(AExp::id("i"), Nu(1)))))));
    // let binding = Stmt::if_then(&EqE(&TimesE(&Id("i"), &Id("i")), &Id("n")),
    //             &AtrE("p", &Nu(1)));
    // let binding = Seq(&binding,
    //         &AtrE("i", &PlusE(&Id("i"), &Nu(1))));
    // let is_square : Stmt =
    //     WhileE(&LE(&TimesE(&Id("i"), &Id("i")), &Id("n")),
    //     &binding
    //     );
    let mut store_no = Store::new();
    store_no.insert("n".to_string(), 3);
    let store_no1 = store_no.clone();
    println!("{:#?}", sum_no.big_step(store_no));
    let cfg_final = Config {stmt: Rc::new(sum_no), store: store_no1}.small_step();
    println!("{:#?}", cfg_final);
    // let mut store_sqr = Store::new();
    // store_sqr.insert("n", 35);
    // store_sqr.insert("i", 5);
    // store_sqr.insert("p", 0);
    // println!("{:#?}", is_square.big_step(store_sqr));
}



#[cfg(test)]
mod tests {
    use crate::AExp;

    #[test]
    fn parse_nat_1() {
        let input = "123Ana";
        let (n, rest) = AExp::nat_from_str(input);
        assert_eq!(n, 123);
        assert_eq!(rest, "Ana");
    }

    #[test]
    fn parse_nat_2() {
        let input = "123   Ana";
        let (n, rest) = AExp::nat_from_str(input);
        assert_eq!(n, 123);
        assert_eq!(rest, "Ana");
    }

}
