#![feature(trace_macros)]
// trace_macros!(true);


use ::std::collections::{HashMap, HashSet, *};
use ::std::hash::*;
use ::std::marker::PhantomData;

fn hash(x: &str) -> i64 {
    use hash_map::DefaultHasher;
    let mut s = DefaultHasher::new();
    s.write(x.as_bytes());
    let val = s.finish() as i64 & (-1i64 as u64 >> 1) as i64;
    // println!("{} => {} | {}", x, val, !val);
    val
}

const EPS: i64 = 0i64;
const BOTTOM: i64 = 1i64;

pub type Term = i64;

struct Rule {
    patts: Vec<i64>,
    handler: Box<Fn()>,
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.patts)
    }
}

#[derive(Debug)]
pub struct Param {
    rules: Vec<Rule>,
    item: i64,
}

trait IndexMutOrInsert {
    type Key;
    type Item;

    fn index_mut_or_insert(&mut self, key: Self::Key) -> &mut Self::Item;
}

impl<K, V> IndexMutOrInsert for HashMap<K, HashSet<V>>
where
    K: Copy + Eq + Hash,
    V: Copy + Eq + Hash,
{
    type Key = K;
    type Item = HashSet<V>;

    fn index_mut_or_insert(&mut self, key: Self::Key) -> &mut Self::Item {
        self.entry(key).or_insert(HashSet::<V>::new())
    }
}

fn make_first(first: &mut HashMap<i64, HashSet<i64>>, params: &Vec<Param>) {

    let mut add_sub: bool;
    loop {
        add_sub = false;
        for param in params.iter() {
            // i am curr param
            let my_firsts = first.get(&param.item).unwrap();
            let mut new_firsts = vec![];
            // each rule generated my me
            for rule in param.rules.iter() {
                // this rule generates eps
                let mut has_empty = false;
                // each elem in this rule
                for elem in rule.patts.iter() {
                    has_empty = false;
                    let firsts = first.get(elem).unwrap();
                    // each elem of first[elem]
                    for first_elem in firsts.iter() {
                        if *first_elem == EPS {
                            // this elem generates eps
                            has_empty = true;
                        } else if !my_firsts.contains(first_elem) {
                            new_firsts.push(*first_elem);
                        }
                    }
                    // this elem cant generate eps
                    if !has_empty {
                        break;
                    }
                }
                // this rule generates eps
                if has_empty && !my_firsts.contains(&EPS) {
                    new_firsts.push(EPS);
                }
            }
            // now add new firsts into first[me]
            let my_firsts_mut = first.get_mut(&param.item).unwrap();
            for first_elem in new_firsts.iter() {
                my_firsts_mut.insert(*first_elem);
                add_sub = true;
            }
        }
        // first closure is full
        if !add_sub {
            break;
        }
    }

}

fn make_follow(
    first: &HashMap<i64, HashSet<i64>>,
    follow: &mut HashMap<i64, HashSet<i64>>,
    params: &Vec<Param>,
) {
    let mut add_sub: bool;
    loop {
        add_sub = false;
        for param in params.iter() {
            for rule in param.rules.iter() {
                for (curr_elem, prev_elem) in rule
                    .patts
                    .iter()
                    .skip(1)
                    .rev()
                    .zip(rule.patts.iter().rev().skip(1))
                {
                    if *prev_elem < 0i64 {

                        let mut new_follows = vec![];
                        if *curr_elem < 0i64 {

                            let curr_first = first.get(curr_elem).unwrap();
                            let prev_follow = follow.get(prev_elem).unwrap();

                            // add all first[curr] to follow[prev]
                            for first_elem in curr_first.iter() {
                                if *first_elem != EPS && !prev_follow.contains(first_elem) {
                                    new_follows.push(*first_elem);
                                }
                            }
                            // if this elem yields eps
                            if curr_first.contains(&EPS) {
                                // add all follow[me] to follow[prev]
                                for follow_elem in follow.get(&param.item).unwrap() {
                                    if !prev_follow.contains(&follow_elem) {
                                        new_follows.push(*follow_elem);
                                    }
                                }
                            }

                        } else if !follow.get(prev_elem).unwrap().contains(curr_elem) {
                            new_follows.push(*curr_elem);
                        }

                        // add all new follows to follow[prev]
                        let prev_follow_mut = follow.get_mut(prev_elem).unwrap();
                        for follow in new_follows {
                            prev_follow_mut.insert(follow);
                            add_sub = true;
                        }
                    }
                }

                let back = rule.patts.iter().rev().next().unwrap();
                let back_follow = follow.get(back).unwrap();
                let mut new_follows = vec![];
                if *back < 0i64 {
                    for follow_elem in follow.get(&param.item).unwrap() {
                        if !back_follow.contains(follow_elem) {
                            new_follows.push(*follow_elem);
                        }
                    }
                }
                let back_follow_mut = follow.get_mut(back).unwrap();
                for follow in new_follows {
                    back_follow_mut.insert(follow);
                    add_sub = true;
                }
            }
        }
        // this closure follow is full
        if !add_sub {
            break;
        }
    }

}

pub trait LRLang {
    fn new() -> (Vec<Param>, Vec<Term>);
}

#[derive(Debug)]
enum ActionType {
    Accept,
    MoveIn,
    Reduce,
    Hold,
}

#[derive(Debug)]
struct Action<'a> {
    flag: ActionType,
    rule: Option<&'a Rule>,
}

impl<'a> Action<'a> {
    fn from(ty: ActionType) -> Self {
        Action {
            flag: ty,
            rule: None,
        }
    }
}

#[allow(dead_code)]
pub struct LRParser<'a, T> {
    params: Vec<Param>,
    terms: Vec<Term>,
    first: HashMap<i64, HashSet<i64>>,
    follow: HashMap<i64, HashSet<i64>>,
    parent_of: HashMap<&'a Rule, i64>,
    param_of: HashMap<i64, &'a Param>,
    index_of: HashMap<&'a Rule, usize>,
    closures: Vec<Closure<'a>>,
    action: Vec<HashMap<i64, Action<'a>>>,
    goto: Vec<HashMap<i64, usize>>,
    phantom: PhantomData<T>,
}

impl<'a> PartialEq for &'a Rule {
    fn eq(&self, other: &&'a Rule) -> bool {
        let ptr: *const Rule = *self;
        let ptr_other: *const Rule = *other;
        ptr == ptr_other
    }
}

impl<'a> Eq for &'a Rule {}

impl<'a> Hash for &'a Rule {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Rule = *self;
        ptr.hash(state)
    }
}

#[derive(Debug)]
struct Item<'a> {
    rule: &'a Rule,
    pos: usize,
}

impl<'a> PartialEq for Item<'a> {
    fn eq(&self, other: &Self) -> bool {
        let ptr: *const Rule = self.rule;
        let other_ptr: *const Rule = other.rule;
        ptr == other_ptr && self.pos == other.pos
    }
}

impl<'a> Eq for Item<'a> {}

impl<'a> Hash for Item<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Rule = self.rule;
        ptr.hash(state)
    }
}

type Closure<'a> = HashSet<Item<'a>>;

impl<'a, T: LRLang> LRParser<'a, T> {

    fn find_empty(&self, val: i64) -> Option<&'a Rule> {
        let param = self.param_of.get(&val).unwrap();
        for rule in param.rules.iter() {
            if rule.patts.len() == 1 && rule.patts[0] == EPS {
                return Some(rule);
            } else {
                let mut is_empty = true;
                for elem in rule.patts.iter() {
                    is_empty = self.first.get(&elem).unwrap().contains(&EPS);
                    if !is_empty {
                        break;
                    }
                }
                if is_empty && rule.patts[0] != val {
                    return self.find_empty(rule.patts[0]);
                }
            }
        }
        None
    }

    fn register_sub(&mut self, val: i64, rule: Option<&'a Rule>, state: usize) {
        if let None = self.action[state].get(&val) {
            self.action[state].insert(
                val,
                Action {
                    flag: ActionType::Hold,
                    rule: rule,
                },
            );
            self.goto[state].insert(val, self.closures.len());
            if let Some(param) = self.param_of.get(&val) {
                for rule in param.rules.iter() {
                    for elem in rule.patts.iter() {
                        self.register_sub(*elem, Some(rule), state);
                        if !self.first[elem].contains(&EPS) {
                            break;
                        }
                    }
                }
            }
        }
    }

    fn expand_closure(&mut self, closure: &Closure, state: usize) {

        for item in closure.iter() {
            if item.pos > 0
                && self
                    .first
                    .get(&item.rule.patts[item.pos - 1])
                    .unwrap()
                    .contains(&EPS)
            {
                if item.rule.patts.len() != item.pos {
                    self.register_sub(
                        item.rule.patts[item.pos],
                        self.find_empty(item.rule.patts[item.pos - 1]),
                        state,
                    );
                } else {
                    for i in 0..self.terms.len() {
                        let term = self.terms[i];
                        if let None = self.action[state].get(&term) {
                            let rule = self.find_empty(item.rule.patts[item.pos - 1]);
                            self.action[state].insert(
                                term,
                                Action {
                                    flag: ActionType::Hold,
                                    rule: rule,
                                },
                            );
                            self.goto[state].insert(term, self.closures.len());
                        }
                    }
                }
            }
        }
    }

    fn make_closure(&mut self, mut closure: Closure<'a>, state: usize) {

        self.expand_closure(&closure, state);

        let mut gen_sub: bool;
        loop {
            gen_sub = false;

            let mut items = vec![];
            for item in closure.iter() {
                if item.rule.patts.len() != item.pos {
                    let mut pos = item.pos;
                    loop {
                        if let Some(param) = self.param_of.get(&item.rule.patts[pos]) {
                            for rule in param.rules.iter() {
                                if rule.patts.len() != 1 || rule.patts[0] != EPS {
                                    let item = Item { rule: rule, pos: 0 };
                                    if !closure.contains(&item) {
                                        items.push(item);
                                    }
                                }
                            }
                        }
                        if !self
                            .first
                            .get(&item.rule.patts[pos])
                            .unwrap()
                            .contains(&EPS)
                            || {
                                pos += 1;
                                pos
                            } == item.pos
                        {
                            break;
                        }
                    }
                }
            }
            for item in items.into_iter() {
                closure.insert(item);
                gen_sub = true;
            }

            if !gen_sub {
                break;
            }
        }

        // println!("===>   {:?}", closure);
        self.closures.push(closure);
        self.goto.push(HashMap::new());
        self.action.push(HashMap::new());
    }

    fn init(&mut self) {

        let mut origin: Closure = Closure::new();
        unsafe {
            let rule: *const Rule = &self.params[0].rules[0];
            origin.insert(Item {
                rule: &*rule,
                pos: 0,
            });
        }
        self.make_closure(origin, 0);

        let mut add_sub: bool;
        loop {
            add_sub = false;
            for state in 0..self.closures.len() {
                for item in self.closures[state].iter() {
                    if item.rule.patts.len() == item.pos {
                        for elem in self
                            .follow
                            .get(&self.parent_of.get(&item.rule).unwrap())
                            .unwrap()
                            .iter()
                        {
                            if {
                                if let Some(Action {
                                    flag: ActionType::MoveIn,
                                    ..
                                }) = self.action[state].get(elem)
                                {
                                    false
                                } else {
                                    true
                                }
                            } {
                                self.action[state].insert(
                                    *elem,
                                    Action {
                                        flag: if item.rule as *const Rule
                                            == &self.params[0].rules[0]
                                        {
                                            ActionType::Accept
                                        } else {
                                            ActionType::Reduce
                                        },
                                        rule: Some(item.rule),
                                    },
                                );
                            }
                        }
                    }
                }
                for i in 0..self.terms.len() {
                    let term = self.terms[i];
                    if {
                        if let None = self.goto[state].get(&term) {
                            true
                        } else {
                            false
                        }
                    } || if let Some(Action {
                        flag: ActionType::Hold,
                        ..
                    }) = self.action[state].get(&term)
                    {
                        true
                    } else {
                        false
                    } {
                        let mut new = Closure::new();
                        for item in self.closures[state].iter() {
                            if item.pos != item.rule.patts.len()
                                && item.rule.patts[item.pos] == term
                            {
                                // println!("Here {}", state);
                                new.insert(Item {
                                    rule: item.rule,
                                    pos: item.pos + 1,
                                });
                            }
                        }
                        if !new.is_empty() {
                            let mut is_sub = false;
                            let mut rule_ptr = None;

                            let mut dest_state: usize = 0;
                            for dest_i in 0..self.closures.len() {
                                is_sub = true;
                                for item in new.iter() {
                                    if rule_ptr.is_none()
                                        || self.index_of.get(&item.rule).unwrap()
                                            < self.index_of.get(&rule_ptr.unwrap()).unwrap()
                                    {
                                        rule_ptr = Some(item.rule);
                                    }
                                    if !self.closures[dest_i].contains(&item) {
                                        is_sub = false;
                                        break;
                                    }
                                }
                                dest_state = dest_i;
                                if is_sub {
                                    break;
                                }
                            }
                            if is_sub {
                                self.goto[state].insert(term, dest_state);
                                self.action[state].insert(term, Action::from(ActionType::MoveIn));
                                let this: *const Self = self;
                                unsafe {
                                    self.expand_closure(&(*this).closures[dest_state], state);
                                }
                            } else {
                                self.goto[state].insert(term, self.closures.len());
                                self.make_closure(new, state);
                                add_sub = true;
                                match self.action[state].get(&term) {
                                    Some(val) => match val.flag {
                                        ActionType::Accept => panic!(),
                                        ActionType::MoveIn => panic!(),
                                        ActionType::Reduce => {
                                            self.action[state].insert(
                                                term,
                                                Action {
                                                    flag: ActionType::MoveIn,
                                                    rule: rule_ptr,
                                                },
                                            );
                                        }
                                        _ => {}
                                    },
                                    None => {
                                        self.action[state]
                                            .insert(term, Action::from(ActionType::MoveIn));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if !add_sub {
                break;
            }
        }
    }

    pub fn new() -> Self {

        let mut parent_of = HashMap::new();
        let mut index_of = HashMap::new();

        let (params, mut terms) = T::new();

        let mut first: HashMap<i64, HashSet<i64>> = HashMap::new();
        let mut follow: HashMap<i64, HashSet<i64>> = HashMap::new();

        // insert terms
        for term in terms.iter() {
            first.index_mut_or_insert(*term).insert(*term);
            follow.index_mut_or_insert(*term);
        }
        // insert params
        for param in params.iter() {
            for rule in param.rules.iter() {
                for patt in rule.patts.iter() {
                    first.index_mut_or_insert(*patt);
                    if *patt < 0 {
                        follow.index_mut_or_insert(*patt).insert(BOTTOM);
                    }
                }
            }
            first.index_mut_or_insert(param.item);
            follow.index_mut_or_insert(param.item).insert(BOTTOM);
            terms.push(param.item);
        }
        // insert eps
        first.index_mut_or_insert(EPS).insert(EPS);
        first.index_mut_or_insert(!EPS).insert(EPS);

        // make first
        make_first(&mut first, &params);
        // make follow
        make_follow(&first, &mut follow, &params);

        terms.push(BOTTOM);

        // index using params::rule!
        let mut index: usize = 0;
        let mut param_of = HashMap::<i64, &'a Param>::new();
        for param in params.iter() {
            for rule in param.rules.iter() {
                unsafe {
                    let rule_ptr: *const Rule = rule;
                    parent_of.insert(&*rule_ptr, param.item);
                    index_of.insert(&*rule_ptr, index);
                    index += 1;
                }
            }
            unsafe {
                let param_ptr: *const Param = param;
                param_of.insert(param.item, &*param_ptr);
            }
        }

        let mut parser = LRParser {
            params: params,
            terms: terms,
            first: first,
            follow: follow,
            parent_of: parent_of,
            index_of: index_of,
            param_of: param_of,
            closures: vec![],
            action: vec![],
            goto: vec![],
            phantom: PhantomData,
        };

        parser.init();

        // println!("CLOSURE = {:?}", parser.closures);
        // println!("ACTION = {:?}", parser.action);
        // println!("GOTO = {:?}", parser.goto);

        parser
    }

    pub fn parse() {}
}

macro_rules! mkrule {
    ($set: ident, $($b: tt)* $(=> $cb: expr)?) => {{
        // unimplemented!()
        let mut rule = Rule {
            patts: vec![],
            handler: Box::new(|| {}),
        };
        $(
            {
                let s = stringify!($b);
                let ha = hash(s);
                // match (s.chars().next(), s.chars().rev().next()) {
                //     (Some('\''), Some('\'')) => {
                //         $set.insert(&ha);
                //         // s = s.chars().rev().skip(1).rev().skip(1).collect();
                //     }
                //     (Some('\"'), Some('\"')) => {
                //         $set.insert(&ha);
                //         // s = s.chars().rev().skip(1).rev().skip(1).collect();
                //     }
                //     _=> {}
                // }
                if $set.contains(&ha) {
                    rule.patts.push(ha);
                } else {
                    rule.patts.push(!ha);
                }
            }
        )*
        $(
            rule.handler = Box::new($cb);
        )*
        rule
    }};
}

macro_rules! lang {
    (
        $(
            $l: ident => $reg: expr
        ),* $(,)?
        ;;
        $(
            $a: ident => [
                $( $($b: ident)* $(=> $cb: expr)?),* $(,)?
            ]
        ),* $(,)?
    ) => {
        #[allow(dead_code, non_snake_case, unused_variables)]
        struct Lang {}
        #[allow(dead_code, non_snake_case, unused_variables)]
        impl LRLang for Lang {
            fn new() -> (Vec<Param>, Vec<Term>) {
                // make terms
                let mut terms = vec![];
                $(
                    terms.push(hash(stringify!($l)));
                )*
                let mut terms_set: HashSet<_> = terms.iter().collect();

                let mut params = vec![];
                // make params
                $({
                    let mut param = Param{
                        rules: vec![],
                        item: !hash(stringify!($a))
                    };
                    $(
                        param.rules.push(
                            mkrule!(terms_set, $($b)* $(=> $cb)?)
                        );
                    )*
                    params.push(param);
                })*

                (params, terms)
            }
        }
    };
}

lang! {

    Number => "[0-9]+",
    Id => "[a-zA-Z]+",
    Add => "+",
    Sub => "-",
    Mul => "-",
    Div => "/",

    ;;

    S => [
        Expr
    ],
    Expr => [
        Expr Add Term,
        Expr Sub Term,
        Term
    ],
    Term => [
        Term Mul Factor,
        Term Div Factor,
        Factor
    ],
    Factor => [
        Number
    ]
}

#[allow(dead_code, non_snake_case, unused_variables)]
fn main() {
    let parser = LRParser::<Lang>::new();

}
