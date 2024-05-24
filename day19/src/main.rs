// Advent of Code 2023
//
// John Perry
//
// Day 19: Aplenty
//
// part 1: sum the "rating numbers" of the parts that survive the rules
//
// part 2: how many products can make it through the rules?
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use lazy_static::lazy_static;
use std::{
    collections::{HashMap, VecDeque},
    io::{BufRead, BufReader},
    ops::{Index, IndexMut},
};

fn main() {
    let (all_rules, all_parts) = read_input();
    println!(
        "sum of accepted parts' ratings is {}",
        part_1(&all_rules, &all_parts)
    );
    println!(
        "number of acceptable parts is {}",
        part_2(&all_rules)
    );
}

#[derive(Debug, Clone, Copy, enum_iterator::Sequence)]
enum Attribute {
    Aero,
    Cool,
    Music,
    Shine,
}

#[derive(Default, Debug, Clone, Copy)]
struct Part {
    cool: usize,
    music: usize,
    aero: usize,
    shine: usize,
}

impl Index<Attribute> for Part {
    type Output = usize;

    fn index(&self, index: Attribute) -> &Self::Output {
        match index {
            Attribute::Cool => &self.cool,
            Attribute::Music => &self.music,
            Attribute::Aero => &self.aero,
            Attribute::Shine => &self.shine,
        }
    }
}

type Label = [char; 3];

// SURE WOULD BE NICE IF ARRAYS WEREN'T A FOREIGN TYPE DONCHA THINK?!?
// impl Default for Label {
//     fn default() -> Self {
//         [' '; 3] as Label
//     }
// }

#[allow(dead_code)]
fn display_label(label: &Label) {
    print!("{}{}{}", label[0], label[1], label[2]);
}

const fn default_label() -> Label {
    [' '; 3]
}

fn from_str(source: &str) -> Label {
    let mut chars = source.chars();
    [
        chars.next().unwrap(),
        chars.next().unwrap(),
        chars.next().unwrap(),
    ]
}
lazy_static! {
    static ref ACCEPTED: Label = from_str("A  ");
    static ref REJECTED: Label = from_str("R  ");
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum TestType {
    Smaller,
    Larger,
}

#[derive(Debug, Clone, Copy)]
struct Criterion {
    attr: Attribute,
    test: TestType,
    value: usize,
    dest: Label,
}

#[derive(Debug, Clone)]
struct Rule {
    criteria: Vec<Criterion>,
    default: Label,
}

impl Default for Rule {
    fn default() -> Self {
        Self {
            criteria: Default::default(),
            default: default_label(),
        }
    }
}

fn get_label(s: &String, first: &mut usize) -> Label {
    let mut last: usize = *first;
    let mut result = default_label();
    let mut chars = s.chars();
    let mut c = chars.nth(last).unwrap();

    while last < s.len() && (c.is_ascii_lowercase() || c == 'A' || c == 'R') {
        result[last - *first] = c;
        last += 1;
        c = chars.next().unwrap();
    }
    *first = last - 1;
    result
}

fn get_natural(s: &String, pos: &mut usize) -> usize {
    let mut value = 0;

    let mut chars = s.chars();
    let mut c = chars.nth(*pos).unwrap();
    while *pos < s.len() && c.is_ascii_digit() {
        value = value * 10 + str::parse::<usize>(&String::from(c)).unwrap()
            - str::parse::<usize>(&String::from('0')).unwrap();
        *pos += 1;
        c = chars.next().unwrap();
    }
    *pos -= 1;
    value
}

fn get_part(s: &String) -> Part {
    let mut pos = 3;
    let cool = get_natural(s, &mut pos);
    pos += 4;
    let music = get_natural(s, &mut pos);
    pos += 4;
    let aero = get_natural(s, &mut pos);
    pos += 4;
    let shine = get_natural(s, &mut pos);

    Part {
        cool,
        music,
        aero,
        shine,
    }
}

fn get_criterion(s: &String, first: &mut usize) -> Criterion {
    let mut last: usize = *first;

    let mut chars = s.chars();

    let attr = match chars.nth(last).unwrap() {
        'a' => Attribute::Aero,
        'm' => Attribute::Music,
        's' => Attribute::Shine,
        'x' => Attribute::Cool,
        _ => panic!("invalid attribute!"),
    };

    let test = if chars.next().unwrap() == '<' {
        TestType::Smaller
    } else {
        TestType::Larger
    };
    last += 2;

    let value = get_natural(s, &mut last);
    last += 2;

    let dest = get_label(s, &mut last);
    *first = last + 2;

    Criterion {
        attr,
        test,
        value,
        dest,
    }
}

fn read_input() -> (HashMap<Label, Rule>, Vec<Part>) {
    let file = std::fs::File::open("input.txt").expect("need some input!");
    let mut lines = BufReader::new(file).lines();
    let mut all_rules = HashMap::new();
    loop {
        let maybe_line = lines.next().expect("whoah... a line without a line?");
        let line = maybe_line.expect("how'd we have a line that's not a line?!?");
        let mut pos = 0;
        if line.is_empty() {
            break;
        }
        let label = get_label(&line, &mut pos);
        let mut chars = line.chars();
        let mut c = chars.nth(pos).unwrap();
        let mut rule = Rule::default();
        while c != '}' {
            pos += 2;
            let mut chars = line.chars().skip(pos);
            while chars.any(|char| char == ',') {
                rule.criteria.push(get_criterion(&line, &mut pos));
            }
            rule.default = get_label(&line, &mut pos);
            pos += 1;
            let mut chars = line.chars().skip(pos);
            c = chars.next().unwrap();
        }
        all_rules.insert(label, rule);
    }

    let mut all_parts = Vec::new();
    for maybe_line in lines {
        let line = maybe_line.expect("hmm... a line that is not a line... hmm...");
        let part = get_part(&line);
        all_parts.push(part);
    }

    (all_rules, all_parts)
}

fn apply_rule(all_rules: &HashMap<Label, Rule>, p: Part, label: Label) -> Label {
    let rule = all_rules[&label].clone();
    for criterion in &rule.criteria {
        if (criterion.test == TestType::Smaller && p[criterion.attr] < criterion.value)
            || (criterion.test == TestType::Larger && p[criterion.attr] > criterion.value)
        {
            return criterion.dest;
        }
    }
    rule.default
}

fn equal(left: &Label, right: &Label) -> bool {
    left.iter()
        .enumerate()
        .all(|(idx, value)| *value == right[idx])
}

fn part_1(all_rules: &HashMap<Label, Rule>, all_parts: &Vec<Part>) -> usize {
    use Attribute::*;
    let mut result = 0;

    for part in all_parts {
        let mut label: Label = ['i', 'n', ' '];
        while !equal(&label, &ACCEPTED) && !equal(&label, &REJECTED) {
            label = apply_rule(all_rules, *part, label);
            if equal(&label, &ACCEPTED) {
                result += part[Cool] + part[Music] + part[Aero] + part[Shine];
            }
        }
    }

    result
}

#[derive(Clone)]
struct AttributeArray {
    aero: usize,
    cool: usize,
    music: usize,
    shine: usize,
}

impl AttributeArray {
    fn new(default: usize) -> Self {
        Self {
            aero: default,
            cool: default,
            music: default,
            shine: default,
        }
    }
}

impl Index<Attribute> for AttributeArray {
    type Output = usize;

    fn index(&self, index: Attribute) -> &Self::Output {
        use Attribute::*;
        match index {
            Cool => &self.cool,
            Music => &self.music,
            Aero => &self.aero,
            Shine => &self.shine,
        }
    }
}

impl IndexMut<Attribute> for AttributeArray {
    fn index_mut(&mut self, index: Attribute) -> &mut Self::Output {
        use Attribute::*;
        match index {
            Cool => &mut self.cool,
            Music => &mut self.music,
            Aero => &mut self.aero,
            Shine => &mut self.shine,
        }
    }
}

#[derive(Clone)]
struct Trace {
    location: Label,
    mins: AttributeArray,
    maxs: AttributeArray,
}

fn part_2(all_rules: &HashMap<Label, Rule>) -> usize {
    let mut result = 0;
    let mut traces = VecDeque::default();
    traces.push_back(Trace {
        location: ['i', 'n', ' '],
        mins: AttributeArray::new(1),
        maxs: AttributeArray::new(4_000),
    });

    while !traces.is_empty() {
        let mut trace = traces.pop_front().unwrap();

        if equal(&trace.location, &ACCEPTED) {
            let mut product = 1;
            for attribute in enum_iterator::all::<Attribute>() {
                product *= trace.maxs[attribute] - trace.mins[attribute] + 1;
            }
            result += product;
        } else if equal(&trace.location, &REJECTED) {
            // do nothing
        } else {
            let mut found = false;
            let rule = &all_rules[&trace.location];
            for criterion in &rule.criteria {
                if criterion.test == TestType::Smaller {
                    if trace.maxs[criterion.attr] < criterion.value {
                        if !equal(&criterion.dest, &REJECTED) {
                            trace.location = criterion.dest;
                            traces.push_back(trace.clone());
                        }
                        found = true;
                    } else if trace.mins[criterion.attr] >= criterion.value {
                        // do nothing
                    } else {
                        let mut trace_more = trace.clone();
                        trace_more.mins[criterion.attr] = criterion.value;
                        traces.push_back(trace_more);
                        if !equal(&criterion.dest, &REJECTED) {
                            let mut trace_less = trace.clone();
                            trace_less.maxs[criterion.attr] = criterion.value - 1;
                            trace_less.location = criterion.dest;
                            traces.push_back(trace_less);
                        }
                        found = true;
                    }
                } else {
                    // criterion.test == TestType::Larger
                    if trace.mins[criterion.attr] > criterion.value {
                        if !equal(&criterion.dest, &REJECTED) {
                            trace.location = criterion.dest;
                            traces.push_back(trace.clone());
                        }
                        found = true;
                    } else if trace.maxs[criterion.attr] <= criterion.value {
                        // do nothing
                    } else {
                        let mut trace_less = trace.clone();
                        trace_less.maxs[criterion.attr] = criterion.value;
                        traces.push_back(trace_less);
                        if !equal(&criterion.dest, &REJECTED) {
                            let mut trace_more = trace.clone();
                            trace_more.mins[criterion.attr] = criterion.value + 1;
                            trace_more.location = criterion.dest;
                            traces.push_back(trace_more);
                        }
                        found = true;
                    }
                }
                if found {
                    break;
                }
            }
            if !found {
                trace.location = rule.default;
                traces.push_back(trace);
            }
        }
    }

    result
}
