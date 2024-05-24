// Advent of Code 2023
//
// John Perry
//
// Day 20: Pulse Propagation
//
//  part 1: determine product of low and high pulses after 1000 button presses
//
//  part 2: determine how many button presses it will take to activate machine
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{
    collections::{BTreeMap, VecDeque}, io::{BufRead, BufReader}, iter::Skip, ops::{Index, IndexMut}, str::Chars
};

use num::Integer;

const FLIPPED_OFF: bool = false;

type Label = [char; 2];

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
struct MemoryRecord {
    source: Label,
    last_pulse: PulseEnum,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
enum Module {
    Broadcast,
    Conjunction { memory: Vec<MemoryRecord> },
    Flipflop { flip_state: bool },
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Clone, Copy)]
enum PulseEnum {
    Low,
    High,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug)]
struct ModuleRecord {
    children: Vec<Label>,
    kind: Module,
}

#[derive(Default)]
struct PulseData {
    lo: usize,
    hi: usize,
}

impl Index<PulseEnum> for PulseData {
    type Output = usize;

    fn index(&self, index: PulseEnum) -> &Self::Output {
        match index {
            PulseEnum::Low => &self.lo,
            PulseEnum::High => &self.hi,
        }
    }
}

impl IndexMut<PulseEnum> for PulseData {
    fn index_mut(&mut self, index: PulseEnum) -> &mut Self::Output {
        match index {
            PulseEnum::Low => &mut self.lo,
            PulseEnum::High => &mut self.hi,
        }
    }
}

fn main() {
    let mut all_modules = read_input();
    setup_memories(&mut all_modules);
    parts_1_and_2(&mut all_modules);
}

fn get_children(chars: &mut Skip<Chars>, children: &mut Vec<Label>) {
    while let Some(first) = chars.next() {
        let second = chars.next().unwrap();
        children.push([first, second]);
        let (_, _) = (chars.next(), chars.next());
    }
}

fn get_label(chars: &mut Skip<Chars>, label: &mut Label) {
    label[0] = chars.next().expect("no first character!");
    label[1] = chars.next().expect("no second character!");
}

fn read_input() -> BTreeMap<Label, ModuleRecord> {
    let mut result = BTreeMap::new();
    let input = std::fs::File::open("input.txt").expect("where's my input?!?");
    let lines = BufReader::new(input).lines();
    for maybe_line in lines {
        let mut children = Vec::new();
        let line = maybe_line.expect("a line that's not a line?!?");
        let char_1 = line
            .chars()
            .next()
            .expect("i was expecting a character here... {line}");
        let mut label = ['t', 'x'];
        match char_1 {
            '%' => {
                get_label(&mut line.chars().skip(1), &mut label);
                get_children(&mut line.chars().skip(7), &mut children);
                let module = ModuleRecord {
                    children,
                    kind: Module::Flipflop {
                        flip_state: FLIPPED_OFF,
                    },
                };
                result.insert(label, module);
            }
            '&' => {
                get_label(&mut line.chars().skip(1), &mut label);
                get_children(&mut line.chars().skip(7), &mut children);
                let module = ModuleRecord {
                    children,
                    kind: Module::Conjunction { memory: Vec::new() },
                };
                result.insert(label, module);
            }
            _ => {
                get_children(&mut line.chars().skip(15), &mut children);
                let module = ModuleRecord {
                    children,
                    kind: Module::Broadcast,
                };
                result.insert(label, module);
            }
        }
    }
    result
}

fn setup_memories(all_modules: &mut BTreeMap<Label, ModuleRecord>) {
    let mut new_memories = BTreeMap::<[char; 2], Vec<MemoryRecord>>::new();
    for label in all_modules.keys() {
        for child_label in &all_modules[label].children {
            if all_modules.contains_key(child_label) {
                let child = all_modules
                    .get(child_label)
                    .expect("whoah, unable to find {child_label}!");
                if let Module::Conjunction { .. } = &child.kind {
                    new_memories
                        .entry(*child_label)
                        .or_default()
                        .push(MemoryRecord {
                            source: *label,
                            last_pulse: PulseEnum::Low,
                        });
                }
            }
        }
    }
    for (child_label, mut new_memory) in new_memories {
        if let Module::Conjunction { memory } = &mut all_modules.get_mut(&child_label).unwrap().kind
        {
            memory.append(&mut new_memory);
        }
    }
}

struct BroadcastRecord {
    source: Label,
    dest: Label,
    kind: PulseEnum,
}

fn pulse(
    module: &mut ModuleRecord,
    pulse: PulseEnum,
    source: Label,
    me: Label,
    queue: &mut VecDeque<BroadcastRecord>,
    pulses: &mut PulseData,
) {
    let mut propagate = PulseEnum::Low;
    match &mut module.kind {
        Module::Broadcast => {
            for child in &module.children {
                queue.push_back(BroadcastRecord {
                    source: me,
                    dest: *child,
                    kind: pulse,
                });
            }
            pulses[propagate] += module.children.len() + 1;
        }
        Module::Conjunction { memory } => {
            let mut ith = 0;
            while memory[ith].source != source {
                ith += 1;
            }
            memory[ith].last_pulse = pulse;

            if memory.iter().any(|mem| mem.last_pulse == PulseEnum::Low) {
                propagate = PulseEnum::High;
            }
            for child in &module.children {
                queue.push_back(BroadcastRecord {
                    source: me,
                    dest: *child,
                    kind: propagate,
                });
            }
            pulses[propagate] += module.children.len();
        }
        Module::Flipflop { flip_state } => {
            if pulse == PulseEnum::Low {
                *flip_state = !*flip_state;
                if *flip_state {
                    propagate = PulseEnum::High;
                }
                for child in &module.children {
                    queue.push_back(BroadcastRecord {
                        source: me,
                        dest: *child,
                        kind: propagate,
                    });
                }
                pulses[propagate] += module.children.len();
            }
        }
    }
}

fn parts_1_and_2(all_modules: &mut BTreeMap<Label, ModuleRecord>) {
    let rx_source = all_modules
        .iter()
        .find(|(_, module)| module.children.contains(&['r', 'x']))
        .map(|(label, _)| *label)
        .unwrap();
    let mut rx_source_sources = all_modules
        .iter()
        .filter(|(_, module)| module.children.contains(&rx_source))
        .map(|(label, _)| (*label, 0_usize))
        .collect::<BTreeMap<_, _>>();

    let mut each = 0;
    let mut pulses = PulseData::default();
    let mut broadcast_queue = VecDeque::new();
    'button_press: loop {
        each += 1;
        broadcast_queue.push_back(BroadcastRecord {
            source: ['b', 't'],
            dest: ['t', 'x'],
            kind: PulseEnum::Low,
        });
        while !broadcast_queue.is_empty() {
            let br = broadcast_queue.pop_front().unwrap();

            if br.dest != ['r', 'x'] {
                pulse(
                    all_modules.get_mut(&br.dest).unwrap(),
                    br.kind,
                    br.source,
                    br.dest,
                    &mut broadcast_queue,
                    &mut pulses,
                );
                if br.dest == rx_source
                    && br.kind == PulseEnum::High
                    && rx_source_sources[&br.source] == 0
                {
                    println!(
                        "On button press {each} {rx_source:?} received {:?} from {:?}",
                        br.kind, br.source
                    );
                    *rx_source_sources.get_mut(&br.source).unwrap() = each;
                    if rx_source_sources.iter().all(|(_, value)| *value != 0) {
                        break 'button_press;
                    }
                }
            }
        }
        if each == 1_000 {
            println!(
                "after 1000 button presses, low pulses: {} high pulses: {}",
                pulses.lo, pulses.hi
            );
            println!(
                "product of high and low pulses is {}",
                pulses.hi * pulses.lo
            );
        }
    }

    let mut result = 1_usize;
    for (_, each) in rx_source_sources {
        result = result.lcm(&each);
    }
    println!("it will require {result} presses to start the machine");
}
