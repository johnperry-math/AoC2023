// Advent of Code 2023
//
// John Perry
//
// Day 14: Parabolic Reflector Dish
//
// part 1: compute the load incurred by the movable rocks after tilting north
//
// part 2: compute the load after 1 billion spin cycles
//
// this basically translates the Ada code; see that for details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::io::BufRead;

fn main() {
    let initialization = read_input();
    println!("sum of hash results is {}", part_1(&initialization));
    println!("focusing power is {}", part_2(&initialization));
}

fn read_input() -> String {
    let input = std::fs::File::open("input.txt").expect("Dood, where's my input?");
    let lines = std::io::BufReader::new(input).lines();
    lines
        .into_iter()
        .next()
        .expect("no line of input?!?")
        .expect("no line in the line of input?!?")
}

type HashValue = u8;

fn hash(s: &str) -> HashValue {
    s.chars()
        .filter(|symbol| *symbol != ' ')
        .fold(0, |result, symbol| {
            (usize::from(result + symbol as u8) * 17 % 256) as HashValue
        })
}

fn part_1(string: &str) -> usize {
    string
        .clone()
        .split(',')
        .fold(0, |result, substring| result + usize::from(hash(substring)))
}

type Lens = String;

struct LabeledLens {
    label: Lens,
    focal_length: u8,
}

fn find_label(lens: &Lens, my_box: &[LabeledLens]) -> Option<usize> {
    my_box
        .iter()
        .enumerate()
        .find(|(_, labeled_lens)| labeled_lens.label == *lens)
        .map(|(ith, _)| ith)
}

fn focusing_power(labeled_lens: &LabeledLens, slot_number: usize) -> usize {
    (1 + usize::from(hash(&labeled_lens.label)))
        * slot_number
        * usize::from(labeled_lens.focal_length)
}

fn part_2(string: &str) -> usize {
    const EMPTY_BOX: Vec<LabeledLens> = Vec::new();
    let mut boxes = [EMPTY_BOX; 256];
    string.split(',').for_each(|command| {
        let chars = command.chars();
        let label: String = chars.take_while(|c| c.is_ascii_lowercase()).collect();
        let mut chars = command.chars().skip(label.len());
        let slot = usize::from(hash(&label));
        let ch = chars.next().expect("unable to get next character!");
        match ch {
            '-' => {
                if let Some(position) = find_label(&label, &boxes[slot]) {
                    // println!("removing {label} from {slot}, {}", position + 1);
                    boxes[slot].remove(position);
                    // } else {
                    //     println!("{label} does not exist in {slot}");
                }
            }
            '=' => {
                let focal_length =
                    String::from(chars.next().expect("unable to extract digit from {ch}!"))
                        .parse::<u8>()
                        .expect("failed to parse a digit! from {ch}");
                if let Some(position) = find_label(&label, &boxes[slot]) {
                    // println!("adjusting {label} at {slot}, {} to {focal_length}", position + 1);
                    boxes[slot][position].focal_length = focal_length;
                } else {
                    // println!("adding {label} to {slot} at {} with {focal_length}", boxes[slot].len() + 1);
                    boxes[slot].push(LabeledLens {
                        label,
                        focal_length,
                    });
                }
            }
            _ => println!("uh-oh; {} is unexpected: {ch}", label.len()),
        }
    });
    boxes.iter().fold(0, |result, each_box| {
        result
            + each_box
                .iter()
                .enumerate()
                .fold(0, |result, (ith, labeled_lens)| {
                    result + focusing_power(labeled_lens, ith + 1)
                })
    })
}
