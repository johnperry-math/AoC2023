// Advent of Code 2023
//
// John Perry
//
// Day 8: Haunted Wasteland
//
// part 1: how long does it take to get from AAA to ZZZ?
//
// part 2: how long does it take to get from every node ending with A
//         to a node ending with Z?
//
// this basically translates the Ada code; see that for details

use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
};

type Node = [char; 3];

struct Connection {
    left: Node,
    right: Node,
}

#[derive(Clone, Copy, PartialEq)]
enum Direction {
    Left,
    Right,
}

fn main() {
    let (directions, map, ghost_starts) = read_input();
    println!(
        "It takes {} steps to reach ZZZ from AAA",
        part_1(&directions, &map)
    );
    println!(
        "It takes {} steps when traversing all xxA",
        part_2(&directions, &map, ghost_starts)
    );
}

fn read_input() -> (Vec<Direction>, HashMap<Node, Connection>, Vec<Node>) {
    let input = std::fs::File::open("input.txt").expect("what, where's input.txt?!?");
    let mut lines = BufReader::new(input).lines();

    let directions = lines
        .next()
        .expect("no directions?!?")
        .expect("no, really: no directions?!?")
        .chars()
        .map(|c| {
            if c == 'L' {
                Direction::Left
            } else {
                Direction::Right
            }
        })
        .collect();

    let lines = lines.skip(1);

    let mut ghost_starts = Vec::new();
    let map: HashMap<Node, Connection> = lines
        .map(|line| {
            let binding = line.expect("uhh... how did we iterate to a non-existent line?!?");
            let mut split = binding.split_ascii_whitespace();
            let mut source = ['.'; 3];
            let mut left = ['.'; 3];
            let mut right = ['.'; 3];
            split
                .next()
                .expect("what, no source?!?")
                .chars()
                .enumerate()
                .for_each(|(ith, c)| source[ith] = c);
            if source[2] == 'A' {
                ghost_starts.push(source);
            }
            let _ = split.next();
            split
                .next()
                .expect("what, no left?!?")
                .chars()
                .skip(1)
                .take(3)
                .enumerate()
                .for_each(|(ith, c)| left[ith] = c);
            split
                .next()
                .expect("what, no right?!?")
                .chars()
                .take(3)
                .enumerate()
                .for_each(|(ith, c)| right[ith] = c);
            (source, Connection { left, right })
        })
        .collect::<HashMap<Node, Connection>>();

    (directions, map, ghost_starts)
}

fn part_1(directions: &Vec<Direction>, map: &HashMap<Node, Connection>) -> usize {
    let mut n: Node = ['A', 'A', 'A'];
    let mut step = 0;
    const TERMINUS: Node = ['Z', 'Z', 'Z'];
    while n != TERMINUS {
        let direction = directions[step % directions.len()];
        n = if direction == Direction::Left {
            map[&n].left
        } else {
            map[&n].right
        };
        step += 1;
    }
    step
}

fn part_2(
    directions: &Vec<Direction>,
    map: &HashMap<Node, Connection>,
    ghost_starts: Vec<Node>,
) -> usize {
    let mut step = 0;
    let mut locations = ghost_starts.clone();
    let mut path_lengths: Vec<usize> = ghost_starts.iter().map(|_| 0).collect();
    while locations
        .iter()
        .enumerate()
        .any(|(_, location)| location[2] != 'Z')
    {
        for (ith, location) in locations
            .iter_mut()
            .enumerate()
            .filter(|(_, location)| location[2] != 'Z')
        {
            let direction = directions[step % directions.len()];
            let lookup = *location;
            *location = if direction == Direction::Left {
                map[&lookup].left
            } else {
                map[&lookup].right
            };
            if location[2] == 'Z' {
                path_lengths[ith] = step + 1;
            }
        }
        step += 1;
    }
    path_lengths
        .iter()
        .fold(1, |result, value| num::integer::lcm(result, *value))
}
