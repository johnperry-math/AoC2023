//  Advent of Code 2023
//
//  John Perry
//
//  Day 23: A long walk
//
//  part 1: find the _longest_ path through a maze,
//          when you can't walk against slopes
//
//  part 2: repeat, except now you can walk up slopes
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{collections::VecDeque, ops::RangeInclusive};

use common::{
    two_dimensional_map::{Location, Map},
    two_dimensional_map_io::read_input,
    two_dimensional_motion::{delta, Direction, ALL_DIRECTIONS},
};

const DOING_EXAMPLE: bool = false;
const SIDE_LENGTH: usize = if DOING_EXAMPLE { 23 } else { 141 };
const SIDE_RANGE: RangeInclusive<usize> = RangeInclusive::new(0, SIDE_LENGTH - 1);

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
enum Object {
    #[default]
    Forest,
    Path,
    SlopeEast,
    SlopeWest,
    SlopeNorth,
    SlopeSouth,
}

const fn opposite(dir: Direction) -> Object {
    match dir {
        Direction::North => Object::SlopeSouth,
        Direction::South => Object::SlopeNorth,
        Direction::East => Object::SlopeWest,
        Direction::West => Object::SlopeEast,
    }
}

fn deserialize(symbol: char) -> Object {
    match symbol {
        '.' => Object::Path,
        '#' => Object::Forest,
        '>' => Object::SlopeEast,
        '<' => Object::SlopeWest,
        '^' => Object::SlopeNorth,
        'v' => Object::SlopeSouth,
        _ => panic!("invalid map symbol!"),
    }
}

// fn serialize(o: Object) -> char {
//     match o {
//         Object::Forest => '#',
//         Object::Path => '.',
//         Object::SlopeEast => '>',
//         Object::SlopeWest => '<',
//         Object::SlopeNorth => '^',
//         Object::SlopeSouth => 'v',
//     }
// }

const GOIN: Location = Location::new(0, 1);
const GOAL: Location = Location::new(SIDE_LENGTH - 1, SIDE_LENGTH - 2);

fn part_1(map: &Map<SIDE_LENGTH, SIDE_LENGTH, Object>) -> usize {
    let mut queue = VecDeque::new();
    let mut best = Vec::new();

    queue.push_back(vec![GOIN]);

    while let Some(curr) = queue.pop_front() {
        let pos = curr.last().expect("how'd we get an empty path in here?!?");
        if *pos == GOAL && curr.len() > best.len() {
            best.clone_from(&curr);
        }
        for dir in ALL_DIRECTIONS {
            let d = delta(dir);
            #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
            let step = Location::new(
                (pos.row() as isize + isize::from(d.d_row())) as usize,
                (pos.col() as isize + isize::from(d.d_col())) as usize,
            );
            if SIDE_RANGE.contains(&step.row())
                && SIDE_RANGE.contains(&step.col())
                && map[step] != Object::Forest
                && !curr.contains(&step)
            {
                if map[step] == Object::Path {
                    let mut next = curr.clone();
                    next.push(step);
                    queue.push_back(next);
                } else if map[step] != opposite(dir) {
                    let mut next = curr.clone();
                    next.push(step);
                    next.push(match map[step] {
                        Object::SlopeEast => Location::new(step.row(), step.col() + 1),
                        Object::SlopeWest => Location::new(step.row(), step.col() - 1),
                        Object::SlopeNorth => Location::new(step.row() - 1, step.col()),
                        Object::SlopeSouth => Location::new(step.row() + 1, step.col()),
                        _ => panic!("invalid step in direction {d:?} from position {step:?}"),
                    });
                    queue.push_back(next);
                }
            }
        }
    }

    best.len() - 1
}

fn main() {
    let map = read_input("input.txt".to_string(), &deserialize);
    println!("the most scenic route's length is {} steps", part_1(&map));
}
