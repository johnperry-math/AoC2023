// Advent of Code 2023
//
// John Perry
//
// Day 10: Pipe Maze
//
// part 1: determine the locations of the closed loop
//
// part 2: count the locations within the closed loop
//
// this mostly transcribes the Ada code, with alterations
// to account for differences in indexing. see the Ada code
// for detailed comments on the logic.

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{
    collections::VecDeque,
    io::{BufRead, BufReader},
};

fn main() {
    let (start, map) = read_input();
    let mut traversed_map = [[false; SIDE_LENGTH]; SIDE_LENGTH];
    println!(
        "from entrance to farthest point takes {} steps",
        part_1(&map, &mut traversed_map, start)
    );
    // put_traversed_map(&traversed_map);
    println!("nest area contains {} spaces", part_2(&map, &traversed_map));
}

#[derive(Default, Clone, Copy, PartialEq)]
enum Pipe {
    Vertical,
    Horizontal,
    SEOrWN,
    SWOrEN,
    WSOrNE,
    ESOrNW,
    #[default]
    Ground,
    Start,
}

const DOING_EXAMPLE: bool = false;

const SIDE_LENGTH: usize = if DOING_EXAMPLE { 20 } else { 140 };

type MapType = [[Pipe; SIDE_LENGTH]; SIDE_LENGTH];

type TraversedMapType = [[bool; SIDE_LENGTH]; SIDE_LENGTH];

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
struct Location {
    row: usize,
    col: usize,
}

fn read_input() -> (Location, MapType) {
    let mut result = [[Pipe::default(); SIDE_LENGTH]; SIDE_LENGTH];
    let mut start_location = Location { row: 0, col: 0 };
    let file = std::fs::File::open(if DOING_EXAMPLE {
        "example.txt"
    } else {
        "input.txt"
    })
    .expect("where's my input?");
    let lines = BufReader::new(file).lines();
    lines.enumerate().take(SIDE_LENGTH).for_each(|(row, line)| {
        line.expect("we had a line that's not OK?!")
            .chars()
            .enumerate()
            .for_each(|(col, symbol)| {
                result[row][col] = match symbol {
                    '|' => Pipe::Vertical,
                    '-' => Pipe::Horizontal,
                    'L' => Pipe::SEOrWN,
                    'J' => Pipe::SWOrEN,
                    '7' => Pipe::ESOrNW,
                    'F' => Pipe::WSOrNE,
                    '.' => Pipe::Ground,
                    'S' => Pipe::Start,
                    _ => panic!("we encountered an invalid input! {symbol}"),
                };
                if symbol == 'S' {
                    start_location = Location { row, col };
                }
            });
    });
    (start_location, result)
}

#[allow(dead_code)]
fn put_traversed_map(traversed_map: &TraversedMapType) {
    for row in traversed_map {
        for col in row {
            print!("{}", if *col { '#' } else { ' ' });
        }
        println!();
    }
}

#[derive(Clone, Copy, Debug)]
struct Animal {
    curr: Location,
    prev: Location,
}

fn can_move(map: &MapType, here: Location, from: Location) -> bool {
    #[allow(clippy::enum_glob_use)]
    use Pipe::*;
    let current = map[from.row][from.col];
    match map[here.row][here.col] {
        Vertical => {
            (from.col == here.col
                && from.row - here.row == 1
                && current != Horizontal
                && current != ESOrNW
                && current != WSOrNE)
                || (from.col == here.col
                    && here.row - from.row == 1
                    && current != Horizontal
                    && current != SWOrEN
                    && current != SEOrWN)
        }
        Horizontal => {
            (from.row == here.row
                && from.col - here.col == 1
                && current != SEOrWN
                && current != WSOrNE
                && current != Vertical)
                || (from.row == here.row
                    && here.col - from.col == 1
                    && current != SWOrEN
                    && current != ESOrNW
                    && current != Vertical)
        }
        SEOrWN => {
            (here.row - from.row == 1
                && from.col == here.col
                && current != Horizontal
                && current != SEOrWN
                && current != SWOrEN)
                || (from.row == here.row
                    && from.col - here.col == 1
                    && current != WSOrNE
                    && current != SEOrWN
                    && current != Vertical)
        }
        SWOrEN => {
            (from.row == here.row
                && here.col - from.col == 1
                && current != ESOrNW
                && current != Vertical
                && current != SWOrEN)
                || (here.row - from.row == 1
                    && from.col == here.col
                    && current != Horizontal
                    && current != SEOrWN
                    && current != SWOrEN)
        }
        WSOrNE => {
            (from.row - here.row == 1
                && from.col == here.col
                && current != Horizontal
                && current != ESOrNW
                && current != WSOrNE)
                || (from.row == here.row
                    && from.col - here.col == 1
                    && current != SEOrWN
                    && current != WSOrNE
                    && current != Vertical)
        }
        ESOrNW => {
            (from.row - here.row == 1
                && from.col == here.col
                && current != ESOrNW
                && current != Horizontal
                && current != WSOrNE)
                || (from.row == here.row
                    && here.col - from.col == 1
                    && current != ESOrNW
                    && current != SWOrEN
                    && current != Vertical)
        }
        Ground => panic!("we've left the loop at {here:?} from {from:?}"),
        Start => panic!("we seem to have cycled from {from:?}"),
    }
}

#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn move_animal(me: &mut Animal, map: &MapType, but_not_here: Location) {
    for d_row in (-1_isize..=1)
        .filter(|d_row| (0..SIDE_LENGTH as isize).contains(&(me.curr.row as isize + d_row)))
    {
        for d_col in (-1_isize..=1)
            .filter(|d_col| (0..SIDE_LENGTH as isize).contains(&(me.curr.col as isize + d_col)))
        {
            if d_row.abs() != d_col.abs() {
                let option = Location {
                    row: (me.curr.row as isize + d_row) as usize,
                    col: (me.curr.col as isize + d_col) as usize,
                };
                if option != me.prev
                    && option != but_not_here
                    && map[option.row][option.col] != Pipe::Ground
                    && can_move(map, option, me.curr)
                {
                    me.prev = me.curr;
                    me.curr = option;
                    return;
                }
            }
        }
    }
    panic!("Did not move from {:?}", me.curr);
}

fn part_1(map: &MapType, traversed_map: &mut TraversedMapType, start: Location) -> usize {
    let mut first = Animal {
        curr: start,
        prev: start,
    };
    let mut second = first;

    move_animal(&mut first, map, start);
    move_animal(&mut second, map, first.curr);
    traversed_map[start.row][start.col] = true;
    traversed_map[first.curr.row][first.curr.col] = true;
    traversed_map[second.curr.row][second.curr.col] = true;

    let mut step = 1;
    loop {
        move_animal(&mut first, map, start);
        traversed_map[first.curr.row][first.curr.col] = true;
        if first.curr == second.curr {
            break;
        }
        move_animal(&mut second, map, start);
        traversed_map[second.curr.row][second.curr.col] = true;
        if second.curr == first.curr {
            break;
        }
        step += 1;
    }
    step + 1
}

const DOUBLED_SIDE_LENGTH: usize = 2 * SIDE_LENGTH + 1;

type DoubledMapType = [[bool; DOUBLED_SIDE_LENGTH]; DOUBLED_SIDE_LENGTH];

#[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
fn flood_fill(is_filled: &mut DoubledMapType) {
    let mut to_do = VecDeque::<Location>::default();

    to_do.push_back(Location { row: 0, col: 0 });
    is_filled[0][0] = true;
    to_do.push_back(Location {
        row: DOUBLED_SIDE_LENGTH - 1,
        col: 0,
    });
    is_filled[DOUBLED_SIDE_LENGTH - 1][0] = true;

    while !to_do.is_empty() {
        let current = to_do
            .pop_front()
            .expect("if it's not empty, why can't we pop?!?");
        for d_row in (-1..=1_isize).filter(|d_row| {
            (0..DOUBLED_SIDE_LENGTH as isize).contains(&(current.row as isize + d_row))
        }) {
            for d_col in (-1..=1_isize).filter(|d_col| {
                (0..DOUBLED_SIDE_LENGTH as isize).contains(&(current.col as isize + d_col))
            }) {
                let next = Location {
                    row: (current.row as isize + d_row) as usize,
                    col: (current.col as isize + d_col) as usize,
                };
                if !is_filled[next.row][next.col] {
                    is_filled[next.row][next.col] = true;
                    to_do.push_back(next);
                }
            }
        }
    }
}

fn size_of_interior(is_filled: &mut DoubledMapType) -> usize {
    let mut result = 0;
    (0..SIDE_LENGTH).for_each(|row| {
        (0..SIDE_LENGTH).for_each(|col| {
            if !is_filled[2 * row + 1][2 * col + 1] {
                result += 1;
            }
        });
    });
    result
}

#[allow(clippy::identity_op, clippy::cast_possible_wrap)]
fn double_map(map: &MapType, traversed_map: &TraversedMapType) -> DoubledMapType {
    #[allow(clippy::enum_glob_use)]
    use Pipe::*;
    let mut result = [[false; DOUBLED_SIDE_LENGTH]; DOUBLED_SIDE_LENGTH];

    map.iter().enumerate().for_each(|(ith, row)| {
        row.iter().enumerate().for_each(|(jth, entry)| {
            if traversed_map[ith][jth] {
                match entry {
                    Vertical => {
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 2][2 * jth + 1] = true;
                    }
                    Horizontal => {
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 1][2 * jth + 2] = true;
                    }
                    SEOrWN => {
                        result[2 * ith + 0][2 * jth + 1] = true;
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 1][2 * jth + 2] = true;
                    }
                    SWOrEN => {
                        result[2 * ith + 1][2 * jth + 0] = true;
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 0][2 * jth + 1] = true;
                    }
                    WSOrNE => {
                        result[2 * ith + 2][2 * jth + 1] = true;
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 1][2 * jth + 2] = true;
                    }
                    ESOrNW => {
                        result[2 * ith + 1][2 * jth + 0] = true;
                        result[2 * ith + 1][2 * jth + 1] = true;
                        result[2 * ith + 2][2 * jth + 1] = true;
                    }
                    Ground => {}
                    Start => {
                        result[2 * ith + 1][2 * jth + 1] = true;
                        if (0..SIDE_LENGTH as isize).contains(&(ith as isize - 1)) {
                            match map[ith - 1][jth] {
                                Vertical | WSOrNE | ESOrNW => {
                                    result[2 * ith + 0][2 * jth + 1] = true;
                                }
                                _ => {}
                            }
                        }
                        if (0..SIDE_LENGTH).contains(&(ith + 1)) {
                            match map[ith + 1][jth] {
                                Vertical | SEOrWN | SWOrEN => {
                                    result[2 * ith + 2][2 * jth + 1] = true;
                                }
                                _ => {}
                            }
                        }
                        if (0..SIDE_LENGTH as isize).contains(&(jth as isize - 1)) {
                            match map[ith][jth - 1] {
                                Horizontal | SEOrWN | WSOrNE => {
                                    result[2 * ith + 1][2 * jth + 0] = true;
                                }
                                _ => {}
                            }
                        }
                        if (0..SIDE_LENGTH).contains(&(jth + 1)) {
                            match map[ith][jth + 1] {
                                Horizontal | SWOrEN | ESOrNW => {
                                    result[2 * ith + 1][2 * jth + 2] = true;
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        });
    });
    result
}

#[allow(dead_code)]
fn put_doubled_map(doubled_map: &DoubledMapType) {
    for row in doubled_map.iter() {
        for entry in row {
            print!("{}", if *entry { '#' } else { ' ' });
        }
        println!();
    }
}

fn part_2(map: &MapType, traversed_map: &TraversedMapType) -> usize {
    let mut doubled_map = double_map(map, traversed_map);
    // put_doubled_map(&doubled_map);
    // println!();
    flood_fill(&mut doubled_map);
    // put_doubled_map(&doubled_map);
    // println!();
    size_of_interior(&mut doubled_map)
}
