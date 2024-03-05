// Advent of Code 2023
//
// John Perry
//
// Day 18: Lavaduct Lagoon
//
// part 1: determine the size of the trench
//
// part 2: whoops, misread directions. lather, rinse, repeat
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use std::{
    collections::VecDeque,
    io::{BufRead, BufReader},
};

use common::{
    two_dimensional_map::{in_range, Location},
    two_dimensional_motion::{delta, ALL_DIRECTIONS},
};

fn main() {
    let result = read_input();
    // put_map(&result.map);
    println!(
        "the lagoon can hold {} cubic meters of lava",
        part_1(&result.map)
    );

    println!(
        "munching for real: {}",
        map_muncher(&result.true_directions)
    );
}

const DOING_EXAMPLE: bool = false;
const FILENAME: &str = if DOING_EXAMPLE {
    "example.txt"
} else {
    "input.txt"
};

type MapType = Vec<Vec<usize>>;

#[derive(Clone, Debug)]
struct RelativeLocation {
    row: isize,
    col: isize,
}

#[derive(Default)]
struct InitialData {
    map: MapType,
    true_directions: Vec<RelativeLocation>,
}

#[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
fn read_input() -> InitialData {
    let file = std::fs::File::open(FILENAME).expect("no input?!?");
    let lines = BufReader::new(file).lines();
    let mut result = InitialData::default();
    let (mut row, mut col) = (0_isize, 0_isize);
    let (mut top_row, mut bot_row, mut left_col, mut right_col) = (0, 0, 0, 0);
    for line in lines {
        let line = line.expect("how'd we get a line that isn't a line?");
        let mut split = line.split_ascii_whitespace();
        let (dir, dist) = (
            split.next().expect("no direction?!?"),
            split.next().expect("no distance?!"),
        );
        let dist: isize = dist
            .parse()
            .unwrap_or_else(|_| panic!("our distance {dist} was not a number!"));
        match dir {
            "U" => row -= dist,
            "D" => row += dist,
            "L" => col -= dist,
            "R" => col += dist,
            _ => panic!("ARGHACKLE received {dir}"),
        }
        top_row = top_row.min(row);
        bot_row = bot_row.max(row);
        left_col = left_col.min(col);
        right_col = right_col.max(col);
    }

    println!(
        "the map is {} x {} units wide",
        bot_row - top_row,
        right_col - left_col
    );
    println!("and ranges over {top_row}..{bot_row} - {left_col}..{right_col}");

    for _ in (top_row - 1)..=(bot_row + 1) {
        result.map.push(vec![
            255_usize.pow(3);
            ((right_col + 1) - (left_col - 1) + 1) as usize
        ]);
    }

    let file = std::fs::File::open(FILENAME).expect("no input?!?");
    let lines = BufReader::new(file).lines();
    let (mut row, mut col) = (1, 1);
    let (mut rel_row, mut rel_col) = (0, 0);
    for line in lines {
        let line = line.expect("what? no lines on the SECOND round?");
        let mut split = line.split_ascii_whitespace();
        let (dir, dist, color) = (
            split.next().unwrap(),
            split.next().unwrap(),
            split.next().unwrap(),
        );
        let dist: usize = dist
            .parse()
            .unwrap_or_else(|_| panic!("unable to parse {dist} as an integer!"));
        let color = &color[2..8];
        let color = usize::from_str_radix(color, 16)
            .unwrap_or_else(|_| panic!("unable to parse {color} as hex integer!"));
        for _ in 0..dist {
            match dir {
                "U" => row -= 1,
                "D" => row += 1,
                "L" => col -= 1,
                "R" => col += 1,
                _ => panic!("wait, how'd we panic here on the SECOND pass?!?"),
            }
            result.map[(row - top_row) as usize][(col - left_col) as usize] = color;
        }

        // for part 2
        let (dist, dir) = (color as isize / 16, color % 16);
        match dir {
            3 => rel_row -= dist, // up
            1 => rel_row += dist, // dn
            2 => rel_col -= dist, // lft
            0 => rel_col += dist, // rgt
            _ => panic!("{}, from is not a valid direction!", color % 16),
        }
        result.true_directions.push(RelativeLocation {
            row: rel_row,
            col: rel_col,
        });
    }

    result
}

#[allow(dead_code)]     // useful for debugging
fn put_map(map: &MapType) {
    for row in map {
        for color in row {
            print!("{}", if *color == 0 { '.' } else { '#' });
        }
        println!();
    }
}

#[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
fn flood_fill(map: &mut MapType) {
    let mut to_do = VecDeque::new();
    to_do.push_back(Location::new(0, 0));
    let (num_rows, num_cols) = (map.len(), map[0].len());
    while !to_do.is_empty() {
        let curr = to_do
            .pop_front()
            .expect("how did we panic on popping a nonempty queue?!?");
        for dir in ALL_DIRECTIONS {
            if in_range(curr, dir, num_rows, num_cols) {
                let next = Location::new(
                    (curr.row() as isize + delta(dir).d_row() as isize) as usize,
                    (curr.col() as isize + delta(dir).d_col() as isize) as usize,
                );
                if map[next.row()][next.col()] == 255_usize.pow(3) {
                    map[next.row()][next.col()] = 0;
                    to_do.push_back(next);
                }
            }
        }
    }
}

fn part_1(map: &MapType) -> usize {
    let mut result = 0;
    let mut map = map.clone();

    flood_fill(&mut map);

    for row in map {
        for color in row {
            if color != 0 {
                result += 1;
            }
        }
    }

    result
}

#[derive(Debug, Default, Clone, Copy)]
struct TakeoutBox {
    min_row: isize,
    max_row: isize,
    min_col: isize,
    max_col: isize,
}

fn contains(takeout: &TakeoutBox, location: &RelativeLocation) -> bool {
    (takeout.min_row..=takeout.max_row).contains(&location.row)
        && (takeout.min_col..=takeout.max_col).contains(&location.col)
}

#[derive(Debug, Clone, Copy)]
enum MenuItem {
    Valid {
        first: usize,
        second: usize,
        third: usize,
        #[allow(dead_code)]     // keeping this for curiosity's sake
        fourth: usize,
        cutoff: TakeoutBox,
        area: usize,
    },
    Invalid,
}

#[allow(clippy::cast_abs_to_unsigned, clippy::similar_names)]
fn inquire_about(ith: usize, buffet: &Vec<RelativeLocation>) -> MenuItem {
    let hth = if ith == 0 { buffet.len() - 1 } else { ith - 1 };
    let jth = if ith == buffet.len() - 1 { 0 } else { ith + 1 };
    let kth = if jth == buffet.len() { 0 } else { jth + 1 };

    let mut cutoff = TakeoutBox::default();

    if buffet[ith].row == buffet[jth].row {
        cutoff.min_col = buffet[ith].col.min(buffet[jth].col);
        cutoff.max_col = buffet[ith].col.max(buffet[jth].col);

        if buffet[hth].row < buffet[ith].row
            && buffet[kth].row < buffet[ith].row
            && buffet[ith].col > buffet[jth].col
        {
            cutoff.max_row = buffet[ith].row;
            cutoff.min_row = buffet[hth].row.max(buffet[kth].row);
        } else if buffet[hth].row > buffet[ith].row
            && buffet[kth].row > buffet[ith].row
            && buffet[ith].col < buffet[jth].col
        {
            cutoff.min_row = buffet[ith].row;
            cutoff.max_row = buffet[hth].row.min(buffet[kth].row);
        } else {
            return MenuItem::Invalid;
        }

        if buffet.iter().enumerate().any(|(lth, item)| {
            lth != hth && lth != ith && lth != jth && lth != kth && contains(&cutoff, item)
        }) {
            MenuItem::Invalid
        } else {
            MenuItem::Valid {
                first: hth,
                second: ith,
                third: jth,
                fourth: kth,
                cutoff,
                area: ((cutoff.max_col - cutoff.min_col).abs() as usize + 1)
                    * (cutoff.max_row - cutoff.min_row).abs() as usize,
            }
        }
    } else {
        cutoff.min_row = buffet[ith].row.min(buffet[jth].row);
        cutoff.max_row = buffet[ith].row.max(buffet[jth].row);

        if buffet[hth].col < buffet[ith].col
            && buffet[kth].col < buffet[ith].col
            && buffet[ith].row < buffet[jth].row
        {
            cutoff.max_col = buffet[ith].col;
            cutoff.min_col = buffet[hth].col.max(buffet[kth].col);
        } else if buffet[hth].col > buffet[ith].col
            && buffet[kth].col > buffet[ith].col
            && buffet[ith].row > buffet[jth].row
        {
            cutoff.min_col = buffet[ith].col;
            cutoff.max_col = buffet[hth].col.min(buffet[kth].col);
        } else {
            return MenuItem::Invalid;
        }

        if buffet.iter().enumerate().any(|(lth, item)| {
            lth != hth && lth != ith && lth != jth && lth != kth && contains(&cutoff, item)
        }) {
            MenuItem::Invalid
        } else {
            MenuItem::Valid {
                first: hth,
                second: ith,
                third: jth,
                fourth: kth,
                cutoff,
                area: (cutoff.max_col - cutoff.min_col).abs() as usize
                    * ((cutoff.max_row - cutoff.min_row).abs() as usize + 1),
            }
        }
    }
}

fn munch(item: MenuItem, buffet: &mut [RelativeLocation]) {
    if let MenuItem::Valid {
        first,
        second,
        third,
        cutoff,
        ..
    } = item
    {
        if buffet[first].row > buffet[second].row {
            buffet[second].row = cutoff.max_row;
            buffet[third].row = cutoff.max_row;
        } else if buffet[first].row < buffet[second].row {
            buffet[second].row = cutoff.min_row;
            buffet[third].row = cutoff.min_row;
        } else if buffet[first].col > buffet[second].col {
            buffet[second].col = cutoff.max_col;
            buffet[third].col = cutoff.max_col;
        } else {
            buffet[second].col = cutoff.min_col;
            buffet[third].col = cutoff.min_col;
        }
    } else {
        panic!("tried to munch an invalid menu item {item:#?}");
    }
}

    #[allow(clippy::similar_names)]
fn clean_place(buffet: &mut Vec<RelativeLocation>) {
    let mut jth = 0;
    loop {
        let mut changed = false;
        while jth < buffet.len() {
            let ith = if jth == 0 { buffet.len() - 1 } else { jth - 1 };
            let kth = if jth == buffet.len() - 1 { 0 } else { jth + 1 };

            if buffet[ith].row == buffet[jth].row && buffet[jth].row == buffet[kth].row {
                buffet.remove(jth);
                changed = true;
                break;
            }
            if buffet[ith].col == buffet[jth].col && buffet[jth].col == buffet[kth].col {
                buffet.remove(jth);
                changed = true;
                break;
            }

            jth += 1;
        }

        if !changed {
            break;
        }
    }
}

#[allow(clippy::cast_sign_loss)]
fn map_muncher(locations: &Vec<RelativeLocation>) -> usize {
    let mut result = 0;
    let mut buffet = (*locations).clone();

    while buffet.len() > 4 {
        for ith in 0..buffet.len() {
            let item = inquire_about(ith, &buffet);
            if let MenuItem::Valid { area, .. } = item {
                munch(item, &mut buffet);
                result += area;
                break;
            }
        }

        if buffet.len() > 4 {
            clean_place(&mut buffet);
        }
    }

    result += if buffet[0].row == buffet[3].row {
        ((buffet[3].col - buffet[0].col) as usize + 1)
            * ((buffet[1].row - buffet[0].row) as usize + 1)
    } else {
        ((buffet[3].row - buffet[0].row) as usize + 1)
            * ((buffet[1].col - buffet[0].col) as usize + 1)
    };

    result
}
