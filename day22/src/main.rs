//  Advent of Code 2023
//
//  John Perry
//
//  Day 22: Sand slabs
//
//  part 1: how many sand slabs can we disintegrate without making others fall?
//
//  part 2: find the sum of the slabs that will fall when we delete each slab
//
// this basically translates the Ada code;
// see that for problem-specific details

#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

fn main() {
    let InputData {
        maxs: max_vals,
        mut bricks,
    } = read_input();
    bricks.sort();

    println!(
        "we can disintegrate {} bricks",
        part_1(&mut bricks, &max_vals)
    );
    println!(
        "sum of bricks that would fall is {}",
        part_2(&mut bricks, &max_vals)
    );
}

#[derive(Default)]
struct Maxs {
    x: usize,
    y: usize,
    z: usize,
}

impl Maxs {
    fn expand_to(&mut self, x: usize, y: usize, z: usize) {
        self.x = self.x.max(x);
        self.y = self.y.max(y);
        self.z = self.z.max(z);
    }
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
struct Location {
    x: usize,
    y: usize,
    z: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord)]
struct Brick {
    first: Location,
    second: Location,
}

impl std::fmt::Display for Brick {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{},{}~{},{},{}",
            self.first.x, self.first.y, self.first.z, self.second.x, self.second.y, self.second.z,
        )
    }
}

impl PartialOrd for Brick {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::{Equal, Greater, Less};
        if (self.first.z < other.first.z)
            || (self.first.z == other.first.z && self.first.x < other.first.x)
            || (self.first.z == other.first.z
                && self.first.x == other.first.x
                && self.first.y < other.first.y)
        {
            Some(Less)
        } else if self.first.x == other.first.x
            && self.first.y == other.first.y
            && self.first.z == other.first.z
        {
            Some(Equal)
        } else {
            Some(Greater)
        }
    }
}

impl Brick {
    fn new(first: Location, second: Location) -> Self {
        Self {
            first: first.min(second),
            second: second.max(first),
        }
    }
}

struct InputData {
    maxs: Maxs,
    bricks: Vec<Brick>,
}

fn read_input() -> InputData {
    use std::io::BufRead;

    let mut maxs = Maxs::default();
    let mut bricks = Vec::new();

    let file = std::fs::File::open("input.txt").expect("where's my input?!?");
    let lines = std::io::BufReader::new(file).lines();
    for line in lines {
        let line = line.expect("We read a line without reading it?!?");
        let mut split = line.split('~');

        let first = split.next().expect("we didn't find a first brick...");
        let second = split.next().expect("we didn't find a second brick...");
        let (mut first, mut second) = (first.split(','), second.split(','));

        let (x, y, z) = (
            first.next().expect("no first x coordinate?!?"),
            first.next().expect("no first y coordinate?!?"),
            first.next().expect("no first z coordinate?!?"),
        );
        let (x, y, z) = (
            x.parse::<usize>()
                .expect("unable to make a number from {x}"),
            y.parse::<usize>()
                .expect("unable to make a number from {y}"),
            z.parse::<usize>()
                .expect("unable to make a number from {z}"),
        );
        maxs.expand_to(x, y, z);
        let first = Location { x, y, z };

        let (x, y, z) = (
            second.next().expect("no second x coordinate?!?"),
            second.next().expect("no second y coordinate?!?"),
            second.next().expect("no second z coordinate?!?"),
        );
        let (x, y, z) = (
            x.parse::<usize>()
                .expect("unable to make a number from {x}"),
            y.parse::<usize>()
                .expect("unable to make a number from {y}"),
            z.parse::<usize>()
                .expect("unable to make a number from {z}"),
        );
        maxs.expand_to(x, y, z);
        let second = Location { x, y, z };

        bricks.push(Brick::new(first, second));
    }

    InputData { maxs, bricks }
}

type Space = Vec<Vec<Vec<bool>>>;

fn new_space(maxs: &Maxs) -> Space {
    let mut space = Vec::new();
    for _row in 0..=maxs.x {
        let mut new_row = Vec::new();
        for _col in 0..=maxs.y {
            new_row.push(vec![false; maxs.z + 1]);
        }
        space.push(new_row);
    }
    space
}

fn is_blocked(brick: Brick, used: &Space) -> bool {
    let (min_x, max_x, min_y, max_y) = (
        brick.first.x.min(brick.second.x),
        brick.first.x.max(brick.second.x),
        brick.first.y.min(brick.second.y),
        brick.first.y.max(brick.second.y),
    );
    (min_x..=max_x).any(|x| (min_y..=max_y).any(|y| used[x][y][brick.first.z - 1]))
}

fn drop(brick: &mut Brick, used: &mut Space, dropped: &mut bool) {
    *dropped = false;

    let mut falling = true;
    while falling {
        if brick.first.z == 1 || is_blocked(*brick, used) {
            falling = false;
        } else {
            brick.first.z -= 1;
            brick.second.z -= 1;
            *dropped = true;
        }
    }

    for x in brick.first.x..=brick.second.x {
        for y in brick.first.y..=brick.second.y {
            for z in brick.first.z..=brick.second.z {
                used[x][y][z] = true;
            }
        }
    }
}

fn fall_in(this: &mut Space, bricks: &mut Vec<Brick>, num_dropped: &mut usize) {
    *num_dropped = 0;
    for brick in bricks {
        let mut dropped = false;
        drop(brick, this, &mut dropped);
        if dropped {
            *num_dropped += 1;
        }
    }
}

fn can_remove(ith: usize, bricks: &Vec<Brick>, used: &Space) -> bool {
    let brick = &bricks[ith];

    let min_x = brick.first.x.min(brick.second.x);
    let max_x = brick.first.x.max(brick.second.x);
    let min_y = brick.first.y.min(brick.second.y);
    let max_y = brick.first.y.max(brick.second.y);
    let z = brick.second.z;

    let mut removable = true;

    let mut scratch = used.clone();

    for x in min_x..=max_x {
        for y in min_y..=max_y {
            for z in brick.first.z..=brick.second.z {
                scratch[x][y][z] = false;
            }
        }
    }

    for jth in ith + 1..bricks.len() {
        if removable && bricks[jth].first.z <= z + 1 {
            if bricks[jth].first.z == z + 1 && !is_blocked(bricks[jth], &scratch) {
                removable = false;
            }
        }
    }

    removable
}

fn part_1(bricks: &mut Vec<Brick>, maxs: &Maxs) -> usize {
    let mut result = 0;
    let mut used = new_space(maxs);

    let mut num_dropped = 0;
    fall_in(&mut used, bricks, &mut num_dropped);
    println!("we dropped {num_dropped}!");

    bricks.sort();
    for ith in 0..bricks.len() {
        if can_remove(ith, bricks, &used) {
            result += 1;
        }
    }
    result
}

fn part_2(bricks: &mut Vec<Brick>, maxs: &Maxs) -> usize {
    let mut result = 0;
    let mut num_dropped: usize = 0;

    for ith in 0..bricks.len() {
        let mut scratch = new_space(maxs);
        let mut brick_scratch = bricks.clone();
        brick_scratch.remove(ith);
        fall_in(&mut scratch, &mut brick_scratch, &mut num_dropped);
        result += num_dropped;
    }

    result
}
