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

use std::{
    collections::{BTreeMap, VecDeque},
    ops::RangeInclusive,
};

use common::{
    two_dimensional_map::{in_range, Location, Map},
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct ForkPath {
    start: Location,
    stop: Location,
}

// impl Ord for ForkPath {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         use std::cmp;
//         if self.start == other.start && self.stop == other.stop {
//             cmp::Ordering::Equal
//         } else if self.start.row() < other.start.row()
//             || (self.start.row() == other.start.row() && self.start.col() < other.start.col())
//             || (self.start == other.start && self.stop.row() < other.stop.row())
//             || (self.start == other.start
//                 && self.stop.row() == other.stop.row()
//                 && self.stop.col() < other.stop.col())
//         {
//             cmp::Ordering::Less
//         } else {
//             cmp::Ordering::Greater
//         }
//     }

//     fn max(self, other: Self) -> Self
//     where
//         Self: Sized,
//     {
//         std::cmp::max_by(self, other, Ord::cmp)
//     }

//     fn min(self, other: Self) -> Self
//     where
//         Self: Sized,
//     {
//         std::cmp::min_by(self, other, Ord::cmp)
//     }

//     fn clamp(self, min: Self, max: Self) -> Self
//     where
//         Self: Sized + PartialOrd,
//     {
//         assert!(min <= max);
//         self.clamp(min, max)
//     }
// }

// impl PartialOrd for ForkPath {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.cmp(other))
//     }
// }

fn find_forks(map: &Map<SIDE_LENGTH, SIDE_LENGTH, Object>, all_forks: &mut Vec<Location>) {
    all_forks.push(GOIN);
    all_forks.push(GOAL);

    for row in SIDE_RANGE {
        for col in SIDE_RANGE {
            let location = Location::new(row, col);
            if map[location] != Object::Forest {
                let mut neighbors = 0;
                for dir in ALL_DIRECTIONS {
                    if in_range(location, dir, SIDE_LENGTH, SIDE_LENGTH)
                        && map[location.delta(dir)] != Object::Forest
                    {
                        neighbors += 1;
                    }
                }
                if neighbors > 2 {
                    all_forks.push(location);
                }
            }
        }
    }
}

fn map_forks(
    map: &Map<SIDE_LENGTH, SIDE_LENGTH, Object>,
    all_forks: &[Location],
    fork_path_lengths: &mut BTreeMap<ForkPath, usize>,
) {
    for start in all_forks {
        for dir in ALL_DIRECTIONS {
            if in_range(*start, dir, SIDE_LENGTH, SIDE_LENGTH)
                && map[start.delta(dir)] != Object::Forest
            {
                let mut steps = 1;
                let mut prev = *start;
                let mut curr = start.delta(dir);
                let mut next = curr;

                while !all_forks.contains(&curr) {
                    for dir in ALL_DIRECTIONS {
                        if in_range(curr, dir, SIDE_LENGTH, SIDE_LENGTH)
                            && map[curr.delta(dir)] != Object::Forest
                        {
                            next = curr.delta(dir);
                            if next != prev {
                                prev = curr;
                                curr = next;
                                steps += 1;
                                break;
                            }
                        }
                    }
                }

                fork_path_lengths.insert(
                    ForkPath {
                        start: *start,
                        stop: next,
                    },
                    steps,
                );
            }
        }
    }
}

fn path_length(path: &[Location], fork_path_lengths: &BTreeMap<ForkPath, usize>) -> usize {
    path.iter()
        .take(path.len() - 1)
        .zip(path.iter().skip(1))
        .fold(0, |result, (start, stop)| {
            result
                + fork_path_lengths[&ForkPath {
                    start: *start,
                    stop: *stop,
                }]
        })
}

fn part_2(all_forks: &[Location], fork_path_lengths: &BTreeMap<ForkPath, usize>) -> usize {
    let mut best = Vec::default();
    let mut last_length = 0;

    let mut queue = VecDeque::from(vec![vec![Location::new(0, 1)]]);

    while !queue.is_empty() {
        let curr = queue
            .pop_front()
            .expect("how did we fail to pop from a nonempty queue?!?");

        if curr.len() > last_length {
            last_length = curr.len();
            println!("path length is now {}", curr.len());
            println!("paths in queue: {}", queue.len());
        }

        let last_location = *curr
            .last()
            .expect("how did a nonempty vector become empty?!?");
        if last_location == GOAL
            && path_length(&curr, fork_path_lengths) > path_length(&best, fork_path_lengths)
        {
            best = curr;
        } else {
            for fork in all_forks {
                if !curr.contains(fork)
                    && (fork_path_lengths.contains_key(&ForkPath {
                        start: last_location,
                        stop: *fork,
                    }) || fork_path_lengths.contains_key(&ForkPath {
                        start: *fork,
                        stop: last_location,
                    }))
                {
                    let mut next = curr.clone();
                    next.push(*fork);
                    queue.push_back(next);
                }
            }
        }
    }

    path_length(&best, fork_path_lengths)
}

fn main() {
    let map = read_input("input.txt".to_string(), &deserialize);
    println!("the most scenic route's length is {} steps", part_1(&map));
    let mut all_forks = Vec::default();
    find_forks(&map, &mut all_forks);
    let mut fork_path_lengths = BTreeMap::default();
    map_forks(&map, &all_forks, &mut fork_path_lengths);
    println!(
        "well, if i can climb slopes, then it's {} steps",
        part_2(&all_forks, &fork_path_lengths)
    );
}
