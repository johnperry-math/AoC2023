//  Advent of Code 2023
//
//  John Perry
//
//  Day 24: Never Tell Me The Odds
//
//  part 1: how many hailstones intersect, in a certain area, in the future?
//
//  part 2: from what position can you throw a stone
//          so that it hits every hailstone?
//
// this basically translates the Ada code;
// see that for problem-specific details

#![feature(f128)]

use std::collections::BTreeSet;

const PART_1_MIN: f128 = 200_000_000_000_000.0;
const PART_1_MAX: f128 = 400_000_000_000_000.0;

#[derive(Clone)]
struct HailstoneRecord {
    x: f128,
    y: f128,
    z: f128,
    dx: f128,
    dy: f128,
    dz: f128,
}

#[derive(PartialEq, PartialOrd, Ord, Eq)]
struct LudicrousStoneRecord {
    x: i64,
    y: i64,
    z: i64,
    dx: i64,
    dy: i64,
    dz: i64,
}

impl From<&LudicrousStoneRecord> for HailstoneRecord {
    fn from(value: &LudicrousStoneRecord) -> Self {
        Self {
            x: value.x as f128,
            y: value.y as f128,
            z: value.z as f128,
            dx: value.dx as f128,
            dy: value.dy as f128,
            dz: value.dz as f128,
        }
    }
}

fn read_input() -> Vec<LudicrousStoneRecord> {
    use std::io::BufRead;
    let mut result = Vec::new();
    let file = std::fs::File::open("input.txt").expect("where's my input?!?");
    let lines = std::io::BufReader::new(file).lines();
    for line in lines {
        let line = line.expect("how'd we get a line that isn't a line?!?");
        let mut pos_then_vel = line.split(" @ ");
        let positions = pos_then_vel
            .next()
            .expect("how'd we get a line without positions?");
        let velocities = pos_then_vel
            .next()
            .expect("how'd we get a line without velocities?!?");
        let mut positions = positions.split(", ");
        let mut velocities = velocities.split(", ");
        result.push(LudicrousStoneRecord {
            x: positions
                .next()
                .expect("how'd i get a position with no coordinate?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
            y: positions
                .next()
                .expect("how'd i get a position with no coordinate?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
            z: positions
                .next()
                .expect("how'd i get a position with no coordinate?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
            dx: velocities
                .next()
                .expect("how'd i get a velocity with no values?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
            dy: velocities
                .next()
                .expect("how'd i get a velocity with no values?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
            dz: velocities
                .next()
                .expect("how'd i get a velocity with no values?!?")
                .trim()
                .parse()
                .expect("somehow failed to parse the value!"),
        });
    }
    result
}

enum Intersection {
    Valid,
    Invalid,
}

fn intersect_in_future(first: &HailstoneRecord, second: &HailstoneRecord) -> Intersection {
    let m_ith = first.dy / first.dx;
    let m_jth = second.dy / second.dx;

    if (m_ith - m_jth).abs() > 0.000_000_000_01 {
        let x = ((second.y - first.y) + (m_ith * first.x - m_jth * second.x)) / (m_ith - m_jth);
        if (PART_1_MIN..PART_1_MAX).contains(&x) && (x - first.x) / first.dx > 0.0 {
            let y = m_ith * (x - first.x) + first.y;
            if (PART_1_MIN..PART_1_MAX).contains(&y) && (y - second.y) / (second.dy) > 0.0 {
                return Intersection::Valid;
            }
        }
    }

    Intersection::Invalid
}

fn part_1(all_hailstones: &[HailstoneRecord]) -> u64 {
    let mut result = 0;

    for (ith, first) in all_hailstones
        .iter()
        .enumerate()
        .take(all_hailstones.len() - 1)
    {
        for second in all_hailstones.iter().skip(ith) {
            if matches!(
                intersect_in_future(first, second),
                Intersection::Valid { .. }
            ) {
                result += 1;
            }
        }
    }

    result
}

fn part_2(all_hailstones: &mut [LudicrousStoneRecord]) -> i64 {
    all_hailstones.sort();

    let mut potential_x_set = BTreeSet::new();
    let mut potential_y_set = BTreeSet::new();
    let mut potential_z_set = BTreeSet::new();
    for (ith, first) in all_hailstones
        .iter()
        .enumerate()
        .take(all_hailstones.len() - 1)
    {
        for second in all_hailstones.iter().skip(ith) {
            if first.dx == second.dx && first.dx.abs() > 100 {
                let mut new_x_set = BTreeSet::new();
                let difference = second.x - first.x;
                for v in -1_000..1_000 {
                    if v != first.dx && difference % (v - first.dx) == 0 {
                        new_x_set.insert(v);
                    }
                }
                if potential_x_set.is_empty() {
                    potential_x_set = new_x_set;
                } else {
                    potential_x_set = potential_x_set.intersection(&new_x_set).copied().collect();
                }
            }
            if first.dy == second.dy && first.dy.abs() > 100 {
                let mut new_y_set = BTreeSet::new();
                let difference = second.y - first.y;
                for v in -1_000..1_000 {
                    if v != first.dy && difference % (v - first.dy) == 0 {
                        new_y_set.insert(v);
                    }
                }
                if potential_y_set.is_empty() {
                    potential_y_set = new_y_set;
                } else {
                    potential_y_set = potential_y_set.intersection(&new_y_set).copied().collect();
                }
            }
            if first.dz == second.dz && first.dz.abs() > 100 {
                let mut new_z_set = BTreeSet::new();
                let difference = second.z - first.z;
                for v in -1_000..1_000 {
                    if v != first.dz && difference % (v - first.dz) == 0 {
                        new_z_set.insert(v);
                    }
                }
                if potential_z_set.is_empty() {
                    potential_z_set = new_z_set;
                } else {
                    potential_z_set = potential_z_set.intersection(&new_z_set).copied().collect();
                }
            }
        }
    }

    assert_eq!(potential_x_set.len(), 1);
    assert_eq!(potential_y_set.len(), 1);
    assert_eq!(potential_z_set.len(), 1);

    let (dx, dy, dz) = (
        *potential_x_set.first().unwrap(),
        *potential_y_set.first().unwrap(),
        *potential_z_set.first().unwrap(),
    );
    let first = &all_hailstones[0];
    let second = &all_hailstones[1];

    let first_m = (first.dy - dy) as f64 / (first.dx - dx) as f64;
    let second_m = (second.dy - dy) as f64 / (second.dx - dx) as f64;
    let ca = first.y as f64 - first_m * first.x as f64;
    let cb = second.y as f64 - second_m * second.x as f64;
    let x_pos = ((cb - ca) / (first_m - second_m)) as i64;
    let y_pos = (first_m * x_pos as f64 + ca) as i64;
    let time = (x_pos - first.x) / (first.dx - dx);
    let z_pos = first.z + (first.dz - dz) * time;

    x_pos + y_pos + z_pos
}

fn find_triplet(all_hailstones: &[HailstoneRecord]) -> [HailstoneRecord; 3] {
    for (ith, first) in all_hailstones
        .iter()
        .enumerate()
        .take(all_hailstones.len() - 2)
    {
        for (jth, second) in all_hailstones.iter().enumerate().skip(ith) {
            if let Intersection::Valid { .. } = intersect_in_future(first, second) {
                for third in all_hailstones.iter().skip(jth) {
                    if let Intersection::Valid { .. } = intersect_in_future(second, third) {
                        return [first.clone(), second.clone(), third.clone()];
                    }
                }
            }
        }
    }
    panic!("this isn't supposed to happen!");
}

fn part_2_by_gb(all_hailstones: &[HailstoneRecord]) -> usize {
    let [first, second, third] = find_triplet(all_hailstones);
    println!(
        "{} * {} = {}!!!",
        second.x as i64,
        second.dy as i64,
        (second.x * second.dy) as i64
    );

    let mut matrix = [
        [
            first.y - second.y,
            second.x - first.x,
            0.0,
            second.dy - first.dy,
            first.dx - second.dx,
            0.0,
            -(first.x * first.dy - first.y * first.dx - second.x * second.dy
                + second.y * second.dx),
        ],
        [
            first.z - second.z,
            0.0,
            second.x - first.x,
            second.dz - first.dz,
            0.0,
            first.dx - second.dx,
            -(first.x * first.dz - first.z * first.dx - second.x * second.dz
                + second.z * second.dx),
        ],
        [
            0.0,
            first.z - second.z,
            second.y - first.y,
            0.0,
            second.dz - first.dz,
            first.dy - second.dy,
            -(first.y * first.dz - first.z * first.dy - second.y * second.dz
                + second.z * second.dy),
        ],
        [
            first.y - third.y,
            third.x - first.x,
            0.0,
            third.dy - first.dy,
            first.dx - third.dx,
            0.0,
            -(first.x * first.dy - first.y * first.dx - third.x * third.dy + third.y * third.dx),
        ],
        [
            first.z - third.z,
            0.0,
            third.x - first.x,
            third.dz - first.dz,
            0.0,
            first.dx - third.dx,
            -(first.x * first.dz - first.z * first.dx - third.x * third.dz + third.z * third.dx),
        ],
        [
            0.0,
            first.z - third.z,
            third.y - first.y,
            0.0,
            third.dz - first.dz,
            first.dy - third.dy,
            -(first.y * first.dz - first.z * first.dy - third.y * third.dz + third.z * third.dy),
        ],
    ];

    for pivot in 0..=4 {
        if matrix[pivot][pivot] == 0.0 {
            for col in pivot..=6 {
                let pivoter = matrix[pivot][col];
                matrix[pivot][col] = matrix[pivot + 1][col];
                matrix[pivot + 1][col] = pivoter;
            }
        }
        for row in pivot + 1..=5 {
            let pivoter = matrix[row][pivot];
            for col in pivot..=6 {
                matrix[row][col] -= pivoter / matrix[pivot][pivot] * matrix[pivot][col];
            }
        }
    }

    for row in 3..=4 {
        let pivoter = matrix[row][5];
        for col in 5..=6 {
            matrix[row][col] -= pivoter / matrix[5][5] * matrix[5][col];
        }
    }
    let pivoter = matrix[3][4];
    for col in 4..=6 {
        matrix[3][col] -= pivoter / matrix[4][4] * matrix[4][col];
    }

    let x = matrix[3][6] / matrix[3][3];
    let y = matrix[4][6] / matrix[4][4];
    let z = matrix[5][6] / matrix[5][5];

    (x + y + z) as usize
}

fn main() {
    let mut all_hailstones_int = read_input();
    let all_hailstones_flt: Vec<HailstoneRecord> = all_hailstones_int
        .iter()
        .map(HailstoneRecord::from)
        .collect();
    println!(
        "exactly {} intersections occur in test area",
        part_1(&all_hailstones_flt)
    );
    println!(
        "magical number via gb is {}",
        part_2_by_gb(&all_hailstones_flt)
    );
    println!(
        "magical number via magic is {}",
        part_2(&mut all_hailstones_int)
    );
}
