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

use std::collections::BTreeSet;

use malachite::Rational;

const PART_1_MIN: f64 = 200_000_000_000_000.0;
const PART_1_MAX: f64 = 400_000_000_000_000.0;

#[derive(Clone)]
struct HailstoneRecord {
    x: Rational,
    y: Rational,
    z: Rational,
    dx: Rational,
    dy: Rational,
    dz: Rational,
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
            x: Rational::from(value.x),
            y: Rational::from(value.y),
            z: Rational::from(value.z),
            dx: Rational::from(value.dx),
            dy: Rational::from(value.dy),
            dz: Rational::from(value.dz),
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
    use malachite::num::arithmetic::traits::Abs;
    let m_ith = first.dy.clone() / first.dx.clone();
    let m_jth = second.dy.clone() / second.dx.clone();

    if (m_ith.clone() - m_jth.clone()).abs() > 0.000_000_000_01 {
        let x = ((second.y.clone() - first.y.clone())
            + (m_ith.clone() * first.x.clone() - m_jth.clone() * second.x.clone()))
            / (m_ith.clone() - m_jth.clone());
        if (PART_1_MIN..PART_1_MAX).contains(&x)
            && (x.clone() - first.x.clone()) / first.dx.clone() > 0.0
        {
            let y = m_ith.clone() * (x.clone() - first.x.clone()) + first.y.clone();
            if (PART_1_MIN..PART_1_MAX).contains(&y)
                && (y - second.y.clone()) / (second.dy.clone()) > 0.0
            {
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

fn part_2_by_gb(all_hailstones: &[HailstoneRecord]) -> Rational {
    use malachite::rounding_modes::RoundingMode;
    let [first, second, third] = find_triplet(all_hailstones);
    println!(
        "{} * {} = {}!!!",
        <malachite::Rational as malachite::num::conversion::traits::RoundingInto<f64>>::rounding_into(second.x.clone(), RoundingMode::Nearest)
        .0,
        <malachite::Rational as malachite::num::conversion::traits::RoundingInto<f64>>::rounding_into(second.dy.clone(), RoundingMode::Nearest)
        .0,
        <malachite::Rational as malachite::num::conversion::traits::RoundingInto<f64>>::rounding_into(second.x.clone(), RoundingMode::Nearest)
        .0 * <malachite::Rational as malachite::num::conversion::traits::RoundingInto<f64>>::rounding_into(second.dy.clone(), RoundingMode::Nearest)
        .0
    );

    let mut matrix = [
        [
            first.y.clone() - second.y.clone(),
            second.x.clone() - first.x.clone(),
            Rational::from(0),
            second.dy.clone() - first.dy.clone(),
            first.dx.clone() - second.dx.clone(),
            Rational::from(0),
            -(first.x.clone() * first.dy.clone()
                - first.y.clone() * first.dx.clone()
                - second.x.clone() * second.dy.clone()
                + second.y.clone() * second.dx.clone()),
        ],
        [
            first.z.clone() - second.z.clone(),
            Rational::from(0),
            second.x.clone() - first.x.clone(),
            second.dz.clone() - first.dz.clone(),
            Rational::from(0),
            first.dx.clone() - second.dx.clone(),
            -(first.x.clone() * first.dz.clone()
                - first.z.clone() * first.dx.clone()
                - second.x.clone() * second.dz.clone()
                + second.z.clone() * second.dx.clone()),
        ],
        [
            Rational::from(0),
            first.z.clone() - second.z.clone(),
            second.y.clone() - first.y.clone(),
            Rational::from(0),
            second.dz.clone() - first.dz.clone(),
            first.dy.clone() - second.dy.clone(),
            -(first.y.clone() * first.dz.clone()
                - first.z.clone() * first.dy.clone()
                - second.y.clone() * second.dz.clone()
                + second.z.clone() * second.dy.clone()),
        ],
        [
            first.y.clone() - third.y.clone(),
            third.x.clone() - first.x.clone(),
            Rational::from(0),
            third.dy.clone() - first.dy.clone(),
            first.dx.clone() - third.dx.clone(),
            Rational::from(0),
            -(first.x.clone() * first.dy.clone()
                - first.y.clone() * first.dx.clone()
                - third.x.clone() * third.dy.clone()
                + third.y.clone() * third.dx.clone()),
        ],
        [
            first.z.clone() - third.z.clone(),
            Rational::from(0),
            third.x.clone() - first.x.clone(),
            third.dz.clone() - first.dz.clone(),
            Rational::from(0),
            first.dx.clone() - third.dx.clone(),
            -(first.x.clone() * first.dz.clone()
                - first.z.clone() * first.dx.clone()
                - third.x * third.dz.clone()
                + third.z.clone() * third.dx.clone()),
        ],
        [
            Rational::from(0),
            first.z.clone() - third.z.clone(),
            third.y.clone() - first.y.clone(),
            Rational::from(0),
            third.dz.clone() - first.dz.clone(),
            first.dy.clone() - third.dy.clone(),
            -(first.y.clone() * first.dz.clone()
                - first.z.clone() * first.dy.clone()
                - third.y.clone() * third.dz
                + third.z.clone() * third.dy.clone()),
        ],
    ];

    for pivot in 0..=4 {
        if matrix[pivot][pivot] == 0.0 {
            for col in pivot..=6 {
                let pivoter = matrix[pivot][col].clone();
                matrix[pivot][col] = matrix[pivot + 1][col].clone();
                matrix[pivot + 1][col] = pivoter;
            }
        }
        for row in pivot + 1..=5 {
            let pivoter = matrix[row][pivot].clone();
            for col in pivot..=6 {
                matrix[row][col] -=
                    pivoter.clone() / matrix[pivot][pivot].clone() * matrix[pivot][col].clone();
            }
        }
    }

    for row in 3..=4 {
        let pivoter = matrix[row][5].clone();
        for col in 5..=6 {
            matrix[row][col] -= pivoter.clone() / matrix[5][5].clone() * matrix[5][col].clone();
        }
    }
    let pivoter = matrix[3][4].clone();
    for col in 4..=6 {
        matrix[3][col] -= pivoter.clone() / matrix[4][4].clone() * matrix[4][col].clone();
    }

    let x = matrix[3][6].clone() / matrix[3][3].clone();
    let y = matrix[4][6].clone() / matrix[4][4].clone();
    let z = matrix[5][6].clone() / matrix[5][5].clone();

    x + y + z
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
