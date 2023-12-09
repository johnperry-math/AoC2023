use std::{
    cmp::Ordering,
    collections::HashMap,
    io::{BufRead, BufReader},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Face {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Hand {
    cards: [Face; 5],
    bid: usize,
}

impl From<char> for Face {
    fn from(value: char) -> Self {
        match value {
            '1' => Face::One,
            '2' => Face::Two,
            '3' => Face::Three,
            '4' => Face::Four,
            '5' => Face::Five,
            '6' => Face::Six,
            '7' => Face::Seven,
            '8' => Face::Eight,
            '9' => Face::Nine,
            'T' => Face::Ten,
            'J' => Face::Jack,
            'Q' => Face::Queen,
            'K' => Face::King,
            'A' => Face::Ace,
            _ => panic!("received an invalid card {value}"),
        }
    }
}

impl From<String> for Hand {
    fn from(value: String) -> Self {
        let mut split = value.split_ascii_whitespace();
        let mut cards = [Face::One; 5];
        split
            .next()
            .expect("no string?!?")
            .chars()
            .take(5)
            .map(|c| c.into())
            .enumerate()
            .for_each(|(ith, face)| cards[ith] = face);
        let bid = split
            .next()
            .expect("no bid?!?")
            .parse::<usize>()
            .expect("unable to parse integer!");
        Hand { cards, bid }
    }
}

#[derive(PartialEq, PartialOrd)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn main() {
    let mut all_hands = read_input();
    println!("the hands' total winnings are {}", part_1(&mut all_hands));
    println!(
        "with the new rule, the hands' total winnings are {}",
        part_2(&mut all_hands)
    );
}

fn read_input() -> Vec<Hand> {
    let input = std::fs::File::open("input.txt").expect("what, where's input.txt?!?");
    BufReader::new(input)
        .lines()
        .map(|line| {
            let line = line.expect("how'd we get a line that's not a line?!?");
            Hand::from(line)
        })
        .collect()
}

fn rank(hand: &Hand) -> HandType {
    let mut counts = HashMap::<Face, usize>::default();
    let (mut most_identical, mut second_most_identical) = (1, 1);
    for face in hand.cards {
        *counts.entry(face).or_insert(0) += 1;
    }
    for count in counts.values() {
        match count {
            5 | 4 => most_identical = *count,
            3 => {
                if most_identical == 2 {
                    second_most_identical = 2;
                }
                most_identical = 3;
            }
            2 => {
                if most_identical < 2 {
                    most_identical = 2;
                } else {
                    second_most_identical = 2;
                }
            }
            _ => {}
        }
    }
    match most_identical {
        1 => HandType::HighCard,
        2 => {
            if second_most_identical == 1 {
                HandType::OnePair
            } else {
                HandType::TwoPair
            }
        }
        3 => {
            if second_most_identical == 1 {
                HandType::ThreeOfAKind
            } else {
                HandType::FullHouse
            }
        }
        4 => HandType::FourOfAKind,
        5 => HandType::FiveOfAKind,
        _ => panic!("somehow had more than 5 identical cards..."),
    }
}

fn first_compare(left: &Hand, right: &Hand) -> Ordering {
    let left_rank = rank(left);
    let right_rank = rank(right);
    if left_rank < right_rank {
        Ordering::Less
    } else if left_rank > right_rank {
        Ordering::Greater
    } else {
        left.cards
            .iter()
            .zip(right.cards)
            .find(|(left, right)| **left != *right)
            .map(|(left, right)| left.cmp(&right))
            .unwrap()
    }
}

fn part_1(all_hands: &mut [Hand]) -> usize {
    all_hands.sort_by(first_compare);
    // ARGH ith + 1
    all_hands
        .iter()
        .enumerate()
        .map(|(ith, hand)| (ith + 1) * hand.bid)
        .sum()
}

fn rank_with_joker(hand: &Hand) -> HandType {
    let mut counts = HashMap::<Face, usize>::default();
    let (mut most_identical, mut second_most_identical) = (1, 1);
    let mut num_jokers = 0;
    for face in hand.cards {
        if face == Face::Jack {
            num_jokers += 1;
        } else {
            *counts.entry(face).or_insert(0) += 1;
        }
    }
    for count in counts.values() {
        match count {
            5 | 4 => most_identical = *count,
            3 => {
                if most_identical == 2 {
                    second_most_identical = 2;
                }
                most_identical = 3;
            }
            2 => {
                if most_identical < 2 {
                    most_identical = 2;
                } else {
                    second_most_identical = 2;
                }
            }
            _ => {}
        }
    }
    if num_jokers > 0 {
        match num_jokers {
            5 | 4 => most_identical = 5,
            3 => {
                if most_identical == 2 {
                    most_identical = 5;
                } else {
                    most_identical = 4;
                }
            }
            2 => {
                if most_identical == 3 {
                    most_identical = 5;
                } else if most_identical == 2 {
                    most_identical = 4;
                } else {
                    most_identical = 3;
                }
            }
            _ => most_identical += 1,
        }
    }
    match most_identical {
        1 => HandType::HighCard,
        2 => {
            if second_most_identical == 1 {
                HandType::OnePair
            } else {
                HandType::TwoPair
            }
        }
        3 => {
            if second_most_identical == 1 {
                HandType::ThreeOfAKind
            } else {
                HandType::FullHouse
            }
        }
        4 => HandType::FourOfAKind,
        5 => HandType::FiveOfAKind,
        _ => panic!("somehow had more than 5 identical cards..."),
    }
}

fn new_ranking(face: &Face) -> usize {
    match face {
        Face::One => 1,
        Face::Two => 2,
        Face::Three => 3,
        Face::Four => 4,
        Face::Five => 5,
        Face::Six => 6,
        Face::Seven => 7,
        Face::Eight => 8,
        Face::Nine => 9,
        Face::Ten => 10,
        Face::Jack => 0,
        Face::Queen => 11,
        Face::King => 12,
        Face::Ace => 13,
    }
}

fn second_compare(left: &Hand, right: &Hand) -> Ordering {
    let left_rank = rank_with_joker(left);
    let right_rank = rank_with_joker(right);

    if left_rank < right_rank {
        Ordering::Less
    } else if left_rank > right_rank {
        Ordering::Greater
    } else {
        left.cards
            .iter()
            .map(new_ranking)
            .zip(right.cards.iter().map(new_ranking))
            .find(|(left, right)| *left != *right)
            .map(|(left, right)| left.cmp(&right))
            .unwrap()
    }
}

fn part_2(all_hands: &mut [Hand]) -> usize {
    all_hands.sort_by(second_compare);
    all_hands
        .iter()
        .enumerate()
        .map(|(ith, hand)| (ith + 1) * hand.bid)
        .sum()
}
