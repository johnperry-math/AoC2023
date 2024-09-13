//  Advent of Code 2023
//
//  John Perry
//
//  Day 25: Sand slabs
//
//  part 1: which three wires must you disconnect to form two distinct circuits
//
//  part 2: the usual freebie
//
// this basically translates the Ada code;
// see that for problem-specific details

use std::collections::{HashMap, HashSet, VecDeque};

use rand::Rng;

type LabelString = [char; 3];

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct EdgeRecord {
    one: LabelString,
    tother: LabelString,
}

impl EdgeRecord {
    fn new(first: LabelString, second: LabelString) -> Self {
        Self {
            one: first.min(second),
            tother: second.max(first),
        }
    }
}

impl std::fmt::Display for EdgeRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}~{}{}{}",
            self.one[0], self.one[1], self.one[2], self.tother[0], self.tother[1], self.tother[2]
        )
    }
}

type Graph = HashMap<LabelString, Vec<EdgeRecord>>;

fn read_input() -> Graph {
    use std::io::BufRead;

    let mut result = HashMap::new();

    let file = std::fs::File::open("input.txt").expect("where's my input?!?");
    let lines = std::io::BufReader::new(file).lines();
    for line in lines {
        let line = line.expect("how'd i geta line that's not a line?!?");
        let mut chars = line.chars();
        let key: LabelString = [
            chars.next().expect("missing first key character"),
            chars.next().expect("missing second key character"),
            chars.next().expect("missing third key character"),
        ];
        result.entry(key).or_insert_with(Vec::new);
        // skip ":"
        let _colon = chars.next();
        while let Some(_space) = chars.next() {
            let value: LabelString = [
                chars.next().expect("missing first value character"),
                chars.next().expect("missing second value character"),
                chars.next().expect("missing third value character"),
            ];
            result.entry(value).or_insert_with(Vec::new);
            let edge = EdgeRecord::new(key, value);
            result
                .get_mut(&key)
                .expect("how did we fail to get a key we know is there?!?")
                .push(edge);
            result
                .get_mut(&value)
                .expect("how did we fail to get a value key we know is there?!?")
                .push(edge);
        }
    }

    result
}

fn find_path(source: LabelString, dest: LabelString, graph: &Graph) -> Vec<EdgeRecord> {
    let mut queue = VecDeque::new();
    queue.push_back(vec![source]);
    let mut shortest_distances = HashMap::new();
    while !queue.is_empty() {
        let path = queue
            .pop_front()
            .expect("how'd we fail to pop a nonempty queue?!?");
        let last_element = path[path.len() - 1];
        if last_element == dest {
            let mut result = Vec::new();
            for ith in 0..path.len() - 1 {
                result.push(EdgeRecord::new(path[ith], path[ith + 1]));
            }
            return result;
        }
        // println!("checking {last_element:?} against {dest:?}");
        for edge in &graph[&last_element] {
            let route = if edge.one == last_element {
                edge.tother
            } else {
                edge.one
            };
            // println!("\t{edge:?}: {route:?}");
            if !path.contains(&route)
                && (!shortest_distances.contains_key(&route)
                    || shortest_distances[&route] > path.len())
            {
                shortest_distances
                    .entry(route)
                    .and_modify(|value| *value = path.len())
                    .or_insert(path.len());
                let mut new_path = path.clone();
                new_path.push(route);
                queue.push_back(new_path);
            }
        }
    }
    Vec::new()
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Endpoints {
    start: LabelString,
    finish: LabelString,
}

fn make_the_cut(graph: &mut Graph) {
    use rand::thread_rng;
    let mut rng = thread_rng();

    let num_vertices = graph.len();

    for _ in 0..=2 {
        let mut searched_paths = HashSet::new();
        let mut edge_importance = HashMap::new();
        for edge_list in graph.iter() {
            for edge in edge_list.1 {
                if !edge_importance.contains_key(edge) {
                    edge_importance.insert(edge, 0);
                }
            }
        }
        for _ in 0..100 {
            let edge = loop {
                let ith = rng.gen_range(0..num_vertices);
                let mut jth = ith;
                while ith == jth {
                    jth = rng.gen_range(0..num_vertices);
                }

                let cursor = graph.iter().nth(ith).expect("whoah, nothing here?!?");
                let source = cursor.0;

                let cursor = graph.iter().nth(jth).expect("whoah, nothing here?!?");
                let dest = cursor.0;

                let edge = Endpoints {
                    start: *source,
                    finish: *dest,
                };
                if !searched_paths.contains(&edge) {
                    break edge;
                }
            };
            let Endpoints {
                start: source,
                finish: dest,
            } = edge;
            searched_paths.insert(edge);
            for edge in find_path(source, dest, graph) {
                *edge_importance
                    .get_mut(&edge)
                    .expect("this really should be here...") += 1;
            }
        }

        let removed_edge = edge_importance
            .iter()
            .reduce(|result, entry| if entry.1 > result.1 { entry } else { result })
            .expect("how did we get an empty map?!?");
        println!("removing {} with weight {}", removed_edge.0, removed_edge.1);
        let removed_edge = **removed_edge.0;
        edge_importance.remove(&removed_edge);
        for each in graph.values_mut() {
            if let Some(ith) = each.iter().position(|value| *value == removed_edge) {
                each.remove(ith);
            }
        }
    }
}

fn part_1(graph: &mut Graph) -> usize {
    make_the_cut(graph);

    let mut group_one = HashSet::new();
    let mut group_two = HashSet::new();

    let all_labels = graph.keys();

    let source = all_labels
        .clone()
        .take(1)
        .next()
        .expect("how is the graph empty?!?");
    group_one.insert(source);
    for dest in all_labels.filter(|label| *label != source) {
        if !find_path(*source, *dest, graph).is_empty() {
            group_one.insert(dest);
        } else {
            group_two.insert(dest);
        }
    }

    println!("lengths: {} {}", group_one.len(), group_two.len());
    group_one.len() * group_two.len()
}

fn main() {
    let mut graph = read_input();
    println!("product is {}", part_1(&mut graph));
}
