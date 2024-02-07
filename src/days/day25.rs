use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_alphanumeric, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::Args;
use itertools::Itertools;
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{
    cell::LazyCell,
    collections::{HashMap, HashSet, VecDeque},
    iter::once,
    rc::Rc,
};

pub const DAY_25: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day25>::new(
            "day25",
            "Finds the product of the two subgraphs after cutting edges",
            "The undirected edges for each graph node",
        )
        .with_part(
            "Cuts 3 edges to find the sub graphs",
            CommandLineArguments {},
        ),
    )
});

struct Input(HashMap<Rc<str>, HashSet<Rc<str>>>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let component = parse_alphanumeric()
            .then_ignore(just(": "))
            .then(
                parse_alphanumeric()
                    .separated_by(just(" "))
                    .collect::<HashSet<_>>(),
            )
            .map(|(key, values): (&str, HashSet<&str>)| {
                let mut map = HashMap::new();
                map.insert(
                    Rc::from(key),
                    values.clone().into_iter().map(Rc::from).collect(),
                );
                values.clone().into_iter().fold(map, |mut acc, value| {
                    acc.insert(Rc::from(value), HashSet::from_iter(once(Rc::from(key))));
                    acc
                })
            });
        parse_lines(component)
            .map(|maps| {
                maps.into_iter().fold(
                    HashMap::new(),
                    |mut acc: HashMap<Rc<str>, HashSet<Rc<str>>>, map| {
                        map.into_iter().for_each(|(key, value)| {
                            let entry = acc.entry(key).or_default();
                            let result = entry.union(&value).cloned().collect();
                            *entry = result;
                        });
                        acc
                    },
                )
            })
            .map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day25 {}

impl Problem<Input, CommandLineArguments> for Day25 {
    type Output = usize;

    fn run(input: Input, _arguments: &CommandLineArguments) -> Self::Output {
        let encoded = encode_graph(&input.0);
        let graph = (0..encoded.len())
            .par_bridge()
            .map(|key| (key, find_all_shortest_paths(key, &encoded)))
            .fold(HashMap::new, |mut edges, (key, tree)| {
                calculate_weighted_paths(&tree, key, &mut edges);
                edges
            })
            .reduce(HashMap::new, |mut a, b| {
                b.into_iter()
                    .for_each(|(key, value)| *a.entry(key).or_insert(0) += value);
                a
            })
            .into_iter()
            .sorted_by(|(_, a), (_, b)| b.cmp(a))
            .take(3)
            .map(|(key, _)| key)
            .fold(encoded, |graph, (first, second)| {
                break_connection(graph, first, second)
            });

        is_graph_disjoint(&graph)
            .map(|(first, second)| first * second)
            .expect("Result exists")
    }
}

fn encode_graph(graph: &HashMap<Rc<str>, HashSet<Rc<str>>>) -> Vec<Vec<usize>> {
    let (str_to_usize, _) = graph.iter().fold(
        (HashMap::new(), 0usize),
        |(mut map, mut largest_index): (HashMap<&Rc<str>, usize>, usize), (key, values)| {
            if !map.contains_key(key) {
                let key_value = largest_index;
                map.insert(key, key_value);
                largest_index += 1;
            }

            values.iter().for_each(|key| {
                if !map.contains_key(key) {
                    let key_value = largest_index;
                    map.insert(key, key_value);
                    largest_index += 1;
                }
            });

            (map, largest_index)
        },
    );

    let mut result = vec![Vec::new(); str_to_usize.len()];

    graph.iter().for_each(|(key, values)| {
        let new_key = *str_to_usize.get(key).expect("exists");
        let new_values = values
            .iter()
            .map(|key| *str_to_usize.get(key).expect("Exists"))
            .collect();

        *result.get_mut(new_key).expect("exists") = new_values
    });

    result
}

fn find_all_shortest_paths(start: usize, graph: &Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    let mut visited = vec![false; graph.len()];
    let mut paths: Vec<Vec<usize>> = vec![Vec::new(); graph.len()];
    let mut queue = VecDeque::new();
    queue.push_back((start, None));

    while let Some((current, parent)) = queue.pop_front() {
        let is_visited = visited.get_mut(current).expect("Exists");
        if *is_visited {
            continue;
        }

        *is_visited = true;

        parent.into_iter().for_each(|parent| {
            let child: &mut Vec<usize> = paths.get_mut(parent).expect("exists");
            child.push(current);
        });

        graph
            .get(current)
            .expect("Node exists")
            .iter()
            .filter(|next| !visited.get(**next).expect("exists"))
            .for_each(|next| queue.push_back((*next, Some(current))));
    }

    paths
}

fn calculate_weighted_paths(
    paths: &Vec<Vec<usize>>,
    root: usize,
    edges: &mut HashMap<(usize, usize), usize>,
) -> usize {
    1 + paths
        .get(root)
        .expect("exists")
        .iter()
        .map(|child| {
            let result = calculate_weighted_paths(paths, *child, edges);
            let (e1, e2) = get_cononical_edge(&root, child);
            *edges.entry((*e1, *e2)).or_insert(0) += result;

            result
        })
        .sum::<usize>()
}

fn get_cononical_edge<'a, T: Ord>(a: &'a T, b: &'a T) -> (&'a T, &'a T) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

fn is_graph_disjoint(graph: &Vec<Vec<usize>>) -> Option<(usize, usize)> {
    let first = 0;
    let mut visited = vec![false; graph.len()];
    let mut queue = VecDeque::new();
    queue.push_back(first);

    while let Some(next) = queue.pop_front() {
        let is_visited = visited.get_mut(next).expect("Exists");
        if *is_visited {
            continue;
        }

        *is_visited = true;

        graph
            .get(next)
            .expect("Exists")
            .iter()
            .for_each(|value| queue.push_back(*value));
    }

    let visited_count = visited.into_iter().filter(|visited| *visited).count();

    if graph.len() != visited_count {
        Some((visited_count, graph.len() - visited_count))
    } else {
        None
    }
}

fn break_connection(mut graph: Vec<Vec<usize>>, first: usize, second: usize) -> Vec<Vec<usize>> {
    graph
        .get_mut(first)
        .expect("Exists")
        .retain(|a| a != &second);
    graph
        .get_mut(second)
        .expect("Exists")
        .retain(|a| a != &first);
    graph
}
