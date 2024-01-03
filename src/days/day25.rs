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
    collections::{BTreeMap, HashSet, VecDeque},
    iter::once,
};
use tap::Tap;

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

struct Input(BTreeMap<String, HashSet<String>>);

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
                let mut map = BTreeMap::new();
                map.insert(
                    key.to_string(),
                    values.clone().into_iter().map(|s| s.to_string()).collect(),
                );
                values.clone().into_iter().fold(map, |mut acc, value| {
                    acc.insert(value.to_string(), HashSet::from_iter(once(key.to_string())));
                    acc
                })
            });
        parse_lines(component)
            .map(|maps| {
                maps.into_iter().fold(
                    BTreeMap::new(),
                    |acc: BTreeMap<String, HashSet<String>>, map| {
                        acc.into_iter()
                            .merge_join_by(map.into_iter(), |(key1, _), (key2, _)| key1.cmp(key2))
                            .map(|result| match result {
                                itertools::EitherOrBoth::Both((key, value1), (_, value2)) => (
                                    key,
                                    value2.union(&value1).cloned().collect::<HashSet<String>>(),
                                ),
                                itertools::EitherOrBoth::Left((key, set)) => (key, set),
                                itertools::EitherOrBoth::Right((key, set)) => (key, set),
                            })
                            .collect()
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
        let graph = input
            .0
            .keys()
            .par_bridge()
            .map(|key| find_all_shortest_paths(key, &input.0))
            .flat_map(|paths| paths.values().cloned().collect::<Vec<_>>())
            .fold(
                || BTreeMap::new(),
                |mut acc, path| {
                    path.into_iter()
                        .map_windows(|[first, second]| {
                            let [first, second] =
                                [first.clone(), second.clone()].tap_mut(|v| v.sort());
                            (first, second)
                        })
                        .for_each(|pair| {
                            *acc.entry(pair).or_insert(0usize) += 1;
                        });
                    acc
                },
            )
            .reduce(
                || BTreeMap::new(),
                |a, b| {
                    a.into_iter()
                        .merge_join_by(b.into_iter(), |(key1, _), (key2, _)| key1.cmp(key2))
                        .map(|result| match result {
                            itertools::EitherOrBoth::Both((key, value1), (_, value2)) => {
                                (key, value1 + value2)
                            }
                            itertools::EitherOrBoth::Left(result) => result,
                            itertools::EitherOrBoth::Right(result) => result,
                        })
                        .collect()
                },
            )
            .into_iter()
            .sorted_by(|(_, a), (_, b)| b.cmp(a))
            .take(3)
            .map(|(key, _)| key)
            .fold(input.0.clone(), |graph, (first, second)| {
                break_connection(graph, &first, &second)
            });

        is_graph_disjoint(&graph)
            .map(|(first, second)| first * second)
            .expect("Result exists")
    }
}

fn find_all_shortest_paths(
    start: &String,
    graph: &BTreeMap<String, HashSet<String>>,
) -> BTreeMap<(String, String), Vec<String>> {
    let mut visited = HashSet::new();
    let mut paths = BTreeMap::new();
    let mut queue = VecDeque::new();
    queue.push_back((start, Vec::new()));

    while let Some((current, path)) = queue.pop_front() {
        if visited.contains(current) {
            continue;
        }

        visited.insert(current);

        let next_path = path.clone().tap_mut(|p| p.push(current.clone()));

        paths.insert((start.clone(), current.clone()), path);

        graph
            .get(current)
            .expect("Node exists")
            .into_iter()
            .filter(|next| !visited.contains(next))
            .for_each(|next| queue.push_back((next, next_path.clone())));
    }

    paths
}

fn is_graph_disjoint(graph: &BTreeMap<String, HashSet<String>>) -> Option<(usize, usize)> {
    let first = graph.first_key_value().expect("At least one").0;
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(first);

    while let Some(next) = queue.pop_front() {
        if visited.contains(next) {
            continue;
        }
        visited.insert(next);

        graph
            .get(next)
            .expect("Exists")
            .into_iter()
            .for_each(|value| queue.push_back(value));
    }

    if graph.len() != visited.len() {
        Some((visited.len(), graph.len() - visited.len()))
    } else {
        None
    }
}

fn break_connection(
    mut graph: BTreeMap<String, HashSet<String>>,
    first: &String,
    second: &String,
) -> BTreeMap<String, HashSet<String>> {
    graph.get_mut(first).expect("Exists").remove(second);
    graph.get_mut(second).expect("Exists").remove(first);
    graph
}
