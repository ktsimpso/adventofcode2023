use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_alphanumeric, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::Args;
use either::Either;
use itertools::Itertools;
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{
    cell::LazyCell,
    collections::{HashMap, HashSet, VecDeque},
    iter::{empty, once},
    sync::Arc,
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

struct Input(HashMap<Arc<str>, HashSet<Arc<str>>>);

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
                    Arc::from(key),
                    values.clone().into_iter().map(|s| Arc::from(s)).collect(),
                );
                values.clone().into_iter().fold(map, |mut acc, value| {
                    acc.insert(Arc::from(value), HashSet::from_iter(once(Arc::from(key))));
                    acc
                })
            });
        parse_lines(component)
            .map(|maps| {
                maps.into_iter().fold(
                    HashMap::new(),
                    |mut acc: HashMap<Arc<str>, HashSet<Arc<str>>>, map| {
                        map.into_iter().for_each(|(key, value)| {
                            let entry = acc.entry(key).or_insert_with(|| HashSet::new());
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
        let graph = input
            .0
            .keys()
            .par_bridge()
            .map(|key| find_all_shortest_paths(key, &input.0))
            .flat_map(|paths| {
                let mut map = HashMap::new();
                paths.calculate_weighted_paths(&mut map);
                map
            })
            .fold(
                || HashMap::new(),
                |mut acc, (key, value)| {
                    *acc.entry(key).or_insert(0) += value;
                    acc
                },
            )
            .reduce(
                || HashMap::new(),
                |mut a, b| {
                    b.into_iter()
                        .for_each(|(key, value)| *a.entry(key).or_insert(0) += value);
                    a
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

fn find_all_shortest_paths<'a>(
    start: &'a Arc<str>,
    graph: &'a HashMap<Arc<str>, HashSet<Arc<str>>>,
) -> Node<'a> {
    let mut visited = HashSet::new();
    let mut paths = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_back((start, None));

    while let Some((current, parent)) = queue.pop_front() {
        if visited.contains(current) {
            continue;
        }

        visited.insert(current);

        parent.into_iter().for_each(|parent| {
            paths
                .entry(parent)
                .or_insert_with(|| Vec::new())
                .push(current)
        });

        graph
            .get(current)
            .expect("Node exists")
            .into_iter()
            .filter(|next| !visited.contains(next))
            .for_each(|next| queue.push_back((next, Some(current))));
    }

    Node::from_map_and_root(&paths, start)
}

struct Node<'a> {
    current: &'a Arc<str>,
    children: Vec<Node<'a>>,
}

impl<'a> Node<'a> {
    fn from_map_and_root(
        map: &HashMap<&'a Arc<str>, Vec<&'a Arc<str>>>,
        root: &'a Arc<str>,
    ) -> Self {
        let children = map
            .get(root)
            .map(|children| Either::Left(children.into_iter()))
            .unwrap_or_else(|| Either::Right(empty()))
            .map(|child| Node::from_map_and_root(map, child))
            .collect();
        Node {
            current: root,
            children,
        }
    }

    fn calculate_weighted_paths(
        &self,
        map: &mut HashMap<(&'a Arc<str>, &'a Arc<str>), usize>,
    ) -> usize {
        1 + self
            .children
            .iter()
            .map(|child| {
                let result = child.calculate_weighted_paths(map);

                map.insert(get_cononical_edge(&self.current, &child.current), result);

                result
            })
            .sum::<usize>()
    }
}

fn get_cononical_edge<'a>(a: &'a Arc<str>, b: &'a Arc<str>) -> (&'a Arc<str>, &'a Arc<str>) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
}

fn is_graph_disjoint(graph: &HashMap<Arc<str>, HashSet<Arc<str>>>) -> Option<(usize, usize)> {
    let first = graph.keys().next().expect("At least one");
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
    mut graph: HashMap<Arc<str>, HashSet<Arc<str>>>,
    first: &Arc<str>,
    second: &Arc<str>,
) -> HashMap<Arc<str>, HashSet<Arc<str>>> {
    graph.get_mut(first).expect("Exists").remove(second);
    graph.get_mut(second).expect("Exists").remove(first);
    graph
}
