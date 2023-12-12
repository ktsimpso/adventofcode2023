use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    parse::{parse_digit, parse_lines, parse_table, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{any, choice, end, just},
    text::newline,
    IterParser, Parser,
};
use clap::{Args, ValueEnum};
use itertools::Itertools;
use std::{
    cell::LazyCell,
    collections::{BTreeMap, BTreeSet, HashMap},
};
use tap::Tap;

pub const DAY_08: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day08>::new("day08", "help", "file help")
            .with_part("part1", CommandLineArguments {})
            .with_part("part2", CommandLineArguments {}),
    )
});

#[derive(Debug)]
struct Input {
    directions: Vec<Direction>,
    map: BTreeMap<String, (String, String)>,
}

#[derive(Clone, Debug)]
enum Direction {
    Left,
    Right,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let direction = just("L")
            .to(Direction::Left)
            .or(just("R").to(Direction::Right));
        let token = any().repeated().exactly(3).to_slice();
        let map = token
            .then_ignore(just(" = ("))
            .then(token)
            .then_ignore(just(", "))
            .then(token)
            .then_ignore(just(")"))
            .map(|((key, v1), v2): ((&str, &str), &str)| {
                (key.to_string(), (v1.to_string(), v2.to_string()))
            });
        direction
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then_ignore(newline().repeated().exactly(2))
            .then(parse_lines(map))
            .map(|(directions, map)| {
                let result_map = map.into_iter().fold(BTreeMap::new(), |acc, (key, value)| {
                    acc.tap_mut(|m| {
                        m.insert(key, value);
                    })
                });
                Input {
                    directions,
                    map: result_map,
                }
            })
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day08 {}

impl Problem<Input, CommandLineArguments> for Day08 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let directions = input.directions.into_iter().cycle();
        let mut current = input
            .map
            .keys()
            .filter(|key| key.ends_with("A"))
            .cloned()
            .collect::<Vec<_>>();
        let initial = current.clone();
        println!("Started with: {} nodes", initial.len());
        let mut found = initial.iter().map(|_| false).collect::<Vec<_>>();
        //let mut current = "AAA".to_string();
        let mut steps = 0usize;

        for direction in directions {
            current = current
                .into_iter()
                .map(|c| {
                    let mapping = input.map.get(&c).expect("Mapping exists");
                    match direction {
                        Direction::Left => mapping.0.clone(),
                        Direction::Right => mapping.1.clone(),
                    }
                })
                .collect::<Vec<_>>();
            //println!("================");
            steps += 1;
            initial
                .iter()
                .zip(current.iter())
                .zip(found.iter_mut())
                .for_each(|((i, c), been_found)| {
                    if c.ends_with("Z") && !*been_found {
                        *been_found = true;
                        println!("I: {} loops after: {}", i, steps);
                    }
                });
            if found.iter().all(|c| *c) {
                break;
            }

            if current.iter().all(|c| c.ends_with("Z")) {
                break;
            }
            //println!("------------------");
            //let mapping = input.map.get(&current).expect("Mapping exists");
            /*match direction {
                Direction::Left => current = mapping.0.clone(),
                Direction::Right => current = mapping.1.clone(),
            };
            steps += 1;
            if current == "ZZZ".to_string() {
                break;
            }*/
        }

        steps
    }
}
