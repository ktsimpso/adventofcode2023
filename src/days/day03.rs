use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    parse::{parse_digit, parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::{Args, ValueEnum};
use itertools::Itertools;
use ndarray::Array2;
use std::{
    cell::LazyCell,
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

pub const DAY_03: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day03>::new(
            "day03",
            "Finds parts numbers in the machine schematic",
            "Grid of equal rows and columns with part numbers and symbols.",
        )
        .with_part(
            "Sums all the valid part numbers in the schematic",
            CommandLineArguments {
                part_stat: PartStats::ValidParts,
            },
        )
        .with_part(
            "Finds the gear ratio of all gears (*) in the schematic and sums them",
            CommandLineArguments {
                part_stat: PartStats::GearRatio,
            },
        ),
    )
});

struct Input(Array2<Item>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Item {
    Number(char),
    Symbol(Symbol),
    Blank,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Symbol {
    Star,
    Ampersand,
    Percent,
    Dollar,
    Hash,
    At,
    Plus,
    Equal,
    Slash,
    Minus,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let symbols = choice((
            just("*").to(Symbol::Star),
            just("&").to(Symbol::Ampersand),
            just("%").to(Symbol::Percent),
            just("$").to(Symbol::Dollar),
            just("#").to(Symbol::Hash),
            just("@").to(Symbol::At),
            just("+").to(Symbol::Plus),
            just("=").to(Symbol::Equal),
            just("/").to(Symbol::Slash),
            just("-").to(Symbol::Minus),
        ));
        let item = parse_digit()
            .map(|value| Item::Number(value))
            .or(symbols.map(|symbol| Item::Symbol(symbol)))
            .or(just(".").to(Item::Blank));
        parse_table2(item).map(Input)
    }
}

#[derive(ValueEnum, Clone)]
enum PartStats {
    ValidParts,
    GearRatio,
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The stat to look for")]
    part_stat: PartStats,
}

struct Day03 {}

impl Problem<Input, CommandLineArguments> for Day03 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_x = input.0.dim().1 - 1;
        let max_y = input.0.dim().0 - 1;

        let part_numbers = find_numbers(&input.0, max_y, max_x)
            .into_iter()
            .filter(|numbers| {
                numbers
                    .into_iter()
                    .any(|number| adjacent_to_symbol(number, &input.0))
            })
            .collect::<Vec<_>>();

        match arguments.part_stat {
            PartStats::ValidParts => part_numbers
                .into_iter()
                .map(|numbers| combine_numbers(&numbers, &input.0))
                .sum(),
            PartStats::GearRatio => {
                let part_map = part_numbers
                    .into_iter()
                    .fold(HashMap::new(), |mut acc, parts| {
                        let parts = Rc::new(parts);
                        parts.iter().for_each(|part| {
                            acc.insert(*part, parts.clone());
                        });
                        acc
                    });
                find_stars(&input.0, max_y, max_x)
                    .filter_map(|star| {
                        let adjacent_numbers = star
                            .into_iter_radial_adjacent()
                            .filter_map(|adjacent| part_map.get(&adjacent))
                            .fold(BTreeSet::new(), |mut acc, number| {
                                acc.insert(number);
                                acc
                            });
                        if adjacent_numbers.len() == 2 {
                            Some(adjacent_numbers)
                        } else {
                            None
                        }
                    })
                    .map(|matched_numbers| {
                        matched_numbers
                            .into_iter()
                            .map(|number| combine_numbers(&number, &input.0))
                            .fold(1, |acc, number| acc * number)
                    })
                    .sum()
            }
        }
    }
}

fn find_numbers(items: &Array2<Item>, max_y: usize, max_x: usize) -> Vec<Vec<BoundedPoint>> {
    items
        .rows()
        .into_iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.indexed_iter()
                .group_by(|(_, item)| match item {
                    Item::Number(_) => true,
                    _ => false,
                })
                .into_iter()
                .filter_map(|(key, group)| {
                    if key {
                        Some(
                            group
                                .map(|(x, _)| BoundedPoint { x, y, max_x, max_y })
                                .collect::<Vec<_>>(),
                        )
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn adjacent_to_symbol(point: &BoundedPoint, items: &Array2<Item>) -> bool {
    point.into_iter_radial_adjacent().any(|adjacent| {
        items
            .get((adjacent.y, adjacent.x))
            .into_iter()
            .any(|value| match value {
                Item::Symbol(_) => true,
                _ => false,
            })
    })
}

fn combine_numbers(numbers: &Vec<BoundedPoint>, items: &Array2<Item>) -> usize {
    usize::from_str_radix(
        &numbers
            .into_iter()
            .filter_map(|number| items.get((number.y, number.x)))
            .filter_map(|number| match number {
                Item::Number(value) => Some(value),
                _ => None,
            })
            .collect::<String>(),
        10,
    )
    .expect("Valid int")
}

fn find_stars<'a>(
    items: &'a Array2<Item>,
    max_y: usize,
    max_x: usize,
) -> impl Iterator<Item = BoundedPoint> + 'a {
    items
        .indexed_iter()
        .filter_map(move |((y, x), value)| match value {
            Item::Symbol(symbol) => match symbol {
                Symbol::Star => Some(BoundedPoint { x, y, max_x, max_y }),
                _ => None,
            },
            _ => None,
        })
}
