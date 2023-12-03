use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    parse::{parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, end, just, one_of},
    text::newline,
    IterParser, Parser,
};
use clap::{Args, ValueEnum};
use std::{
    cell::LazyCell,
    collections::{BTreeSet, HashMap},
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

struct Input(Vec<Vec<Item>>);

#[derive(Clone, Copy, Debug)]
enum Item {
    Number(char),
    Symbol(Symbol),
    Blank,
}

#[derive(Clone, Copy, Debug)]
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
        let item = one_of('0'..='9')
            .map(|value| Item::Number(value))
            .or(symbols.map(|symbol| Item::Symbol(symbol)))
            .or(just(".").to(Item::Blank));
        parse_lines(item.repeated().at_least(1).collect::<Vec<_>>(), 0)
            .then_ignore(newline().or_not())
            .then_ignore(end())
            .map(Input)
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
        let max_y = input.0.len() - 1;
        let max_x = input.0.first().map(|row| row.len()).unwrap_or(0) - 1;
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
                        parts.iter().into_iter().for_each(|part| {
                            acc.insert(*part, parts.clone());
                        });
                        acc
                    });
                let stars = find_stars(&input.0, max_y, max_x);
                stars
                    .into_iter()
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

fn find_numbers(items: &Vec<Vec<Item>>, max_y: usize, max_x: usize) -> Vec<Vec<BoundedPoint>> {
    let mut all_numbers = Vec::new();
    for column in 0..=max_y {
        let mut accumulator = Vec::new();

        for row in 0..=max_x {
            let current = BoundedPoint {
                x: row,
                y: column,
                max_x,
                max_y,
            };
            let value = get_value_from_item(&current, items).expect("Value exists");
            match value {
                Item::Number(_) => accumulator.push(current),
                _ => {
                    if !accumulator.is_empty() {
                        all_numbers.push(accumulator);
                        accumulator = Vec::new();
                    }
                }
            }
        }
        if !accumulator.is_empty() {
            all_numbers.push(accumulator);
        }
    }
    all_numbers
}

fn adjacent_to_symbol(point: &BoundedPoint, items: &Vec<Vec<Item>>) -> bool {
    point.into_iter_radial_adjacent().any(|adjacent| {
        get_value_from_item(&adjacent, items)
            .into_iter()
            .any(|value| match value {
                Item::Symbol(_) => true,
                _ => false,
            })
    })
}

fn combine_numbers(numbers: &Vec<BoundedPoint>, items: &Vec<Vec<Item>>) -> usize {
    usize::from_str_radix(
        &numbers
            .into_iter()
            .filter_map(|number| get_value_from_item(number, items))
            .filter_map(|number| match number {
                Item::Number(value) => Some(value),
                _ => None,
            })
            .collect::<String>(),
        10,
    )
    .expect("Valid int")
}

fn get_value_from_item(point: &BoundedPoint, items: &Vec<Vec<Item>>) -> Option<Item> {
    items
        .get(point.y)
        .and_then(|row| row.get(point.x).map(|value| *value))
}

fn find_stars(items: &Vec<Vec<Item>>, max_y: usize, max_x: usize) -> Vec<BoundedPoint> {
    items
        .into_iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.into_iter()
                .enumerate()
                .filter_map(move |(x, value)| match value {
                    Item::Symbol(symbol) => match symbol {
                        Symbol::Star => Some(BoundedPoint { x, y, max_x, max_y }),
                        _ => None,
                    },
                    _ => None,
                })
        })
        .collect()
}
