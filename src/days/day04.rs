use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    parse::{parse_digit, parse_lines, parse_table, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    IterParser, Parser,
};
use clap::{Args, ValueEnum};
use itertools::Itertools;
use std::{
    cell::LazyCell,
    collections::{BTreeSet, HashMap},
};

pub const DAY_04: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day04>::new(
            "day04",
            "Finds information about winning scratch cards",
            "Each line should have one scratch card.",
        )
        .with_part(
            "Finds the score for each scratch card and sums the values.",
            CommandLineArguments {},
        ), //.with_part("help", CommandLineArguments {}),
    )
});

struct Input(Vec<Card>);

struct Card {
    number: usize,
    winning_numbers: Vec<usize>,
    match_numbers: Vec<usize>,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let card = just("Card")
            .then_ignore(just(" ").repeated().at_least(1))
            .ignore_then(parse_usize())
            .then_ignore(just(": ").then(just(" ").or_not()))
            .then(
                parse_usize()
                    .separated_by(just(" ").repeated().at_least(1))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(" |").then_ignore(just(" ").repeated().at_least(1)))
            .then(
                parse_usize()
                    .separated_by(just(" ").repeated().at_least(1))
                    .collect::<Vec<_>>(),
            )
            .map(|((number, winning_numbers), match_numbers)| Card {
                number,
                winning_numbers,
                match_numbers,
            });
        parse_lines(card).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day04 {}

impl Problem<Input, CommandLineArguments> for Day04 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input.0.into_iter().map(|card| count_points(&card)).sum()
    }
}

fn count_points(card: &Card) -> usize {
    card.match_numbers
        .iter()
        .filter(|number| card.winning_numbers.contains(number))
        .fold(0, |acc, _| if acc == 0 { 1 } else { acc * 2 })
}
