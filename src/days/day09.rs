use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_isize, parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{end, just},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use integer_sqrt::IntegerSquareRoot;
use std::cell::LazyCell;
use tap::Tap;

pub const DAY_09: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day09>::new("day09", "help", "file help")
            .with_part("part1", CommandLineArguments {})
            .with_part("part2", CommandLineArguments {}),
    )
});

struct Input(Vec<Vec<isize>>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        parse_lines(parse_isize().separated_by(just(" ")).at_least(1).collect()).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day09 {}

impl Problem<Input, CommandLineArguments> for Day09 {
    type Output = isize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .into_iter()
            .map(|sequence| {
                let mut last_items = Vec::new();
                //last_items.push(*sequence.last().expect("At least one item"));
                last_items.push(*sequence.first().expect("At least one item"));

                let mut next_sequence = sequence;

                while !next_sequence.iter().all(|value| value == &0) {
                    next_sequence = map_sequence(&next_sequence);
                    //last_items.push(*next_sequence.last().expect("At least one item"))
                    last_items.push(*next_sequence.first().expect("At least one item"))
                }

                last_items
                    .tap_mut(|items| items.reverse())
                    .into_iter()
                    .fold(0, |acc, value| value - acc)
            })
            .sum()
    }
}

fn map_sequence(sequence: &Vec<isize>) -> Vec<isize> {
    sequence
        .into_iter()
        .map_windows(|[first, second]| **second - **first)
        .collect()
}
