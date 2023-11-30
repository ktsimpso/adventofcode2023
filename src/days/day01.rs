use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_between_blank_lines, parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, Parser};
use clap::Args;
use std::cell::LazyCell;
use tap::Tap;

pub const DAY_01: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day01>::new(
            "day01",
            "The first day!",
            "The file help",
        )
        .with_part("part1 help", CommandLineArguments { n: 1 })
        .with_part("part2 help", CommandLineArguments { n: 3 }),
    )
});

struct Input(Vec<Vec<usize>>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        parse_between_blank_lines(parse_lines(parse_usize(), 1)).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long = "number", help = "The number of elves to sum")]
    n: usize,
}

struct Day01 {}

impl Problem<Input, CommandLineArguments> for Day01 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .iter()
            .map(|bag| bag.into_iter().sum())
            .collect::<Vec<usize>>()
            .tap_mut(|sums| sums.sort())
            .tap_mut(|sums| sums.reverse())
            .into_iter()
            .take(arguments.n)
            .sum()
    }
}
