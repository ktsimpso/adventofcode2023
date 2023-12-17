use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_isize, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::Args;
use std::{
    cell::LazyCell,
    ops::{Add, Sub},
};
use tap::Tap;

pub const DAY_09: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day09>::new(
            "day09",
            "Finds the next value in sequences and sums them",
            "One sequence per line each value separated by a space.",
        )
        .with_part(
            "Calculates the next value at the back",
            CommandLineArguments { previous: false },
        )
        .with_part(
            "Calculates the previous value at the front",
            CommandLineArguments { previous: true },
        ),
    )
});

struct Input(Vec<Vec<isize>>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        parse_lines(parse_isize().separated_by(just(" ")).at_least(1).collect()).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "If the value at the front should be calculated instead of the back"
    )]
    previous: bool,
}

struct Day09 {}

impl Problem<Input, CommandLineArguments> for Day09 {
    type Output = isize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let push = if arguments.previous {
            |last_items: &mut Vec<isize>, sequence: &Vec<isize>| {
                last_items.push(*sequence.first().expect("At least one item"))
            }
        } else {
            |last_items: &mut Vec<isize>, sequence: &Vec<isize>| {
                last_items.push(*sequence.last().expect("At least one item"))
            }
        };
        let operation: fn(isize, isize) -> isize = if arguments.previous {
            isize::sub
        } else {
            isize::add
        };

        input
            .0
            .into_iter()
            .map(|sequence| {
                let mut last_items = Vec::new();
                push(&mut last_items, &sequence);

                let mut next_sequence = sequence;

                while !next_sequence.iter().all(|value| value == &0) {
                    next_sequence = map_sequence(&next_sequence);
                    push(&mut last_items, &next_sequence);
                }

                last_items
                    .tap_mut(|items| items.reverse())
                    .into_iter()
                    .fold(0, |acc, value| operation(value, acc))
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
