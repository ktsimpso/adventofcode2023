use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_alphanumeric, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, Parser};
use clap::Args;
use itertools::Itertools;
use std::cell::LazyCell;

pub const DAY_01: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day01>::new(
            "day01",
            "Finds the first numeric digit and last numeric digit and concatenates them for each line. Then sums the values of each line from the resulting number",
            "newline delimited strings with at least 2 digits per line.",
        )
        .with_part("Calibration values only use literal digits on the default input", CommandLineArguments { words: false })
        .with_part("Calibration values can use both literal, and spelt out digits on the default input", CommandLineArguments { words: true }),
    )
});

struct Input(Vec<String>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        parse_lines(parse_alphanumeric().map(|s: &'a str| s.to_string())).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "Include spelt digits when determining the calibration value"
    )]
    words: bool,
}

struct Day01 {}

impl Problem<Input, CommandLineArguments> for Day01 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let digits = vec![
            ("1", "1"),
            ("2", "2"),
            ("3", "3"),
            ("4", "4"),
            ("5", "5"),
            ("6", "6"),
            ("7", "7"),
            ("8", "8"),
            ("9", "9"),
            ("0", "0"),
        ];

        let words = vec![
            ("one", "1"),
            ("two", "2"),
            ("three", "3"),
            ("four", "4"),
            ("five", "5"),
            ("six", "6"),
            ("seven", "7"),
            ("eight", "8"),
            ("nine", "9"),
            ("zero", "0"),
        ];

        let charset = if arguments.words {
            digits.into_iter().chain(words).collect::<Vec<_>>()
        } else {
            digits
        };

        input
            .0
            .iter()
            .map(|word| {
                let first = charset
                    .iter()
                    .filter_map(|(pattern, value)| {
                        word.find(pattern).map(|index| (index, value.to_string()))
                    })
                    .sorted_by(|(index1, _), (index2, _)| index1.cmp(index2))
                    .map(|(_, value)| value)
                    .take(1);
                let second = charset
                    .iter()
                    .filter_map(|(pattern, value)| {
                        word.rfind(pattern).map(|index| (index, value.to_string()))
                    })
                    .sorted_by(|(index1, _), (index2, _)| index2.cmp(index1))
                    .map(|(_, value)| value)
                    .take(1);

                first
                    .chain(second)
                    .join("")
                    .parse::<usize>()
                    .expect("Valid integer")
            })
            .sum()
    }
}
