use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_alphanumeric, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{end, just},
    text::newline,
    IterParser, Parser,
};
use clap::{Args, ValueEnum};
use std::cell::LazyCell;

pub const DAY_15: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day15>::new(
            "day15",
            "Finds information about the initialization sequence",
            "The sequence of comma separated initialization values",
        )
        .with_part(
            "Calculates the hash of all initiallization values and sums them.",
            CommandLineArguments {
                calculation: Calculation::Hash,
            },
        )
        .with_part(
            "Finds the focusing power of the map specified by the initialization values.",
            CommandLineArguments {
                calculation: Calculation::Map,
            },
        ),
    )
});

struct Input(Vec<Label>);

struct Label {
    label: String,
    operation: Operation,
}

impl ToString for Label {
    fn to_string(&self) -> String {
        format!(
            "{}{}",
            self.label,
            match self.operation {
                Operation::Equal(value) => format!("={}", value),
                Operation::Dash => "-".to_string(),
            }
        )
    }
}

#[derive(Clone)]
enum Operation {
    Equal(usize),
    Dash,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        parse_label()
            .separated_by(just(","))
            .collect::<Vec<_>>()
            .then_ignore(newline())
            .then_ignore(end())
            .map(Input)
    }
}

fn parse_label<'a>() -> impl Parser<'a, &'a str, Label, extra::Err<Rich<'a, char>>> {
    let equal = just("=")
        .ignore_then(parse_usize())
        .map(|value| Operation::Equal(value));
    let dash = just("-").to(Operation::Dash);
    let operation = equal.or(dash);
    parse_alphanumeric()
        .then(operation)
        .map(|(label, operation)| Label {
            label: label.to_string(),
            operation,
        })
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The type of calculation to perform")]
    calculation: Calculation,
}

#[derive(Clone, ValueEnum)]
enum Calculation {
    Hash,
    Map,
}

struct Day15 {}

impl Problem<Input, CommandLineArguments> for Day15 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        match arguments.calculation {
            Calculation::Hash => input
                .0
                .into_iter()
                .map(|item| hash(&item.to_string()))
                .sum(),
            Calculation::Map => {
                let mut boxes = vec![Vec::<(String, usize)>::new(); 256];
                input.0.into_iter().for_each(|label| {
                    let hash = hash(&label.label);
                    let box_ = boxes.get_mut(hash).expect("Exists");
                    let item = box_
                        .iter_mut()
                        .enumerate()
                        .find(|(_, (l, _))| l == &label.label);
                    match (item, label.operation) {
                        (None, Operation::Equal(value)) => box_.push((label.label, value)),
                        (None, Operation::Dash) => (),
                        (Some((_, old)), Operation::Equal(value)) => *old = (label.label, value),
                        (Some((index, _)), Operation::Dash) => {
                            box_.remove(index);
                        }
                    }
                });
                boxes
                    .into_iter()
                    .enumerate()
                    .map(|(box_number, items)| {
                        items
                            .into_iter()
                            .enumerate()
                            .map(|(slot_number, (_, focal_length))| {
                                (box_number + 1) * (slot_number + 1) * focal_length
                            })
                            .sum::<usize>()
                    })
                    .sum()
            }
        }
    }
}

fn hash(input: &str) -> usize {
    input
        .as_bytes()
        .into_iter()
        .fold(0, |acc, char| ((acc + usize::from(*char)) * 17) % 256)
}
