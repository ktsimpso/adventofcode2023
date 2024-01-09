use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    parse::{parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, Parser};
use clap::Args;
use itertools::Itertools;
use ndarray::Array2;
use std::{
    cell::LazyCell,
    cmp::{max, min},
};

pub const DAY_11: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day11>::new(
            "day11",
            "Measures the distance between galaxies and sums them",
            "Table of the current universe",
        )
        .with_part(
            "Expansion Rate is 2",
            CommandLineArguments { expansion_rate: 2 },
        )
        .with_part(
            "Expansion Rate is 1_000_000",
            CommandLineArguments {
                expansion_rate: 1_000_000,
            },
        ),
    )
});

struct Input(Array2<SpaceTime>);

#[derive(Clone, PartialEq, Eq)]
enum SpaceTime {
    Space,
    Galaxy,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let space_time = just(".")
            .to(SpaceTime::Space)
            .or(just("#").to(SpaceTime::Galaxy));
        parse_table2(space_time).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "How fast the universe expands.")]
    expansion_rate: usize,
}

struct Day11 {}

impl Problem<Input, CommandLineArguments> for Day11 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let expansion_rate = arguments.expansion_rate - 1;
        let empty_rows = find_empty_rows(&input.0);
        let empty_columns = find_empty_columns(&input.0);
        let max_x = input.0.dim().1 - 1;
        let max_y = input.0.dim().0 - 1;

        let galaxies = find_galaxies(&input.0, max_x, max_y);

        galaxies
            .into_iter()
            .combinations(2)
            .map(|points| {
                let mut points = points.into_iter();
                let first = points.next().expect("one exists");
                let second = points.next().expect("Two exists");

                let low_x = min(first.x, second.x);
                let high_x = max(first.x, second.x);

                let expand_factor_x = empty_columns
                    .iter()
                    .filter(|x| x > &&low_x && x < &&high_x)
                    .count();

                let low_y = min(first.y, second.y);
                let high_y = max(first.y, second.y);

                let expand_factor_y = empty_rows
                    .iter()
                    .filter(|y| y > &&low_y && y < &&high_y)
                    .count();

                ((high_x + expand_factor_x * expansion_rate) - low_x)
                    + ((high_y + expand_factor_y * expansion_rate) - low_y)
            })
            .sum()
    }
}

fn find_galaxies(space_time: &Array2<SpaceTime>, max_x: usize, max_y: usize) -> Vec<BoundedPoint> {
    space_time
        .indexed_iter()
        .filter(|(_, space)| space == &&SpaceTime::Galaxy)
        .map(move |((y, x), _)| BoundedPoint { x, y, max_x, max_y })
        .collect()
}

fn find_empty_rows(space_time: &Array2<SpaceTime>) -> Vec<usize> {
    space_time
        .rows()
        .into_iter()
        .enumerate()
        .filter(|(_, row)| row.into_iter().all(|space| space == &SpaceTime::Space))
        .map(|(y, _)| y)
        .collect()
}

fn find_empty_columns(space_time: &Array2<SpaceTime>) -> Vec<usize> {
    space_time
        .columns()
        .into_iter()
        .enumerate()
        .filter(|(_, column)| column.into_iter().all(|space| space == &SpaceTime::Space))
        .map(|(x, _)| x)
        .collect()
}
