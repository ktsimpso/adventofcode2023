use crate::libs::{
    cli::{CliProblem, Command},
    graph::BoundedPoint,
    math::absolute_difference,
    parse::{parse_isize, parse_lines, parse_table, parse_usize, StringParse},
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
use itertools::Itertools;
use std::{
    cell::LazyCell,
    cmp::{max, min},
};
use tap::Tap;

pub const DAY_11: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day11>::new("day11", "help", "file help")
            .with_part("part1", CommandLineArguments {})
            .with_part("part2", CommandLineArguments {}),
    )
});

struct Input(Vec<Vec<SpaceTime>>);

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
        parse_table(space_time).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day11 {}

impl Problem<Input, CommandLineArguments> for Day11 {
    type Output = usize;

    fn run(mut input: Input, arguments: &CommandLineArguments) -> Self::Output {
        //print_space_time(&input.0);
        /*let space_time = expand_space_time(input.0);

                let max_y = space_time.len() - 1;
                let max_x = space_time.first().map(|row| row.len()).unwrap_or(0) - 1;

                let galaxies = find_galaxies(&space_time, max_x, max_y);

                galaxies
                    .into_iter()
                    .combinations(2)
                    .map(|points| {
                        let mut points = points.into_iter();
                        let first = points.next().expect("one exists");
                        let second = points.next().expect("Two exists");
                        distance(&first, &second)
                    })
                    .sum()
        */
        //print_space_time(&space_time);
        let expansion_rate = 1000000 - 1;
        let empty_rows = find_empty_rows(&input.0);
        let empty_columns = find_empty_columns(&input.0);
        let max_y = input.0.len() - 1;
        let max_x = input.0.first().map(|row| row.len()).unwrap_or(0) - 1;

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

fn distance(first: &BoundedPoint, second: &BoundedPoint) -> usize {
    absolute_difference(first.x, second.x) + absolute_difference(first.y, second.y)
}

fn find_galaxies(
    space_time: &Vec<Vec<SpaceTime>>,
    max_x: usize,
    max_y: usize,
) -> Vec<BoundedPoint> {
    space_time
        .into_iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.into_iter()
                .enumerate()
                .filter(|(_, space)| space == &&SpaceTime::Galaxy)
                .map(move |(x, _)| BoundedPoint { x, y, max_x, max_y })
        })
        .collect()
}

fn find_empty_rows(space_time: &Vec<Vec<SpaceTime>>) -> Vec<usize> {
    space_time
        .iter()
        .enumerate()
        .filter(|(_, row)| row.into_iter().all(|space| space == &SpaceTime::Space))
        .map(|(y, _)| y)
        .rev()
        .collect()
}

fn find_empty_columns(space_time: &Vec<Vec<SpaceTime>>) -> Vec<usize> {
    space_time
        .iter()
        .flat_map(|row| row.into_iter().enumerate())
        .into_group_map()
        .into_iter()
        .filter(|(_, column)| column.into_iter().all(|space| space == &&SpaceTime::Space))
        .map(|(x, _)| x)
        .sorted()
        .rev()
        .collect()
}

fn expand_space_time(mut space_time: Vec<Vec<SpaceTime>>) -> Vec<Vec<SpaceTime>> {
    let row_size = space_time.iter().next().expect("At least one row").len();
    let empty_rows = find_empty_rows(&space_time);

    let empty_columns = find_empty_columns(&space_time);

    empty_rows
        .into_iter()
        .for_each(|y| space_time.insert(y, vec![SpaceTime::Space; row_size]));

    empty_columns.into_iter().for_each(|x| {
        space_time
            .iter_mut()
            .for_each(|row| row.insert(x, SpaceTime::Space))
    });

    space_time
}

fn print_space_time(space_time: &Vec<Vec<SpaceTime>>) {
    space_time.into_iter().for_each(|row| {
        println!(
            "{}",
            row.into_iter()
                .map(|space| match space {
                    SpaceTime::Space => ".",
                    SpaceTime::Galaxy => "#",
                })
                .join("")
        )
    })
}
