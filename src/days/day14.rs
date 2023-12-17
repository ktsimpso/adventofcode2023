use crate::libs::{
    cli::{flag_arg, single_arg, CliArgs, CliProblem, Command},
    parse::{parse_table, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::{value_parser, Arg, ArgMatches};
use either::Either;
use itertools::Itertools;
use std::{
    cell::LazyCell,
    collections::{BTreeMap, HashMap},
};

pub const DAY_14: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day14>::new(
            "day14",
            "Calculates the weight of the dish after tilting various ways",
            "Current conifguration of the rocks controlling the dish",
        )
        .with_part(
            "Tilts north once",
            CommandLineArguments { tilt: Tilt::North },
        )
        .with_part(
            "Tilts north then west the south then east 1_000_000_000 times",
            CommandLineArguments {
                tilt: Tilt::Tumble(1_000_000_000),
            },
        ),
    )
});

struct Input(Vec<Vec<RockField>>);

#[derive(Clone, PartialEq, Eq, Hash)]
enum RockField {
    Rock,
    MovableRock,
    Empty,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let rock_field = choice((
            just("#").to(RockField::Rock),
            just("O").to(RockField::MovableRock),
            just(".").to(RockField::Empty),
        ));
        parse_table(rock_field).map(Input)
    }
}

struct CommandLineArguments {
    tilt: Tilt,
}

impl CliArgs for CommandLineArguments {
    fn get_args() -> Vec<Arg> {
        let north = flag_arg(
            "north",
            'n',
            "Calculates the load of the dish after tilting north",
        );
        let tumble = single_arg(
            "tumble",
            't',
            "Calculates the load of the dish after tumbling n iterations",
        )
        .value_parser(value_parser!(usize))
        .conflicts_with("north");
        vec![north, tumble]
    }

    fn parse_output(args: &ArgMatches) -> Self {
        let north = if args.get_flag("north") {
            Some(Tilt::North)
        } else {
            None
        };
        let tumble = args.get_one::<usize>("tumble").map(|n| Tilt::Tumble(*n));

        let tilt = match (north, tumble) {
            (None, Some(tumble)) => tumble,
            (Some(north), None) => north,
            _ => unreachable!(),
        };

        CommandLineArguments { tilt }
    }
}

#[derive(Clone)]
enum Tilt {
    North,
    Tumble(usize),
}

struct Day14 {}

impl Problem<Input, CommandLineArguments> for Day14 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        match arguments.tilt {
            Tilt::North => score(&fall_north(&input.0)),
            Tilt::Tumble(n) => {
                let mut field = input.0;
                let mut fields = HashMap::new();
                let mut target: Option<Vec<Vec<RockField>>> = None;
                let mut first_index = None;
                let mut second_index = None;

                fields.insert(field.clone(), 1);

                for index in 0..n {
                    field = tumble(&field);
                    match target {
                        Some(ref t) => {
                            if t == &field {
                                second_index = Some(index);
                                break;
                            }
                        }
                        None => {
                            let count = fields.entry(field.clone()).or_insert(0);
                            *count += 1;
                            if *count == 2 {
                                target = Some(field.clone());
                                first_index = Some(index);
                            }
                        }
                    }
                }

                match (first_index, second_index) {
                    (Some(first), Some(second)) => {
                        let cycle_length = second - first;
                        let new_target = (n - second - 1) % cycle_length;
                        for _ in 0..new_target {
                            field = tumble(&field);
                        }
                        score(&field)
                    }
                    _ => score(&field),
                }
            }
        }
    }
}

fn score(field: &Vec<Vec<RockField>>) -> usize {
    field
        .into_iter()
        .rev()
        .enumerate()
        .map(|(index, row)| {
            row.into_iter()
                .filter(|patch| patch == &&RockField::MovableRock)
                .count()
                * (index + 1)
        })
        .sum()
}

fn tumble(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    fall_east(&fall_south(&fall_west(&fall_north(field))))
}

fn fall_east(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    fall(field, east_rocks)
}

fn fall_west(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    fall(field, west_rocks)
}

fn fall_north(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    transpose_field(&fall_west(&transpose_field(field)))
}

fn fall_south(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    transpose_field(&fall_east(&transpose_field(field)))
}

fn fall(
    field: &Vec<Vec<RockField>>,
    fall_direction: fn(usize, usize) -> Vec<RockField>,
) -> Vec<Vec<RockField>> {
    field
        .into_iter()
        .map(|row| {
            row.into_iter()
                .group_by(|patch| match patch {
                    RockField::Rock => false,
                    _ => true,
                })
                .into_iter()
                .flat_map(|(moving, grouping)| {
                    if moving {
                        let group = grouping.collect::<Vec<_>>();
                        let total = group.len();
                        let movable_rocks_count = group
                            .into_iter()
                            .filter(|patch| patch == &&RockField::MovableRock)
                            .count();
                        Either::Left(fall_direction(movable_rocks_count, total).into_iter())
                    } else {
                        Either::Right(grouping.cloned())
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn west_rocks(movable_rocks_count: usize, total: usize) -> Vec<RockField> {
    let mut movable_rocks = vec![RockField::MovableRock; movable_rocks_count];
    let empty = vec![RockField::Empty; total - movable_rocks_count];
    movable_rocks.extend(empty.into_iter());
    movable_rocks
}

fn east_rocks(movable_rocks_count: usize, total: usize) -> Vec<RockField> {
    let movable_rocks = vec![RockField::MovableRock; movable_rocks_count];
    let mut empty = vec![RockField::Empty; total - movable_rocks_count];
    empty.extend(movable_rocks.into_iter());
    empty
}

fn transpose_field(field: &Vec<Vec<RockField>>) -> Vec<Vec<RockField>> {
    field
        .into_iter()
        .flat_map(|row| row.into_iter().enumerate())
        .fold(BTreeMap::new(), |mut acc, (x, value)| {
            acc.entry(x)
                .or_insert_with(|| Vec::new())
                .push(value.clone());
            acc
        })
        .into_iter()
        .sorted_by(|(x1, _), (x2, _)| x1.cmp(x2))
        .map(|(_, column)| column)
        .collect()
}
