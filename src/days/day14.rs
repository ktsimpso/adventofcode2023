use crate::libs::{
    cli::{flag_arg, single_arg, CliArgs, CliProblem, Command},
    parse::{parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::{value_parser, Arg, ArgMatches};
use itertools::Itertools;
use ndarray::Array2;
use std::{cell::LazyCell, collections::HashMap, rc::Rc};
use tap::Pipe;

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

struct Input(Array2<RockField>);

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
        parse_table2(rock_field).map(Input)
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
            Tilt::North => score(&fall_north(input.0)),
            Tilt::Tumble(n) => {
                let mut field = Rc::from(input.0);
                let mut fields = HashMap::new();
                let mut second_index = None;
                let mut indexed_field = Vec::new();

                for index in 0..n {
                    field = Rc::from(tumble(field.as_ref().clone()));
                    if fields.contains_key(&field) {
                        second_index = Some(index);
                        break;
                    }

                    indexed_field.push(field.clone());
                    fields.insert(field.clone(), index);
                }

                match second_index {
                    Some(second) => {
                        let first = fields.get(&field).expect("exists");
                        let cycle_length = second - first;
                        let new_target = first + ((n - second - 1) % cycle_length);
                        score(indexed_field.get(new_target).expect("exists"))
                    }
                    _ => score(&field),
                }
            }
        }
    }
}

fn tumble(field: Array2<RockField>) -> Array2<RockField> {
    fall_north(field)
        .pipe(fall_west)
        .pipe(fall_south)
        .pipe(fall_east)
}

fn fall_east(mut field: Array2<RockField>) -> Array2<RockField> {
    field
        .rows_mut()
        .into_iter()
        .for_each(|row| fall(&mut row.into_iter(), east_rocks));

    field
}

fn fall_west(mut field: Array2<RockField>) -> Array2<RockField> {
    field
        .rows_mut()
        .into_iter()
        .for_each(|row| fall(&mut row.into_iter(), west_rocks));

    field
}

fn fall_north(mut field: Array2<RockField>) -> Array2<RockField> {
    field
        .columns_mut()
        .into_iter()
        .for_each(|column| fall(&mut column.into_iter(), west_rocks));

    field
}

fn fall_south(mut field: Array2<RockField>) -> Array2<RockField> {
    field
        .columns_mut()
        .into_iter()
        .for_each(|column| fall(&mut column.into_iter(), east_rocks));

    field
}

fn score(field: &Array2<RockField>) -> usize {
    let (max, _) = field.dim();
    field
        .rows()
        .into_iter()
        .enumerate()
        .map(|(index, row)| {
            row.into_iter()
                .filter(|patch| patch == &&RockField::MovableRock)
                .count()
                * (max - index)
        })
        .sum()
}

fn fall<'a, T>(
    dimension: &'a mut impl Iterator<Item = &'a mut RockField>,
    rock_fall: fn(usize, usize) -> T,
) where
    T: FnMut((usize, &mut RockField)),
{
    dimension
        .group_by(|patch| !matches!(patch, RockField::Rock))
        .into_iter()
        .filter(|(moving, _)| *moving)
        .map(|(_, section)| section.collect::<Vec<_>>())
        .for_each(|section| {
            let total = section.len();
            let movable_rocks = section
                .iter()
                .filter(|patch| ***patch == RockField::MovableRock)
                .count();
            let fall_direction = rock_fall(movable_rocks, total);
            section.into_iter().enumerate().for_each(fall_direction)
        });
}

fn west_rocks(movable_rocks: usize, _total: usize) -> impl FnMut((usize, &mut RockField)) {
    move |(index, patch)| {
        if index < movable_rocks {
            *patch = RockField::MovableRock
        } else {
            *patch = RockField::Empty
        }
    }
}

fn east_rocks(movable_rocks: usize, total: usize) -> impl FnMut((usize, &mut RockField)) {
    let spaces = total - movable_rocks;
    move |(index, patch)| {
        if index < spaces {
            *patch = RockField::Empty
        } else {
            *patch = RockField::MovableRock
        }
    }
}
