use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_usize, StringParse},
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

pub const DAY_06: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day06>::new("day06", "Finds the product of the sum of all boat racing stratigies that could beat the record distance for that race.", "One line of race times followed by one line of record distances.")
            .with_part("Finds the solution for all races", CommandLineArguments { merge: false })
            .with_part("Merges all times and distances together and finds the solution.", CommandLineArguments { merge: true }),
    )
});

struct Input(Vec<Race>);

struct Race {
    time: usize,
    distance: usize,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let spaces = just(" ").repeated().at_least(1);
        let time = just("Time:")
            .ignored()
            .then_ignore(spaces)
            .ignore_then(parse_usize().separated_by(spaces).collect::<Vec<_>>());
        let distance = just("Distance:")
            .ignored()
            .then_ignore(spaces)
            .ignore_then(parse_usize().separated_by(spaces).collect::<Vec<_>>());
        time.then_ignore(newline())
            .then(distance)
            .map(|(times, distances)| {
                times
                    .into_iter()
                    .zip(distances)
                    .map(|(time, distance)| Race { time, distance })
                    .collect()
            })
            .then_ignore(newline())
            .then_ignore(end())
            .map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "Merge all races together into one number")]
    merge: bool,
}

struct Day06 {}

impl Problem<Input, CommandLineArguments> for Day06 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        if arguments.merge {
            merge_races(&input.0)
        } else {
            input.0
        }
        .into_iter()
        .map(|race| {
            let (low, high) = find_roots(race.time, race.distance);
            high - low + 1
        })
        .product()
    }
}

fn merge_races(races: &[Race]) -> Vec<Race> {
    let (time, distance) = races
        .iter()
        .map(|race| (race.time.to_string(), race.distance.to_string()))
        .fold(
            ("".to_string(), "".to_string()),
            |(mut time_acc, mut distance_acc), (time, distance)| {
                time_acc.push_str(&time);
                distance_acc.push_str(&distance);
                (time_acc, distance_acc)
            },
        );
    vec![Race {
        time: time.parse::<usize>().expect("Valid time"),
        distance: distance.parse::<usize>().expect("Valid distance"),
    }]
}

fn compute_distance_for_held_time(time: usize, hold: usize) -> usize {
    hold * (time - hold)
}

fn find_roots(time: usize, target: usize) -> (usize, usize) {
    let descriminant = (time * time - 4 * target).integer_sqrt();
    let mut low = (time - descriminant) / 2;
    let mut high = (time + descriminant) / 2;

    // handle errors from rounding
    while compute_distance_for_held_time(time, high) <= target {
        high -= 1;
    }

    while compute_distance_for_held_time(time, low) <= target {
        low += 1;
    }

    while compute_distance_for_held_time(time, high + 1) > target {
        high += 1;
    }

    while compute_distance_for_held_time(time, low - 1) > target {
        low -= 1;
    }

    (low, high)
}
