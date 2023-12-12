#![feature(lazy_cell)]
#![feature(iter_map_windows)]

mod days;
mod fetch_input;
mod libs;

use anyhow::Result;
use clap::Command as ClapCommand;
use days::{day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12};
use std::{
    cell::LazyCell,
    time::{Duration, Instant},
};

use crate::libs::{cli::Command, problem::ProblemResult};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() -> Result<()> {
    let commands: Vec<(&str, LazyCell<Box<dyn Command>>)> = vec![
        day01::DAY_01,
        day02::DAY_02,
        day03::DAY_03,
        day04::DAY_04,
        day05::DAY_05,
        day06::DAY_06,
        day07::DAY_07,
        day08::DAY_08,
        day09::DAY_09,
        day10::DAY_10,
        day11::DAY_11,
        day12::DAY_12,
    ]
    .into_iter()
    .map(|command| (command.get_name(), command))
    .collect();

    let subcommands = commands
        .iter()
        .map(|(_, command)| command.get_subcommand())
        .collect::<Vec<_>>();

    let download_command = fetch_input::command();
    let download_command_name = download_command.get_name().to_string();

    let matches = ClapCommand::new("Advent of Code 2023")
        .version(VERSION)
        .about("Run the advent of code problems from this main program")
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(download_command)
        .subcommands(subcommands)
        .get_matches();

    matches
        .subcommand_matches(&download_command_name)
        .map(fetch_input::run)
        .unwrap_or_else(|| {
            commands
                .into_iter()
                .filter_map(|(name, command)| {
                    matches.subcommand_matches(name).map(|args| {
                        println!("=============Running {:}=============", command.get_name());
                        let now = Instant::now();
                        let result = command.run(args);
                        let elapsed = now.elapsed();
                        result.map(|r| (r, elapsed))
                    })
                })
                .collect::<Result<Vec<(ProblemResult, Duration)>>>()
                .map(|results| {
                    results.into_iter().for_each(|(result, elapsed)| {
                        println!("{}", result);
                        println!("Took {:#?} to run", elapsed)
                    })
                })
        })
}
