#![feature(lazy_cell)]
#![feature(iter_map_windows)]
#![feature(let_chains)]
mod days;
mod fetch_input;
mod libs;

use crate::libs::{
    cli::{Command, PART_NAMES},
    problem::ProblemResult,
};
use anyhow::Result;
use clap::Command as ClapCommand;
use days::{
    day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13,
    day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25,
};
use std::cell::LazyCell;

#[cfg(feature = "telemetry")]
use libs::telemetry::Telemetry;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() -> Result<()> {
    #[cfg(feature = "telemetry")]
    let _telemetry = Telemetry::init_telemetry();

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
        day13::DAY_13,
        day14::DAY_14,
        day15::DAY_15,
        day16::DAY_16,
        day17::DAY_17,
        day18::DAY_18,
        day19::DAY_19,
        day20::DAY_20,
        day21::DAY_21,
        day22::DAY_22,
        day23::DAY_23,
        day24::DAY_24,
        day25::DAY_25,
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

    let all_days = commands.iter().flat_map(|(name, command)| {
        command
            .get_parts()
            .into_iter()
            .map(|part_index| (name.to_owned(), command, part_index))
            .collect::<Vec<_>>()
    });

    let all_days_command =
        ClapCommand::new("all_days").about("Runs all days in a row and gets the total time.");

    let matches = ClapCommand::new("Advent of Code 2023")
        .version(VERSION)
        .about("Run the advent of code problems from this main program")
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(download_command)
        .subcommand(all_days_command)
        .subcommands(subcommands)
        .get_matches();

    matches
        .subcommand_matches(&download_command_name)
        .map(fetch_input::run)
        .or_else(|| {
            matches.subcommand_matches("all_days").map(|_| {
                all_days
                    .map(|(day, command, part)| {
                        println!(
                            "=============Running {:}, {:}=============",
                            day, PART_NAMES[part]
                        );
                        let result = command.run_part(part);
                        result.map(|r| (r, day, part))
                    })
                    .collect::<Result<Vec<_>>>()
                    .map(|results| {
                        results.into_iter().for_each(|(result, day, part)| {
                            println!("{} {} Result: {}", day, PART_NAMES[part], result);
                        })
                    })
            })
        })
        .unwrap_or_else(|| {
            commands
                .into_iter()
                .filter_map(|(name, command)| {
                    matches.subcommand_matches(name).map(|args| {
                        println!("=============Running {:}=============", command.get_name());
                        command.run(args)
                    })
                })
                .collect::<Result<Vec<ProblemResult>>>()
                .map(|results| {
                    results.into_iter().for_each(|result| {
                        println!("{}", result);
                    })
                })
        })
}
