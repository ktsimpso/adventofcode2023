#![feature(lazy_cell)]

mod days;
mod libs;

use anyhow::Result;
use clap::Command as ClapCommand;
use days::day01;
use std::{
    cell::LazyCell,
    time::{Duration, Instant},
};

use crate::libs::{cli::Command, problem::ProblemResult};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() -> Result<()> {
    let commands: Vec<(&str, LazyCell<Box<dyn Command>>)> = vec![day01::DAY_01]
        .into_iter()
        .map(|command| (command.get_name(), command))
        .collect();

    let subcommands = commands
        .iter()
        .map(|(_, command)| command.get_subcommand())
        .collect::<Vec<_>>();

    let matches = ClapCommand::new("Advent of Code 2023")
        .version(VERSION)
        .about("Run the advent of code problems from this main program")
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommands(subcommands)
        .get_matches();

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
}
