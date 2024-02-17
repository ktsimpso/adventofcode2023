use std::{marker::PhantomData, path::PathBuf};

use anyhow::Result;
use clap::{
    builder::PathBufValueParser, Arg, ArgAction, ArgMatches, Args, Command as ClapCommand,
    FromArgMatches, ValueHint,
};
use tap::Tap;

use super::{
    file_system::file_to_string,
    parse::{StringParse, StringParser},
    problem::{Problem, ProblemResult},
};

#[cfg(feature = "telemetry")]
use super::telemetry::RunPartTelemetry;

pub trait CliArgs {
    fn get_args() -> Vec<Arg>;

    fn parse_output(args: &ArgMatches) -> Self;
}

impl<T> CliArgs for T
where
    T: Args + FromArgMatches,
{
    fn get_args() -> Vec<Arg> {
        T::augment_args(ClapCommand::new(""))
            .get_arguments()
            .cloned()
            .collect()
    }

    fn parse_output(args: &ArgMatches) -> Self {
        T::from_arg_matches(args).expect("Valid arguments")
    }
}

pub trait Command {
    fn run(&self, args: &ArgMatches) -> Result<ProblemResult>;

    fn get_parts(&self) -> Vec<usize>;

    fn run_part(&self, part: usize) -> Result<ProblemResult>;

    fn get_name(&self) -> &'static str;

    fn get_subcommand(&self) -> ClapCommand;
}

pub struct Part<T> {
    help: &'static str,
    arg: T,
}

pub struct CliProblem<I, A, P>
where
    I: StringParse,
    A: CliArgs,
    P: Problem<I, A>,
{
    name: &'static str,
    help: &'static str,
    file_help: &'static str,
    parts: Vec<Part<A>>,
    _marker: PhantomData<(I, P)>,
}

impl<I, A, P> CliProblem<I, A, P>
where
    I: StringParse,
    A: CliArgs,
    P: Problem<I, A>,
{
    pub fn new(name: &'static str, help: &'static str, file_help: &'static str) -> Self {
        CliProblem {
            name,
            help,
            file_help,
            parts: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn with_part(mut self, help: &'static str, arg: A) -> Self {
        self.parts.push(Part { help, arg });
        self
    }

    fn run_with_file_and_args(
        &self,
        file: &PathBuf,
        args: &A,
        run_value: &'static str,
    ) -> Result<ProblemResult> {
        #[cfg(feature = "telemetry")]
        let run_part = RunPartTelemetry::new(self.name, run_value);
        file_to_string(file)
            .map_err(|e| e.into())
            .and_then(|f| {
                #[cfg(feature = "telemetry")]
                let _parse = run_part.time_parse();
                StringParser::<I>::try_from(f)
            })
            .map(|input| {
                #[cfg(feature = "telemetry")]
                let _run = run_part.time_run();
                P::run(input.0, args).into()
            })
    }
}

pub static PART_NAMES: [&str; 2] = ["part1", "part2"];

impl<I, A, P> Command for CliProblem<I, A, P>
where
    I: StringParse,
    A: CliArgs,
    P: Problem<I, A>,
{
    fn run(&self, args: &ArgMatches) -> Result<ProblemResult> {
        self.parts
            .iter()
            .enumerate()
            .map(|(i, _)| (PART_NAMES[i], i))
            .find_map(|(name, part)| args.subcommand_matches(name).map(|_| self.run_part(part)))
            .unwrap_or_else(|| {
                self.run_with_file_and_args(
                    args.get_one::<PathBuf>("file").expect("File is required"),
                    &A::parse_output(args),
                    "custom",
                )
            })
    }

    fn get_parts(&self) -> Vec<usize> {
        self.parts
            .iter()
            .enumerate()
            .map(|(index, _)| index)
            .collect()
    }

    fn run_part(&self, part_index: usize) -> Result<ProblemResult> {
        let part = &self.parts[part_index];
        self.run_with_file_and_args(
            &PathBuf::new().tap_mut(|path| path.push(format!("input/{}/input.txt", self.name))),
            &part.arg,
            PART_NAMES[part_index],
        )
    }

    fn get_name(&self) -> &'static str {
        self.name
    }

    fn get_subcommand(&self) -> ClapCommand {
        self.parts.iter().enumerate().fold(
            ClapCommand::new(self.name)
                .about(self.help)
                .arg_required_else_help(true)
                .subcommand_negates_reqs(true)
                .arg(file_arg(self.file_help))
                .args(A::get_args()),
            |command, (count, part)| {
                command.subcommand(ClapCommand::new(PART_NAMES[count]).about(part.help))
            },
        )
    }
}

fn file_arg(help: &str) -> Arg {
    single_arg("file", 'f', help)
        .value_hint(ValueHint::FilePath)
        .value_parser(PathBufValueParser::new())
}

pub fn single_arg(name: &'static str, short: char, help: &str) -> Arg {
    Arg::new(name)
        .short(short)
        .long(name)
        .num_args(1)
        .help(help.to_string())
        .required(true)
        .action(ArgAction::Set)
        .value_name(name.to_ascii_uppercase())
}

pub fn flag_arg(name: &'static str, short: char, help: &str) -> Arg {
    Arg::new(name)
        .short(short)
        .long(name)
        .help(help.to_string())
        .num_args(0)
        .action(ArgAction::SetTrue)
}
