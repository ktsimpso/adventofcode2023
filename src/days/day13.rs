use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_between_blank_lines, StringParse},
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
use itertools::Itertools;
use std::{cell::LazyCell, collections::BTreeMap};

pub const DAY_13: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day13>::new(
            "day13",
            "Finds refection points in a field of terrain",
            "Several fields of terrain separated by blank lines.",
        )
        .with_part(
            "Finds the first reflection",
            CommandLineArguments { smudge: false },
        )
        .with_part(
            "Finds the second reflection, accounting for smudges",
            CommandLineArguments { smudge: true },
        ),
    )
});

struct Input(Vec<Vec<Vec<Terrain>>>);

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
enum Terrain {
    Ash,
    Rocks,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let terrain = just(".").to(Terrain::Ash).or(just("#").to(Terrain::Rocks));
        let block = terrain
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .separated_by(newline())
            .collect::<Vec<_>>();
        parse_between_blank_lines(block)
            .then_ignore(end())
            .map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "Whether to take smudges into account or not")]
    smudge: bool,
}

struct Day13 {}

impl Problem<Input, CommandLineArguments> for Day13 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .into_iter()
            .map(|board| {
                let new_board = to_u64_rows(&board);
                let transposed_board = to_u64_rows(&transpose_board(&board));
                let original_reflection_value = reflection(&new_board, false)
                    .first()
                    .map(|index| (index + 1) * 100)
                    .or_else(|| {
                        reflection(&transposed_board, false)
                            .first()
                            .map(|index| index + 1)
                    })
                    .expect("exists");
                if arguments.smudge {
                    reflection(&new_board, arguments.smudge)
                        .into_iter()
                        .map(|index| (index + 1) * 100)
                        .filter(|new| new != &original_reflection_value)
                        .next()
                        .or_else(|| {
                            reflection(&transposed_board, arguments.smudge)
                                .into_iter()
                                .map(|index| index + 1)
                                .filter(|new| new != &original_reflection_value)
                                .next()
                        })
                        .expect("Solution exists")
                } else {
                    original_reflection_value
                }
            })
            .sum()
    }
}

fn to_u64_rows(board: &Vec<Vec<Terrain>>) -> Vec<u64> {
    board
        .into_iter()
        .map(|row| {
            row
                .into_iter()
                .rev()
                .enumerate()
                .map(|(index, terrain)| match terrain {
                    Terrain::Ash => 0,
                    Terrain::Rocks => 1,
                } << index)
                .sum()
        })
        .collect()
}

fn reflection(board: &Vec<u64>, smudge: bool) -> Vec<usize> {
    board
        .into_iter()
        .map_windows(|[first, second]| [**first, **second])
        .enumerate()
        .filter(|(_, [first, second])| {
            let result = (first ^ second).count_ones();
            match smudge {
                true => result == 0 || result == 1,
                false => result == 0,
            }
        })
        .map(|(index, _)| index)
        .filter(|candidate| {
            let (left, right) = board.split_at(candidate + 1);
            let mut smudge_used = false;
            left.into_iter()
                .rev()
                .zip(right.into_iter())
                .all(|(left, right)| {
                    let result = (left ^ right).count_ones();
                    match (smudge, smudge_used, result) {
                        (true, false, 1) => {
                            smudge_used = true;
                            true
                        }
                        _ => result == 0,
                    }
                })
        })
        .collect()
}

fn transpose_board(board: &Vec<Vec<Terrain>>) -> Vec<Vec<Terrain>> {
    board
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
