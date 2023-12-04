use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::{Args, ValueEnum};
use std::{cell::LazyCell, collections::HashMap};

pub const DAY_04: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day04>::new(
            "day04",
            "Finds information about winning scratch cards",
            "Each line should have one scratch card.",
        )
        .with_part(
            "Finds the score for each scratch card and sums the values.",
            CommandLineArguments {
                rules: GameRules::Score,
            },
        )
        .with_part(
            "Finds the total number of scratch cards won.",
            CommandLineArguments {
                rules: GameRules::Cards,
            },
        ),
    )
});

struct Input(Vec<Card>);

struct Card {
    number: usize,
    winning_numbers: Vec<usize>,
    match_numbers: Vec<usize>,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let spaces = just(" ").repeated().at_least(1);
        let card = just("Card")
            .then_ignore(spaces)
            .ignore_then(parse_usize())
            .then_ignore(just(":").then(spaces))
            .then(parse_usize().separated_by(spaces).collect::<Vec<_>>())
            .then_ignore(just(" |").then_ignore(spaces))
            .then(parse_usize().separated_by(spaces).collect::<Vec<_>>())
            .map(|((number, winning_numbers), match_numbers)| Card {
                number,
                winning_numbers,
                match_numbers,
            });
        parse_lines(card).map(Input)
    }
}

#[derive(ValueEnum, Clone)]
enum GameRules {
    Score,
    Cards,
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The game rules to use")]
    rules: GameRules,
}

struct Day04 {}

impl Problem<Input, CommandLineArguments> for Day04 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        match arguments.rules {
            GameRules::Score => input.0.into_iter().map(|card| count_points(&card)).sum(),
            GameRules::Cards => {
                let mut card_map = HashMap::new();
                input.0.into_iter().for_each(|card| {
                    let matches = count_matches(&card);
                    let current_card_count = card_map.entry(card.number).or_insert(0usize);
                    *current_card_count += 1;
                    let current_card_count = *current_card_count;

                    for i in 1..=matches {
                        *card_map.entry(card.number + i).or_insert(0usize) += current_card_count;
                    }
                });
                card_map.values().sum()
            }
        }
    }
}

fn count_matches(card: &Card) -> usize {
    card.match_numbers
        .iter()
        .filter(|number| card.winning_numbers.contains(number))
        .count()
}

fn count_points(card: &Card) -> usize {
    card.match_numbers
        .iter()
        .filter(|number| card.winning_numbers.contains(number))
        .fold(0, |acc, _| if acc == 0 { 1 } else { acc * 2 })
}
