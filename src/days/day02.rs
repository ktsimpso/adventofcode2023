use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{end, just, one_of},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use itertools::Itertools;
use std::cell::LazyCell;

pub const DAY_02: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day02>::new("day02", "Finds out information about the 3 color cube game.", "Each line should have a game id followed by a number of semicolon separated draws of the game")
            .with_part("Sums the game ids of valid games", CommandLineArguments {})
            ,
    )
});

struct Input(Vec<Game>);

struct Game {
    id: usize,
    draws: Vec<Vec<(usize, Color)>>,
}

#[derive(Clone, PartialEq, Eq)]
enum Color {
    Red,
    Green,
    Blue,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let game_id = just("Game ")
            .ignore_then(parse_usize())
            .then_ignore(just(": "));
        let blue = just("blue").to(Color::Blue);
        let red = just("red").to(Color::Red);
        let green = just("green").to(Color::Green);
        let color = red.or(green).or(blue);
        let draw = parse_usize()
            .then_ignore(just(" "))
            .then(color)
            .separated_by(just(", "))
            .collect::<Vec<(usize, Color)>>();
        let draws = draw.separated_by(just("; ")).collect::<Vec<_>>();

        parse_lines(game_id.then(draws).map(|(id, draws)| Game { id, draws }), 0)
            .then_ignore(newline().or_not())
            .then_ignore(end())
            .map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {}

struct Day02 {}

impl Problem<Input, CommandLineArguments> for Day02 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .iter()
            .filter(|game| valid_game(game))
            .map(|game| game.id)
            .sum()
    }
}

fn valid_game(game: &Game) -> bool {
    game.draws.iter().all(|draw| {
        draw.iter().all(|(count, color)| match color {
            Color::Red => count <= &12,
            Color::Green => count <= &13,
            Color::Blue => count <= &14,
        })
    })
}
