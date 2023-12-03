use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::{Args, ValueEnum};
use std::cell::LazyCell;

pub const DAY_02: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day02>::new("day02", "Finds out information about the 3 color cube game.", "Each line should have a game id followed by a number of semicolon separated draws of the game")
            .with_part("Sums the game ids of valid games", CommandLineArguments { stat: GameStat::Valid})
            .with_part("Sums the power of each game", CommandLineArguments { stat: GameStat::Power}),
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

        parse_lines(game_id.then(draws).map(|(id, draws)| Game { id, draws })).map(Input)
    }
}

#[derive(ValueEnum, Clone)]
enum GameStat {
    Valid,
    Power,
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The stat to look for")]
    stat: GameStat,
}

struct Day02 {}

impl Problem<Input, CommandLineArguments> for Day02 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let input = input.0.iter();
        match arguments.stat {
            GameStat::Valid => input
                .filter(|game| valid_game(game))
                .map(|game| game.id)
                .sum(),
            GameStat::Power => input.map(|game| game_power(game)).sum(),
        }
    }
}

fn game_power(game: &Game) -> usize {
    let blue = min_possible(game, &Color::Blue);
    let red = min_possible(game, &Color::Red);
    let green = min_possible(game, &Color::Green);
    blue * red * green
}

fn min_possible(game: &Game, target_color: &Color) -> usize {
    game.draws
        .iter()
        .flat_map(|draw| draw.iter().filter(|(_, color)| color == target_color))
        .map(|(count, _)| *count)
        .max()
        .unwrap_or(0)
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
