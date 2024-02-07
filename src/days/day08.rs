use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{any, just},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use num_integer::Integer;
use std::{cell::LazyCell, collections::BTreeMap, rc::Rc};
use tap::Tap;

pub const DAY_08: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day08>::new(
            "day08",
            "Finds the route from the start to the finish",
            "The left/right steps to take followed by mappings between nodes.",
        )
        .with_part(
            "Finds the number of steps from AAA to the finsh",
            CommandLineArguments { all: false },
        )
        .with_part(
            "Finds the number of steps from all A nodes that would finish at the same time.",
            CommandLineArguments { all: true },
        ),
    )
});

#[derive(Debug)]
struct Input {
    directions: Vec<Direction>,
    map: BTreeMap<Rc<str>, (Rc<str>, Rc<str>)>,
}

#[derive(Clone, Debug)]
enum Direction {
    Left,
    Right,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let direction = just("L")
            .to(Direction::Left)
            .or(just("R").to(Direction::Right));
        let token = any().repeated().exactly(3).to_slice();
        let map = token
            .then_ignore(just(" = ("))
            .then(token)
            .then_ignore(just(", "))
            .then(token)
            .then_ignore(just(")"))
            .map(|((key, v1), v2): ((&str, &str), &str)| {
                (Rc::from(key), (Rc::from(v1), Rc::from(v2)))
            });
        direction
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then_ignore(newline().repeated().exactly(2))
            .then(parse_lines(map))
            .map(|(directions, map)| {
                let result_map = map.into_iter().fold(BTreeMap::new(), |acc, (key, value)| {
                    acc.tap_mut(|m| {
                        m.insert(key, value);
                    })
                });
                Input {
                    directions,
                    map: result_map,
                }
            })
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "Whether to find all items with A or just AAA")]
    all: bool,
}

struct Day08 {}

impl Problem<Input, CommandLineArguments> for Day08 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let directions = input.directions.into_iter().cycle();
        let mut current = if arguments.all {
            input
                .map
                .keys()
                .filter(|key| key.ends_with('A'))
                .cloned()
                .collect::<Vec<_>>()
        } else {
            vec![Rc::from("AAA")]
        };

        let initial = current.clone();
        let mut found = vec![false; initial.len()];
        let mut found_step = Vec::new();

        for (steps, direction) in directions.enumerate() {
            current = current
                .into_iter()
                .map(|c| {
                    let mapping = input.map.get(&c).expect("Mapping exists");
                    match direction {
                        Direction::Left => mapping.0.clone(),
                        Direction::Right => mapping.1.clone(),
                    }
                })
                .collect::<Vec<_>>();

            initial
                .iter()
                .zip(current.iter())
                .zip(found.iter_mut())
                .for_each(|((_, c), been_found)| {
                    if c.ends_with('Z') && !*been_found {
                        *been_found = true;
                        found_step.push(steps + 1);
                    }
                });
            if found.iter().all(|c| *c) {
                break;
            }

            if current.iter().all(|c| c.ends_with('Z')) {
                break;
            }
        }

        found_step.into_iter().fold(1, |acc, step| acc.lcm(&step))
    }
}
