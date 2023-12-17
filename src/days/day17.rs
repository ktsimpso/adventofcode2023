use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection, CARDINAL_DIRECTIONS},
    parse::{parse_digit, parse_table, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, Parser};
use clap::Args;
use priority_queue::PriorityQueue;
use std::{cell::LazyCell, cmp::Ordering, collections::HashSet};

pub const DAY_17: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day17>::new(
            "day17",
            "Finds the least costly path from the start to the end",
            "The lava pool with costs each item is 1 digit",
        )
        .with_part(
            "You can only travel straight a maxium of 3 and a minimum of 1",
            CommandLineArguments { max: 3, min: 1 },
        )
        .with_part(
            "You can only travel straight a maxium of 10 and a minimum of 4",
            CommandLineArguments { max: 10, min: 4 },
        ),
    )
});

struct Input(Vec<Vec<usize>>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let digit = parse_digit().to_slice().try_map(move |number: &str, span| {
            usize::from_str_radix(number, 10).map_err(|op| Rich::custom(span, op))
        });
        parse_table(digit).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short = 'M', long, help = "The maximum length you can go straight")]
    max: usize,
    #[arg(short = 'm', long, help = "The minimum length you can go straight")]
    min: usize,
}

struct Day17 {}

#[derive(PartialEq, Eq, Debug)]
struct Path(Option<usize>);

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.0, other.0) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => b.cmp(&a),
        }
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Problem<Input, CommandLineArguments> for Day17 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_y = input.0.len() - 1;
        let max_x = input.0.first().map(|row| row.len()).unwrap_or(0) - 1;

        let max_straight = arguments.max;
        let min_straight = arguments.min;

        let mut unvisited = input
            .0
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.into_iter().enumerate().flat_map(move |(x, _)| {
                    CARDINAL_DIRECTIONS
                        .into_iter()
                        .map(move |direction| (direction, BoundedPoint { x, y, max_x, max_y }))
                })
            })
            .collect::<HashSet<_>>();

        let mut distance = unvisited
            .clone()
            .into_iter()
            .map(|(direction, point)| ((direction, point), Path(None)))
            .collect::<PriorityQueue<_, _>>();

        CARDINAL_DIRECTIONS.into_iter().for_each(|direction| {
            distance.change_priority(
                &(
                    direction,
                    BoundedPoint {
                        x: 0,
                        y: 0,
                        max_x,
                        max_y,
                    },
                ),
                Path(Some(0)),
            );
        });

        let mut result = None;

        while let Some(((direction, point), priority)) = distance.pop() {
            if !unvisited.remove(&(direction, point)) {
                continue;
            }

            if point.x == max_x && point.y == max_y {
                result = priority.0;
                break;
            }

            if priority.0.is_none() {
                break;
            }

            get_valid_moves(direction, &point, &min_straight, &max_straight, &input.0)
                .into_iter()
                .filter(|(direction, point, _)| unvisited.contains(&(*direction, *point)))
                .for_each(|(direction, point, heat)| {
                    let current_priority = distance
                        .get_priority(&(direction, point))
                        .expect("Priority exists");
                    let new_priority = Path(match priority.0 {
                        Some(current) => Some(current + heat),
                        None => Some(heat),
                    });
                    if &new_priority > current_priority {
                        distance.change_priority(&(direction, point), new_priority);
                    };
                });
        }

        result.expect("Path exists")
    }
}

fn get_valid_moves(
    direction: PointDirection,
    point: &BoundedPoint,
    min_straight: &usize,
    max_straight: &usize,
    pool: &Vec<Vec<usize>>,
) -> Vec<(PointDirection, BoundedPoint, usize)> {
    get_valid_direction(
        direction.get_clockwise(),
        point,
        min_straight,
        max_straight,
        pool,
    )
    .chain(get_valid_direction(
        direction.get_counter_clockwise(),
        point,
        min_straight,
        max_straight,
        pool,
    ))
    .collect()
}

fn get_valid_direction<'a>(
    direction: PointDirection,
    point: &'a BoundedPoint,
    min_straight: &'a usize,
    max_straight: &'a usize,
    pool: &'a Vec<Vec<usize>>,
) -> impl Iterator<Item = (PointDirection, BoundedPoint, usize)> + 'a {
    point
        .into_iter_direction(direction)
        .enumerate()
        .take_while(|(index, _)| index < max_straight)
        .map(|(index, point)| (index, point, get_heat(&point, pool).expect("Valid point")))
        .fold(
            (Vec::new(), 0),
            |(mut acc, acc_heat), (index, point, heat)| {
                let next_heat = acc_heat + heat;
                acc.push((index, (direction.clone(), point, next_heat)));
                (acc, next_heat)
            },
        )
        .0
        .into_iter()
        .filter(|(index, _)| index + 1 >= *min_straight)
        .map(|(_, value)| value)
}

fn get_heat(point: &BoundedPoint, pool: &Vec<Vec<usize>>) -> Option<usize> {
    pool.get(point.y)
        .and_then(|row| row.get(point.x).map(|value| *value))
}
