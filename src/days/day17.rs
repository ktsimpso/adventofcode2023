use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection, CARDINAL_DIRECTIONS},
    parse::{parse_digit, parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, Parser};
use clap::Args;
use ndarray::{Array2, Array3};
use priority_queue::PriorityQueue;
use std::{cell::LazyCell, cmp::Reverse};

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

struct Input(Array2<usize>);

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let digit = parse_digit().to_slice().try_map(move |number: &str, span| {
            number.parse::<usize>().map_err(|op| Rich::custom(span, op))
        });
        parse_table2(digit).map(Input)
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

impl Problem<Input, CommandLineArguments> for Day17 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_x = input.0.dim().1 - 1;
        let max_y = input.0.dim().0 - 1;

        let max_straight = arguments.max;
        let min_straight = arguments.min;

        let mut queue = PriorityQueue::new();

        CARDINAL_DIRECTIONS.into_iter().for_each(|direction| {
            queue.push(
                (
                    direction,
                    BoundedPoint {
                        x: 0,
                        y: 0,
                        max_x,
                        max_y,
                    },
                ),
                Reverse(0),
            );
        });

        let mut result = None;
        let mut visited = Array3::from_elem((max_y + 1, max_x + 1, 4), false);

        while let Some(((direction, point), priority)) = queue.pop() {
            if point.x == max_x && point.y == max_y {
                result = Some(priority.0);
                break;
            }

            let has_visited = visited
                .get_mut((point.y, point.x, direction_to_index(&direction)))
                .expect("Exists");
            if *has_visited {
                continue;
            }

            *has_visited = true;

            get_valid_moves(direction, &point, &min_straight, &max_straight, &input.0)
                .filter(|(direction, point, _)| {
                    !visited
                        .get((point.y, point.x, direction_to_index(direction)))
                        .expect("Exists")
                })
                .for_each(|(direction, point, heat)| {
                    let current_priority = queue.get_priority(&(direction, point));
                    let new_priority = priority.0 + heat;
                    match current_priority {
                        Some(current) => {
                            if new_priority < current.0 {
                                queue.change_priority(&(direction, point), Reverse(new_priority));
                            }
                        }
                        None => {
                            queue.push((direction, point), Reverse(new_priority));
                        }
                    };
                });
        }

        result.expect("Path exists")
    }
}

fn direction_to_index(direction: &PointDirection) -> usize {
    match direction {
        PointDirection::Up => 0,
        PointDirection::Down => 1,
        PointDirection::Left => 2,
        PointDirection::Right => 3,
        _ => unreachable!(),
    }
}

fn get_valid_moves<'a>(
    direction: PointDirection,
    point: &'a BoundedPoint,
    min_straight: &'a usize,
    max_straight: &'a usize,
    pool: &'a Array2<usize>,
) -> impl Iterator<Item = (PointDirection, BoundedPoint, usize)> + 'a {
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
}

fn get_valid_direction<'a>(
    direction: PointDirection,
    point: &'a BoundedPoint,
    min_straight: &'a usize,
    max_straight: &'a usize,
    pool: &'a Array2<usize>,
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
                acc.push((index, (direction, point, next_heat)));
                (acc, next_heat)
            },
        )
        .0
        .into_iter()
        .filter(|(index, _)| index + 1 >= *min_straight)
        .map(|(_, value)| value)
}

fn get_heat(point: &BoundedPoint, pool: &Array2<usize>) -> Option<usize> {
    pool.get((point.y, point.x)).copied()
}
