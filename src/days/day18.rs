use crate::libs::{
    cli::{CliProblem, Command},
    graph::PointDirection,
    parse::{parse_isize, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{any, choice, just},
    Parser,
};
use clap::Args;
use itertools::Itertools;
use std::{cell::LazyCell, cmp::min, collections::HashMap, iter::once};

pub const DAY_18: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day18>::new(
            "day18",
            "Finds the total area of the dig site",
            "Instructions to dig, one instruction per tiles",
        )
        .with_part(
            "Uses the default instructions to plan the dig site",
            CommandLineArguments {
                color_length: false,
            },
        )
        .with_part(
            "Uses the color instructions to plan the dig site",
            CommandLineArguments { color_length: true },
        ),
    )
});

struct Input(Vec<(Instruction, Instruction)>);

#[derive(Clone, Debug)]
struct Instruction {
    direction: PointDirection,
    length: isize,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let direction = choice((
            just("R").to(PointDirection::Right),
            just("D").to(PointDirection::Down),
            just("L").to(PointDirection::Left),
            just("U").to(PointDirection::Up),
        ));
        let color_direction = choice((
            just("0").to(PointDirection::Right),
            just("1").to(PointDirection::Down),
            just("2").to(PointDirection::Left),
            just("3").to(PointDirection::Up),
        ));
        let color_instruction = any()
            .repeated()
            .exactly(5)
            .to_slice()
            .try_map(|hex: &str, span| {
                isize::from_str_radix(&hex.chars().skip_while(|c| c == &'0').join(""), 16)
                    .map_err(|op| Rich::custom(span, op))
            })
            .then(color_direction)
            .map(|(length, direction)| Instruction { direction, length });

        let instruction = direction
            .then_ignore(just(" "))
            .then(parse_isize())
            .then_ignore(just(" (#"))
            .then(color_instruction)
            .then_ignore(just(")"))
            .map(|((direction, length), color)| (Instruction { direction, length }, color));
        parse_lines(instruction).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "Use the hex color as the instruction instead of the original instruction"
    )]
    color_length: bool,
}

struct Day18 {}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

#[derive(Clone, PartialEq, Eq, Hash, Copy, Debug)]
enum Pipe {
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
}

impl Problem<Input, CommandLineArguments> for Day18 {
    type Output = isize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let trench_length = input
            .0
            .iter()
            .map(|c| {
                if arguments.color_length {
                    c.1.length
                } else {
                    c.0.length
                }
            })
            .sum::<isize>();
        let mut previous = if arguments.color_length {
            input.0.last().expect("At least one instruction").1.clone()
        } else {
            input.0.last().expect("At least one instruction").0.clone()
        };
        let first = if arguments.color_length {
            input.0.first().expect("At least one instruction").1.clone()
        } else {
            input.0.first().expect("At least one instruction").0.clone()
        };

        let mut previous_point = Point { x: 0, y: 0 };
        let last = previous.clone();
        let mut all_points = HashMap::new();
        let mut significant_rows = HashMap::new();
        let mut significant_columns = HashMap::new();

        input
            .0
            .into_iter()
            .map(|i| if arguments.color_length { i.1 } else { i.0 })
            .tuple_windows()
            .chain(once((last, first)))
            .for_each(|(current, next)| {
                calculate_point_information(
                    &current,
                    &next,
                    &mut previous,
                    &mut previous_point,
                    &mut significant_columns,
                    &mut significant_rows,
                    &mut all_points,
                )
            });

        let (min_y, max_y) = all_points
            .keys()
            .map(|point| point.y)
            .minmax()
            .into_option()
            .expect("At least one");

        let mut current_row = min_y;
        let mut count = 0;

        while current_row <= max_y {
            let mut in_loop = false;
            let significant_row = significant_rows.get(&current_row);
            let row_points = significant_row.into_iter().flat_map(|row| {
                row.iter().flat_map(|(p1, p2)| {
                    once((p1.x, *all_points.get(p1).expect("Pipe Exists")))
                        .chain(once((p2.x, *all_points.get(p2).expect("Pipe Exists"))))
                })
            });

            let columns = significant_columns
                .iter()
                .map(|(x, column)| {
                    (
                        x,
                        column.iter().find(|(first, second)| {
                            first.y < current_row && second.y > current_row
                        }),
                    )
                })
                .filter_map(|(x, column)| column.map(|c| (x, c)))
                .map(|(x, _)| (*x, Pipe::Vertical))
                .chain(row_points)
                .sorted_by(|(x1, _), (x2, _)| x1.cmp(x2))
                .collect::<Vec<_>>();

            let row_count =
                columns
                    .iter()
                    .tuple_windows()
                    .fold(0, |mut acc, ((x1, pipe1), (x2, pipe2))| {
                        match (pipe1, pipe2) {
                            (Pipe::Vertical, Pipe::Vertical)
                            | (Pipe::Vertical, Pipe::NorthEast)
                            | (Pipe::Vertical, Pipe::NorthWest)
                            | (Pipe::NorthWest, Pipe::Vertical)
                            | (Pipe::NorthWest, Pipe::NorthEast)
                            | (Pipe::NorthWest, Pipe::SouthWest)
                            | (Pipe::Vertical, Pipe::SouthWest) => {
                                acc += if !in_loop { *x2 - *x1 - 1 } else { 0 };
                                in_loop = !in_loop;
                            }

                            (Pipe::Vertical, Pipe::SouthEast)
                            | (Pipe::SouthWest, Pipe::Vertical)
                            | (Pipe::SouthWest, Pipe::NorthEast)
                            | (Pipe::SouthEast, Pipe::Vertical)
                            | (Pipe::SouthEast, Pipe::SouthWest)
                            | (Pipe::SouthEast, Pipe::NorthEast) => {
                                acc += if in_loop { *x2 - *x1 - 1 } else { 0 };
                            }
                            (Pipe::NorthEast, Pipe::Vertical)
                            | (Pipe::NorthEast, Pipe::NorthEast)
                            | (Pipe::NorthEast, Pipe::NorthWest)
                            | (Pipe::NorthEast, Pipe::SouthEast) => {
                                in_loop = !in_loop;
                            }
                            (Pipe::NorthEast, Pipe::SouthWest)
                            | (Pipe::SouthWest, Pipe::SouthWest)
                            | (Pipe::SouthWest, Pipe::NorthWest)
                            | (Pipe::SouthWest, Pipe::SouthEast) => {}
                            _ => unreachable!(),
                        };
                        acc
                    });

            let idenitcal_rows = if significant_row.is_none() {
                min(
                    significant_columns
                        .values()
                        .flat_map(|column| column.iter())
                        .filter_map(|(low, high)| {
                            if low.y < current_row && high.y > current_row {
                                Some(high.y)
                            } else if low.y > current_row {
                                Some(low.y)
                            } else {
                                None
                            }
                        })
                        .min()
                        .unwrap_or(max_y),
                    significant_rows
                        .keys()
                        .filter(|y| y > &&current_row)
                        .min()
                        .copied()
                        .unwrap_or(max_y),
                ) - current_row
            } else {
                1
            };

            count += row_count * idenitcal_rows;
            current_row += idenitcal_rows;
        }

        count + trench_length
    }
}

fn calculate_point_information(
    current: &Instruction,
    next: &Instruction,
    previous: &mut Instruction,
    previous_point: &mut Point,
    significant_columns: &mut HashMap<isize, Vec<(Point, Point)>>,
    significant_rows: &mut HashMap<isize, Vec<(Point, Point)>>,
    all_points: &mut HashMap<Point, Pipe>,
) {
    let next_direction = if current.length == 0 {
        next.direction
    } else {
        current.direction
    };
    let pipe = match (previous.direction, next_direction) {
        (PointDirection::Up, PointDirection::Up) | (PointDirection::Down, PointDirection::Down) => {
            Pipe::Vertical
        }
        (PointDirection::Up, PointDirection::Left)
        | (PointDirection::Right, PointDirection::Down) => Pipe::NorthWest,
        (PointDirection::Up, PointDirection::Right)
        | (PointDirection::Left, PointDirection::Down) => Pipe::NorthEast,
        (PointDirection::Down, PointDirection::Right)
        | (PointDirection::Left, PointDirection::Up) => Pipe::SouthWest,
        (PointDirection::Down, PointDirection::Left)
        | (PointDirection::Right, PointDirection::Up) => Pipe::SouthEast,
        (PointDirection::Left, PointDirection::Left)
        | (PointDirection::Right, PointDirection::Right) => Pipe::Horizontal,
        _ => unreachable!(),
    };

    let (dx, dy) = match previous.direction {
        PointDirection::Up => (0, -previous.length),
        PointDirection::Down => (0, previous.length),
        PointDirection::Left => (-previous.length, 0),
        PointDirection::Right => (previous.length, 0),
        _ => unreachable!(),
    };

    let current_point = Point {
        x: previous_point.x + dx,
        y: previous_point.y + dy,
    };

    match previous.direction {
        PointDirection::Up => {
            let column = significant_columns.entry(previous_point.x).or_default();
            column.push((current_point, *previous_point));
        }
        PointDirection::Down => {
            let column = significant_columns.entry(previous_point.x).or_default();
            column.push((*previous_point, current_point));
        }
        PointDirection::Left => {
            let row = significant_rows.entry(previous_point.y).or_default();
            row.push((current_point, *previous_point));
        }
        PointDirection::Right => {
            let row = significant_rows.entry(previous_point.y).or_default();
            row.push((*previous_point, current_point));
        }
        _ => unreachable!(),
    }

    all_points.insert(current_point, pipe);

    *previous = current.clone();
    *previous_point = current_point;
}
