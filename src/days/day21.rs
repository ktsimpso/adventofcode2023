use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection, CARDINAL_DIRECTIONS},
    parse::{parse_table, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::Args;
use itertools::Itertools;
use std::{
    cell::LazyCell,
    collections::{HashMap, VecDeque},
};

pub const DAY_21: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day21>::new("day21", "help", "file help")
            .with_part("day1", CommandLineArguments { steps: 64 })
            .with_part("day2", CommandLineArguments { steps: 26501365 }),
    )
});

struct Input(Vec<Vec<Terrain>>);

#[derive(Clone, PartialEq, Eq)]
enum Terrain {
    Plot,
    Rock,
    Start,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExpandiblePoint {
    point: BoundedPoint,
    vertical: isize,
    horizontal: isize,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let terrain = choice((
            just(".").to(Terrain::Plot),
            just("#").to(Terrain::Rock),
            just("S").to(Terrain::Start),
        ));
        parse_table(terrain).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "number of steps to take")]
    steps: usize,
}

struct Day21 {}

impl Problem<Input, CommandLineArguments> for Day21 {
    type Output = isize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        // TODO: make these channels and broadcast the original iterator
        let garden_walk = WalkGarden::new(&input.0);
        if arguments.steps < input.0.len() * 9 - (arguments.steps % input.0.len()) {
            garden_walk.skip(arguments.steps).next()
        } else {
            find_quadratic_cycle(garden_walk, arguments.steps, input.0.len(), 2)
        }
        .expect("Result exists")
    }
}

fn find_linear_cycle(
    input: impl Iterator<Item = isize>,
    target: usize,
    stride: usize,
    verification_steps: usize,
) -> Option<isize> {
    let result = input
        .enumerate()
        .skip(target % stride)
        .step_by(stride)
        .take_while(|(index, _)| *index < target)
        .tuple_windows()
        .map(|((index, first), (_, second))| (first, index, second - first))
        .fold_while(None, |acc, (original, index, value)| {
            let (new_original, new_index, new_value, new_count) = match acc {
                Some((last_original, last_index, last_value, count)) if last_value == value => {
                    (last_original, last_index, last_value, count + 1)
                }
                _ => (original, index, value, 0),
            };

            if new_count == verification_steps {
                itertools::FoldWhile::Done(Some((new_original, new_index, new_value, new_count)))
            } else {
                itertools::FoldWhile::Continue(Some((
                    new_original,
                    new_index,
                    new_value,
                    new_count,
                )))
            }
        });
    if result.is_done() {
        result.into_inner().map(|(original, index, value, _)| {
            original + value * (((target - index) / stride) as isize)
        })
    } else {
        None
    }
}

fn find_quadratic_cycle(
    input: impl Iterator<Item = isize>,
    target: usize,
    stride: usize,
    verification_steps: usize,
) -> Option<isize> {
    let result = input
        .enumerate()
        .skip(target % stride)
        .step_by(stride)
        .take_while(|(index, _)| *index < target)
        .tuple_windows()
        .map(|((index, first), (_, second))| (first, index, second - first))
        .tuple_windows()
        .map(|((top, index, first), (_, _, second))| (top, first, index, second - first))
        .fold_while(None, |acc, (original, first, index, value)| {
            let (new_original, new_first, new_index, new_value, new_count) = match acc {
                Some((last_original, last_first, last_index, last_value, count))
                    if last_value == value =>
                {
                    (last_original, last_first, last_index, last_value, count + 1)
                }
                _ => (original, first, index, value, 0),
            };

            if new_count == verification_steps {
                itertools::FoldWhile::Done(Some((
                    new_original,
                    new_first,
                    new_index,
                    new_value,
                    new_count,
                )))
            } else {
                itertools::FoldWhile::Continue(Some((
                    new_original,
                    new_first,
                    new_index,
                    new_value,
                    new_count,
                )))
            }
        });
    if result.is_done() {
        result
            .into_inner()
            .map(|(original, first, index, value, _)| {
                let x = ((target - index) / stride) as isize;
                original + first * x + (x * (x - 1) * value) / 2
            })
    } else {
        None
    }
}

struct WalkGarden<'a> {
    queue: VecDeque<(ExpandiblePoint, usize)>,
    visited: HashMap<ExpandiblePoint, usize>,
    max: usize,
    odd_count: isize,
    even_count: isize,
    field: &'a Vec<Vec<Terrain>>,
}

impl<'a> Iterator for WalkGarden<'a> {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, distance)) = self.queue.pop_front() {
            if self.visited.contains_key(&node) {
                continue;
            }

            if distance % 2 == 0 {
                self.even_count += 1;
            } else {
                self.odd_count += 1;
            }

            get_valid_transitions(&node, self.field)
                .into_iter()
                .filter(|new| !self.visited.contains_key(new))
                .for_each(|new_node| self.queue.push_back((new_node, distance + 1)));

            self.visited.insert(node, distance);

            if distance > self.max {
                self.max = distance;
                return Some(if (self.max - 1) % 2 == 0 {
                    self.even_count
                } else {
                    self.odd_count
                });
            }
        }

        None
    }
}

impl<'a> WalkGarden<'a> {
    fn new(field: &'a Vec<Vec<Terrain>>) -> Self {
        let max_y = field.len() - 1;
        let max_x = field.first().map(|row| row.len()).unwrap_or(0) - 1;
        let start = field
            .iter()
            .enumerate()
            .find_map(|(y, row)| {
                row.into_iter().enumerate().find_map(|(x, plot)| {
                    if plot == &Terrain::Start {
                        Some(ExpandiblePoint {
                            point: BoundedPoint { x, y, max_x, max_y },
                            vertical: 0,
                            horizontal: 0,
                        })
                    } else {
                        None
                    }
                })
            })
            .expect("Start exists");

        let mut queue = VecDeque::new();
        queue.push_back((start, 0usize));
        let visited = HashMap::new();

        let max = 0;
        let odd_count: isize = 0;
        let even_count: isize = 0;

        WalkGarden {
            queue,
            visited,
            max,
            odd_count,
            even_count,
            field,
        }
    }
}

fn get_valid_transitions(
    point: &ExpandiblePoint,
    field: &Vec<Vec<Terrain>>,
) -> Vec<ExpandiblePoint> {
    CARDINAL_DIRECTIONS
        .into_iter()
        .map(|direction| (direction, point.point.get_adjacent_wrapping(&direction)))
        .zip(
            CARDINAL_DIRECTIONS
                .into_iter()
                .map(|direction| point.point.get_adjacent(&direction)),
        )
        .filter(|((_, wrapping), _)| {
            match get_terrain_from_point(wrapping, field).expect("Exists") {
                Terrain::Rock => false,
                _ => true,
            }
        })
        .map(|((direction, wrapping), bounded)| match bounded {
            Some(_) => ExpandiblePoint {
                point: wrapping,
                ..*point
            },
            None => match direction {
                PointDirection::Up => ExpandiblePoint {
                    point: wrapping,
                    vertical: point.vertical - 1,
                    ..*point
                },
                PointDirection::Down => ExpandiblePoint {
                    point: wrapping,
                    vertical: point.vertical + 1,
                    ..*point
                },
                PointDirection::Left => ExpandiblePoint {
                    point: wrapping,
                    horizontal: point.horizontal - 1,
                    ..*point
                },
                PointDirection::Right => ExpandiblePoint {
                    point: wrapping,
                    horizontal: point.horizontal + 1,
                    ..*point
                },
                _ => unreachable!(),
            },
        })
        .collect()
}

fn get_terrain_from_point(point: &BoundedPoint, field: &Vec<Vec<Terrain>>) -> Option<Terrain> {
    field.get(point.y).and_then(|row| row.get(point.x).cloned())
}
