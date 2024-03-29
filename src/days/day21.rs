use crate::libs::{
    cli::{CliProblem, Command},
    graph::{PointDirection, CARDINAL_DIRECTIONS},
    parse::{parse_table2, StringParse},
    problem::Problem,
};
use ahash::AHashSet;
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::Args;
use itertools::Itertools;
use ndarray::Array2;
use std::{cell::LazyCell, collections::VecDeque};

pub const DAY_21: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day21>::new("day21", "help", "file help")
            .with_part("day1", CommandLineArguments { steps: 64 })
            .with_part("day2", CommandLineArguments { steps: 26501365 }),
    )
});

struct Input(Array2<Terrain>);

#[derive(Clone, PartialEq, Eq)]
enum Terrain {
    Plot,
    Rock,
    Start,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExpandiblePoint {
    x: usize,
    y: usize,
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
        parse_table2(terrain).map(Input)
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
        let mut garden_walk = WalkGarden::new(&input.0);
        if arguments.steps < input.0.dim().0 * 8 - (arguments.steps % input.0.dim().0) {
            garden_walk.nth(arguments.steps)
        } else {
            find_quadratic_cycle(garden_walk, arguments.steps, input.0.dim().0, 1)
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
    visited: AHashSet<ExpandiblePoint>,
    max: usize,
    odd_count: isize,
    even_count: isize,
    field: &'a Array2<Terrain>,
    max_x: usize,
    max_y: usize,
}

impl<'a> Iterator for WalkGarden<'a> {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, distance)) = self.queue.pop_front() {
            if self.visited.contains(&node) {
                continue;
            }

            if distance % 2 == 0 {
                self.even_count += 1;
            } else {
                self.odd_count += 1;
            }

            get_valid_transitions(&node, self.field, self.max_x, self.max_y)
                .filter(|new| !self.visited.contains(new))
                .for_each(|new_node| self.queue.push_back((new_node, distance + 1)));

            self.visited.insert(node);

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
    fn new(field: &'a Array2<Terrain>) -> Self {
        let max_x = field.dim().1 - 1;
        let max_y = field.dim().0 - 1;
        let start = field
            .indexed_iter()
            .find_map(|((y, x), plot)| {
                if plot == &Terrain::Start {
                    Some(ExpandiblePoint {
                        x,
                        y,
                        vertical: 0,
                        horizontal: 0,
                    })
                } else {
                    None
                }
            })
            .expect("Start exists");

        let mut queue = VecDeque::new();
        queue.push_back((start, 0usize));
        let visited = AHashSet::new();

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
            max_x,
            max_y,
        }
    }
}

fn wrapping_bounded_increase(item: usize, max: usize) -> (usize, bool) {
    if item == max {
        (0, true)
    } else {
        (item + 1, false)
    }
}

fn wrapping_bounded_decrease(item: usize, max: usize) -> (usize, bool) {
    if item == 0 {
        (max, true)
    } else {
        (item - 1, false)
    }
}

fn get_valid_transitions<'a>(
    point: &'a ExpandiblePoint,
    field: &'a Array2<Terrain>,
    max_x: usize,
    max_y: usize,
) -> impl Iterator<Item = ExpandiblePoint> + 'a {
    CARDINAL_DIRECTIONS
        .into_iter()
        .map(move |direction| match direction {
            PointDirection::Up => {
                let (y, wrapped) = wrapping_bounded_decrease(point.y, max_y);
                let vertical = if wrapped {
                    point.vertical - 1
                } else {
                    point.vertical
                };

                ExpandiblePoint {
                    y,
                    vertical,
                    ..*point
                }
            }
            PointDirection::Down => {
                let (y, wrapped) = wrapping_bounded_increase(point.y, max_y);
                let vertical = if wrapped {
                    point.vertical + 1
                } else {
                    point.vertical
                };

                ExpandiblePoint {
                    y,
                    vertical,
                    ..*point
                }
            }
            PointDirection::Left => {
                let (x, wrapped) = wrapping_bounded_decrease(point.x, max_x);
                let horizontal = if wrapped {
                    point.horizontal - 1
                } else {
                    point.horizontal
                };

                ExpandiblePoint {
                    x,
                    horizontal,
                    ..*point
                }
            }
            PointDirection::Right => {
                let (x, wrapped) = wrapping_bounded_increase(point.x, max_x);
                let horizontal = if wrapped {
                    point.horizontal + 1
                } else {
                    point.horizontal
                };

                ExpandiblePoint {
                    x,
                    horizontal,
                    ..*point
                }
            }
            _ => unreachable!(),
        })
        .filter(|new_point| {
            !matches!(
                field.get((new_point.x, new_point.y)).expect("Exists"),
                Terrain::Rock
            )
        })
}
