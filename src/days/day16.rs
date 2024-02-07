use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection},
    parse::{parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::Args;
use either::Either;
use ndarray::Array2;
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{
    cell::LazyCell,
    collections::{HashSet, VecDeque},
    iter::once,
    usize,
};

pub const DAY_16: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day16>::new(
            "day16",
            "Finds the max number of tiles energized when a light is shown",
            "The field of tiles with contraptions",
        )
        .with_part(
            "The light shines from the top left going right",
            CommandLineArguments {
                all_directions: false,
            },
        )
        .with_part(
            "The light shines from all directions",
            CommandLineArguments {
                all_directions: true,
            },
        ),
    )
});

struct Input(Array2<Contraption>);

#[derive(Clone, Copy)]
enum Contraption {
    Empty,
    Vertical,
    Horizontal,
    BackSlash,
    ForwardSlash,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let contraption = choice((
            just(".").to(Contraption::Empty),
            just("|").to(Contraption::Vertical),
            just("-").to(Contraption::Horizontal),
            just("\\").to(Contraption::BackSlash),
            just("/").to(Contraption::ForwardSlash),
        ));
        parse_table2(contraption).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "Whether to fire the light from all directions or not"
    )]
    all_directions: bool,
}

struct Day16 {}

impl Problem<Input, CommandLineArguments> for Day16 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_x = input.0.dim().1 - 1;
        let max_y = input.0.dim().0 - 1;

        if arguments.all_directions {
            Either::Left(input.0.indexed_iter().flat_map(|((y, x), _)| match (x, y) {
                (x, y) if x == 0 && y == 0 => vec![
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Right),
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Down),
                ],
                (x, y) if x == max_x && y == max_y => vec![
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Right),
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Up),
                ],
                (x, y) if x == 0 && y == max_y => vec![
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Right),
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Up),
                ],
                (x, y) if x == max_x && y == 0 => vec![
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Left),
                    (BoundedPoint { x, y, max_x, max_y }, PointDirection::Down),
                ],
                (x, y) if x == 0 => {
                    vec![(BoundedPoint { x, y, max_x, max_y }, PointDirection::Right)]
                }
                (x, y) if y == 0 => {
                    vec![(BoundedPoint { x, y, max_x, max_y }, PointDirection::Down)]
                }
                (x, y) if x == max_x => {
                    vec![(BoundedPoint { x, y, max_x, max_y }, PointDirection::Left)]
                }
                (x, y) if y == max_y => {
                    vec![(BoundedPoint { x, y, max_x, max_y }, PointDirection::Up)]
                }
                _ => Vec::new(),
            }))
        } else {
            Either::Right(once((
                BoundedPoint {
                    x: 0,
                    y: 0,
                    max_x,
                    max_y,
                },
                PointDirection::Right,
            )))
        }
        .par_bridge()
        .map(|(point, direction)| energized_tiles(point, direction, &input.0))
        .max()
        .unwrap_or(0)
    }
}

fn energized_tiles(
    initial_point: BoundedPoint,
    initial_direction: PointDirection,
    area: &Array2<Contraption>,
) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back((initial_direction, initial_point));
    let mut visited = HashSet::new();

    while let Some((current_direction, point)) = queue.pop_front() {
        if visited.contains(&(point, current_direction)) {
            continue;
        }
        visited.insert((point, current_direction));
        let contraption = get_contraption(&point, area).expect("Exists");
        forward_beam(point, current_direction, contraption)
            .into_iter()
            .for_each(|(point, direction)| queue.push_back((point, direction)));
    }
    visited
        .into_iter()
        .map(|(point, _)| point)
        .collect::<HashSet<_>>()
        .len()
}

fn forward_beam(
    point: BoundedPoint,
    direction: PointDirection,
    contraption: Contraption,
) -> Vec<(PointDirection, BoundedPoint)> {
    match (direction, contraption) {
        (direction, Contraption::Empty) => point
            .get_adjacent(&direction)
            .into_iter()
            .map(|point| (direction, point))
            .collect(),
        (PointDirection::Up, Contraption::Vertical) => point
            .get_adjacent(&PointDirection::Up)
            .into_iter()
            .map(|point| (PointDirection::Up, point))
            .collect(),
        (PointDirection::Up, Contraption::Horizontal) => point
            .get_adjacent(&PointDirection::Left)
            .into_iter()
            .map(|point| (PointDirection::Left, point))
            .chain(
                point
                    .get_adjacent(&PointDirection::Right)
                    .map(|point| (PointDirection::Right, point)),
            )
            .collect(),
        (PointDirection::Up, Contraption::BackSlash) => point
            .get_adjacent(&PointDirection::Left)
            .into_iter()
            .map(|point| (PointDirection::Left, point))
            .collect(),
        (PointDirection::Up, Contraption::ForwardSlash) => point
            .get_adjacent(&PointDirection::Right)
            .into_iter()
            .map(|point| (PointDirection::Right, point))
            .collect(),
        (PointDirection::Down, Contraption::Vertical) => point
            .get_adjacent(&PointDirection::Down)
            .into_iter()
            .map(|point| (PointDirection::Down, point))
            .collect(),
        (PointDirection::Down, Contraption::Horizontal) => point
            .get_adjacent(&PointDirection::Left)
            .into_iter()
            .map(|point| (PointDirection::Left, point))
            .chain(
                point
                    .get_adjacent(&PointDirection::Right)
                    .map(|point| (PointDirection::Right, point)),
            )
            .collect(),
        (PointDirection::Down, Contraption::BackSlash) => point
            .get_adjacent(&PointDirection::Right)
            .into_iter()
            .map(|point| (PointDirection::Right, point))
            .collect(),
        (PointDirection::Down, Contraption::ForwardSlash) => point
            .get_adjacent(&PointDirection::Left)
            .into_iter()
            .map(|point| (PointDirection::Left, point))
            .collect(),
        (PointDirection::Left, Contraption::Vertical) => point
            .get_adjacent(&PointDirection::Up)
            .into_iter()
            .map(|point| (PointDirection::Up, point))
            .chain(
                point
                    .get_adjacent(&PointDirection::Down)
                    .map(|point| (PointDirection::Down, point)),
            )
            .collect(),
        (PointDirection::Left, Contraption::Horizontal) => point
            .get_adjacent(&PointDirection::Left)
            .into_iter()
            .map(|point| (PointDirection::Left, point))
            .collect(),
        (PointDirection::Left, Contraption::BackSlash) => point
            .get_adjacent(&PointDirection::Up)
            .into_iter()
            .map(|point| (PointDirection::Up, point))
            .collect(),
        (PointDirection::Left, Contraption::ForwardSlash) => point
            .get_adjacent(&PointDirection::Down)
            .into_iter()
            .map(|point| (PointDirection::Down, point))
            .collect(),
        (PointDirection::Right, Contraption::Vertical) => point
            .get_adjacent(&PointDirection::Up)
            .into_iter()
            .map(|point| (PointDirection::Up, point))
            .chain(
                point
                    .get_adjacent(&PointDirection::Down)
                    .map(|point| (PointDirection::Down, point)),
            )
            .collect(),
        (PointDirection::Right, Contraption::Horizontal) => point
            .get_adjacent(&PointDirection::Right)
            .into_iter()
            .map(|point| (PointDirection::Right, point))
            .collect(),
        (PointDirection::Right, Contraption::BackSlash) => point
            .get_adjacent(&PointDirection::Down)
            .into_iter()
            .map(|point| (PointDirection::Down, point))
            .collect(),
        (PointDirection::Right, Contraption::ForwardSlash) => point
            .get_adjacent(&PointDirection::Up)
            .into_iter()
            .map(|point| (PointDirection::Up, point))
            .collect(),
        _ => unreachable!(),
    }
}

fn get_contraption(point: &BoundedPoint, area: &Array2<Contraption>) -> Option<Contraption> {
    area.get((point.y, point.x)).copied()
}
