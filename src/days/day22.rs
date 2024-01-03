use crate::libs::{
    cli::{CliProblem, Command},
    math::absolute_difference,
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::{Args, ValueEnum};
use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    cell::LazyCell,
    cmp::{max, min},
    collections::{HashMap, HashSet, VecDeque},
};
use tap::Tap;

pub const DAY_22: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day22>::new(
            "day22",
            "Find stats about falling blocks of sand",
            "The block's current coordinates, one line per block.",
        )
        .with_part(
            "The number of bricks that can be removed without causing a failure",
            CommandLineArguments {
                stat: BlockStat::SafeBricks,
            },
        )
        .with_part(
            "The sum of number of bricks that fall when removing each indidivual block",
            CommandLineArguments {
                stat: BlockStat::CascadingFalls,
            },
        ),
    )
});

struct Input(Vec<Brick>);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Brick {
    a: Point,
    b: Point,
}

impl Brick {
    fn is_adjacent(&self, other: &Brick) -> bool {
        self.to_points()
            .cartesian_product(other.to_points().collect::<Vec<_>>())
            .any(|(a, b)| a.is_adjacent(&b))
    }

    fn to_points(&self) -> impl Iterator<Item = Point> {
        let (low_x, high_x) = self.x_range();
        let (low_y, high_y) = self.y_range();
        let (low_z, high_z) = self.z_range();

        (low_x..=high_x)
            .cartesian_product(low_y..=high_y)
            .cartesian_product(low_z..=high_z)
            .map(|((x, y), z)| Point { x, y, z })
    }

    fn intersects(&self, other: &Brick) -> bool {
        intersects_with(self.x_range(), other.x_range())
            && intersects_with(self.y_range(), other.y_range())
            && intersects_with(self.z_range(), other.z_range())
    }

    fn x_range(&self) -> (usize, usize) {
        (min(self.a.x, self.b.x), max(self.a.x, self.b.x))
    }

    fn y_range(&self) -> (usize, usize) {
        (min(self.a.y, self.b.y), max(self.a.y, self.b.y))
    }

    fn z_range(&self) -> (usize, usize) {
        (min(self.a.z, self.b.z), max(self.a.z, self.b.z))
    }
}

fn intersects_with((low_a, high_a): (usize, usize), (low_b, high_b): (usize, usize)) -> bool {
    low_b <= high_a && low_a <= high_b
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
    z: usize,
}

impl Point {
    fn is_adjacent(&self, other: &Point) -> bool {
        absolute_difference(self.x, other.x)
            + absolute_difference(self.y, other.y)
            + absolute_difference(self.z, other.z)
            == 1
    }
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let point = parse_usize()
            .separated_by(just(","))
            .exactly(3)
            .collect_exactly::<[usize; 3]>()
            .map(|[x, y, z]| Point { x, y, z })
            .boxed();
        let brick = point
            .clone()
            .then_ignore(just("~"))
            .then(point)
            .map(|(a, b)| Brick { a, b });
        parse_lines(brick).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The type of statitsic to calculate")]
    stat: BlockStat,
}

#[derive(Clone, ValueEnum)]
enum BlockStat {
    SafeBricks,
    CascadingFalls,
}

struct Day22 {}

impl Problem<Input, CommandLineArguments> for Day22 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let (final_bricks, z_to_fused_blocks, _) = fall_bricks(input.0);

        match arguments.stat {
            BlockStat::SafeBricks => final_bricks
                .into_iter()
                .filter(|brick| can_remove_brick(brick, &z_to_fused_blocks))
                .count(),
            BlockStat::CascadingFalls => final_bricks
                .par_iter()
                .filter(|brick| !can_remove_brick(brick, &z_to_fused_blocks))
                .map(|brick| {
                    final_bricks
                        .clone()
                        .tap_mut(|bricks| {
                            bricks.remove(brick);
                        })
                        .into_iter()
                        .collect::<Vec<_>>()
                })
                .map(|new_bricks| fall_bricks(new_bricks).2)
                .sum(),
        }
    }
}

fn fall_bricks(input: Vec<Brick>) -> (HashSet<Brick>, HashMap<usize, Vec<Brick>>, usize) {
    let mut z_to_fused_blocks = HashMap::<usize, Vec<Brick>>::new();
    let mut final_bricks = HashSet::<Brick>::new();
    let mut high_fused_z = 0;
    let mut queue = input
        .into_iter()
        .sorted_by(|a, b| min(a.a.z, a.b.z).cmp(&min(b.a.z, b.b.z)))
        .collect::<VecDeque<_>>();
    let mut fall_count = 0;

    while let Some(brick) = queue.pop_front() {
        let mut fell = false;
        let (mut low_z, mut high_z) = brick.z_range();
        let mut current = brick;
        let z_to_fused_blocks = &mut z_to_fused_blocks;

        while low_z > 1 {
            let (new_low_z, new_high_z) = if low_z > high_fused_z {
                let delta = max(low_z - high_fused_z - 1, 1);
                (low_z - delta, high_z - delta)
            } else {
                (low_z - 1, high_z - 1)
            };

            let new_brick = if current.a.z <= current.b.z {
                Brick {
                    a: Point {
                        z: new_low_z,
                        ..current.a
                    },
                    b: Point {
                        z: new_high_z,
                        ..current.b
                    },
                }
            } else {
                Brick {
                    a: Point {
                        z: new_high_z,
                        ..current.a
                    },
                    b: Point {
                        z: new_low_z,
                        ..current.b
                    },
                }
            };

            let z_to_fused_blocks = &z_to_fused_blocks;
            let test_brick = &new_brick;

            if (new_low_z..=new_high_z).into_iter().any(move |z| {
                z_to_fused_blocks
                    .get(&z)
                    .into_iter()
                    .flat_map(|bricks| bricks)
                    .any(|other_brick| test_brick.intersects(&other_brick))
            }) {
                break;
            }

            fell = true;
            low_z = new_low_z;
            high_z = new_high_z;
            current = new_brick;
        }

        final_bricks.insert(current.clone());
        high_fused_z = max(high_fused_z, high_z);
        if fell {
            fall_count += 1;
        }

        (low_z..=high_z).into_iter().for_each(move |z| {
            z_to_fused_blocks
                .entry(z)
                .or_insert_with(|| Vec::new())
                .push(current.clone());
        });
    }

    (final_bricks, z_to_fused_blocks, fall_count)
}

fn can_remove_brick(brick: &Brick, z_to_fused_blocks: &HashMap<usize, Vec<Brick>>) -> bool {
    let (_, high_z) = brick.z_range();
    z_to_fused_blocks
        .get(&(high_z + 1))
        .into_iter()
        .flat_map(|bricks| bricks)
        .filter(|brick| brick.z_range().0 == high_z + 1)
        .filter(|top_brick| brick.is_adjacent(&top_brick))
        .all(|top_brick| {
            let (low_z, _) = top_brick.z_range();
            z_to_fused_blocks
                .get(&(low_z - 1))
                .into_iter()
                .flat_map(|bricks| bricks)
                .filter(|brick| brick.z_range().1 == low_z - 1)
                .filter(|supporting_brick| supporting_brick != &brick)
                .filter(|supporting_brick| top_brick.is_adjacent(&supporting_brick))
                .count()
                > 0
        })
}
