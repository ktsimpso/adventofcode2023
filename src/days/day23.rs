use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection},
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
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    cell::{LazyCell, Ref, RefCell, RefMut},
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
};

pub const DAY_23: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day23>::new(
            "day23",
            "Finds the longest path to walk on the mountain",
            "The mountain terrain map",
        )
        .with_part(
            "Slopes are too slippery to climb",
            CommandLineArguments {
                slippery: true,
                parallel_depth: 0,
            },
        )
        .with_part(
            "Slopes are climbable",
            CommandLineArguments {
                slippery: false,
                parallel_depth: 15,
            },
        ),
    )
});

struct Input(Vec<Vec<Field>>);

#[derive(Debug, Clone)]
enum Field {
    Path,
    Forest,
    NorthSlope,
    EastSlope,
    SouthSlope,
    WestSlope,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let field = choice((
            just(".").to(Field::Path),
            just("#").to(Field::Forest),
            just("^").to(Field::NorthSlope),
            just(">").to(Field::EastSlope),
            just("v").to(Field::SouthSlope),
            just("<").to(Field::WestSlope),
        ));
        parse_table(field).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "Whether the slopes are slippery or not")]
    slippery: bool,

    #[arg(short, long, help = "The max depth to parallelize the path finding")]
    parallel_depth: usize,
}

struct Day23 {}

impl Problem<Input, CommandLineArguments> for Day23 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_y = input.0.len() - 1;
        let max_x = input.0.first().map(|row| row.len()).unwrap_or(0) - 1;
        let start = input
            .0
            .get(0)
            .expect("Row exists")
            .into_iter()
            .enumerate()
            .find_map(|(x, tile)| match tile {
                Field::Path => Some(BoundedPoint {
                    x,
                    y: 0,
                    max_x,
                    max_y,
                }),
                _ => None,
            })
            .expect("Start exists");
        let end = input
            .0
            .get(max_y)
            .expect("Row exists")
            .into_iter()
            .enumerate()
            .find_map(|(x, tile)| match tile {
                Field::Path => Some(BoundedPoint {
                    x,
                    y: max_y,
                    max_x,
                    max_y,
                }),
                _ => None,
            })
            .expect("End Exists");

        let sparse = sparse_graph(&start, &end, &input.0, &arguments.slippery);
        let (point_to_u64, encoded) = encode_graph(&sparse);

        longest_path(
            *point_to_u64.get(&start).expect("exists"),
            *point_to_u64.get(&end).expect("exists"),
            0,
            &encoded,
            0,
            &arguments.parallel_depth,
        )
        .expect("Solution exists")
    }
}

fn sparse_graph(
    start: &BoundedPoint,
    end: &BoundedPoint,
    terrain: &Vec<Vec<Field>>,
    slippery: &bool,
) -> HashMap<BoundedPoint, BTreeSet<(BoundedPoint, usize)>> {
    let mut results = HashMap::new();
    let mut queue = VecDeque::new();
    let mut queue_visited = HashSet::new();
    queue.push_back(*start);

    while let Some(target) = queue.pop_front() {
        if queue_visited.contains(&target) {
            continue;
        }
        queue_visited.insert(target);
        let mut visited = HashSet::new();
        visited.insert(target);
        get_adjacent_tiles(&target, terrain, slippery)
            .into_iter()
            .filter(|next_point| !visited.contains(next_point))
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|path| {
                let mut current = path;
                let mut count = 0usize;

                loop {
                    visited.insert(current);
                    count += 1;
                    let next_points = get_adjacent_tiles(&current, terrain, slippery)
                        .into_iter()
                        .filter(|next_point| !visited.contains(next_point))
                        .collect::<Vec<_>>();
                    match next_points.len() {
                        1 => {
                            current = *next_points.get(0).expect("Exists");
                        }
                        0 => {
                            if current == *end {
                                results
                                    .entry(target)
                                    .or_insert_with(|| BTreeSet::new())
                                    .insert((current.clone(), count));
                            }
                            break;
                        }
                        _ => {
                            results
                                .entry(target)
                                .or_insert_with(|| BTreeSet::new())
                                .insert((current.clone(), count));
                            queue.push_back(current);
                            break;
                        }
                    }
                }
            });
    }

    let mut current = end.clone();
    let results = RefCell::new(results);
    let get_count =
        |results: Ref<HashMap<BoundedPoint, BTreeSet<_>>>, current: &BoundedPoint| -> usize {
            results
                .values()
                .filter(|points| points.iter().any(|(point, _)| point == current))
                .count()
        };
    let filter_current = |mut results: RefMut<HashMap<BoundedPoint, BTreeSet<_>>>,
                          current: &BoundedPoint|
     -> BoundedPoint {
        results
            .iter_mut()
            .find(|(_, points)| {
                points
                    .iter()
                    .any(|(point, _): &(BoundedPoint, usize)| point == current)
            })
            .map(|(key, points)| {
                points.retain(|(point, _)| point == current);
                key
            })
            .expect("Exists")
            .clone()
    };
    while get_count(results.borrow(), &current) == 1 {
        current = filter_current(results.borrow_mut(), &current);
    }

    results.into_inner()
}

fn encode_graph(
    sparse: &HashMap<BoundedPoint, BTreeSet<(BoundedPoint, usize)>>,
) -> (
    BTreeMap<BoundedPoint, u64>,
    BTreeMap<u64, BTreeSet<(u64, usize)>>,
) {
    let point_to_u64 = sparse
        .iter()
        .fold(
            (BTreeMap::new(), 0usize),
            |(mut map, mut largest_index): (BTreeMap<BoundedPoint, u64>, usize), (key, values)| {
                if !map.contains_key(key) {
                    let key_value = 1u64 << largest_index;
                    map.insert(key.clone(), key_value);
                    largest_index += 1;
                }

                values.into_iter().for_each(|(key, _)| {
                    if !map.contains_key(key) {
                        let key_value = 1u64 << largest_index;
                        map.insert(key.clone(), key_value);
                        largest_index += 1;
                    }
                });

                (map, largest_index)
            },
        )
        .0;

    let result = sparse
        .into_iter()
        .map(|(key, values)| {
            let new_key = *point_to_u64.get(key).expect("exists");
            let new_values = values
                .into_iter()
                .map(|(key, length)| (*point_to_u64.get(key).expect("Exists"), *length))
                .collect();

            (new_key, new_values)
        })
        .collect();

    (point_to_u64, result)
}

fn longest_path(
    current: u64,
    target: u64,
    visited: u64,
    encoded: &BTreeMap<u64, BTreeSet<(u64, usize)>>,
    depth: usize,
    max_parallel_depth: &usize,
) -> Option<usize> {
    if current == target {
        return Some(0);
    }

    let new_visited = visited | current;

    if &depth < max_parallel_depth {
        encoded
            .get(&current)
            .expect("Exists")
            .into_par_iter()
            .filter(|(new_point, _)| new_visited & new_point == 0)
            .filter_map(|(new_point, count)| {
                longest_path(
                    *new_point,
                    target,
                    new_visited,
                    encoded,
                    depth + 1,
                    max_parallel_depth,
                )
                .map(|value| value + count)
            })
            .max()
    } else {
        encoded
            .get(&current)
            .expect("Exists")
            .into_iter()
            .filter(|(new_point, _)| new_visited & new_point == 0)
            .filter_map(|(new_point, count)| {
                longest_path(
                    *new_point,
                    target,
                    new_visited,
                    encoded,
                    depth + 1,
                    max_parallel_depth,
                )
                .map(|value| value + count)
            })
            .max()
    }
}

fn get_adjacent_tiles(
    point: &BoundedPoint,
    terrain: &Vec<Vec<Field>>,
    slippery: &bool,
) -> Vec<BoundedPoint> {
    if *slippery {
        let current_tile = get_field_from_point(point, terrain).expect("Exists");
        match current_tile {
            Field::Path => point.into_iter_cardinal_adjacent().collect::<Vec<_>>(),
            Field::NorthSlope => point
                .get_adjacent(&PointDirection::Up)
                .into_iter()
                .collect(),
            Field::EastSlope => point
                .get_adjacent(&PointDirection::Right)
                .into_iter()
                .collect(),
            Field::SouthSlope => point
                .get_adjacent(&PointDirection::Down)
                .into_iter()
                .collect(),
            Field::WestSlope => point
                .get_adjacent(&PointDirection::Left)
                .into_iter()
                .collect(),
            Field::Forest => unreachable!(),
        }
    } else {
        point.into_iter_cardinal_adjacent().collect::<Vec<_>>()
    }
    .into_iter()
    .filter(
        |new_point| match get_field_from_point(&new_point, terrain).expect("Exists") {
            Field::Forest => false,
            _ => true,
        },
    )
    .collect()
}

fn get_field_from_point(point: &BoundedPoint, terrain: &Vec<Vec<Field>>) -> Option<Field> {
    terrain
        .get(point.y)
        .and_then(|row| row.get(point.x).cloned())
}
