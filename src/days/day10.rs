use crate::libs::{
    cli::{CliProblem, Command},
    graph::{BoundedPoint, PointDirection, CARDINAL_DIRECTIONS},
    parse::{parse_table2, StringParse},
    problem::Problem,
};
use chumsky::{
    container::Seq,
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};
use clap::{Args, ValueEnum};
use ndarray::Array2;
use std::{cell::LazyCell, collections::VecDeque};

pub const DAY_10: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day10>::new(
            "day10",
            "Find stats about a loop of pipes",
            "Table with exactly one start which connacts to a loop of pipes",
        )
        .with_part(
            "Finds the furthest lenght of the start in the loop",
            CommandLineArguments {
                loop_stat: LoopStat::LengthFromStart,
            },
        )
        .with_part(
            "Counts the number of segments enclosed by the loop",
            CommandLineArguments {
                loop_stat: LoopStat::EnclosedCount,
            },
        ),
    )
});

struct Input(Array2<Field>);

#[derive(Clone, PartialEq, Eq, Hash, Copy, Debug)]
enum Field {
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
}

impl Field {
    fn valid_pipe_directions(&self) -> Vec<PointDirection> {
        match self {
            Field::Vertical => vec![PointDirection::Up, PointDirection::Down],
            Field::Horizontal => vec![PointDirection::Left, PointDirection::Right],
            Field::NorthEast => vec![PointDirection::Up, PointDirection::Right],
            Field::NorthWest => vec![PointDirection::Up, PointDirection::Left],
            Field::SouthWest => vec![PointDirection::Down, PointDirection::Left],
            Field::SouthEast => vec![PointDirection::Down, PointDirection::Right],
            Field::Ground => vec![],
            Field::Start => vec![],
        }
    }
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let field = choice((
            just("|").to(Field::Vertical),
            just("-").to(Field::Horizontal),
            just("L").to(Field::NorthEast),
            just("J").to(Field::NorthWest),
            just("7").to(Field::SouthWest),
            just("F").to(Field::SouthEast),
            just(".").to(Field::Ground),
            just("S").to(Field::Start),
        ));
        parse_table2(field).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The type of statistic about the loop")]
    loop_stat: LoopStat,
}

#[derive(Clone, ValueEnum)]
enum LoopStat {
    LengthFromStart,
    EnclosedCount,
}

struct Day10 {}

impl Problem<Input, CommandLineArguments> for Day10 {
    type Output = usize;

    fn run(mut input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let max_x = input.0.dim().1 - 1;
        let max_y = input.0.dim().0 - 1;
        let start = find_start(&input.0, max_x, max_y);
        let new_start_field = find_start_field(&start, &input.0);
        *input.0.get_mut((start.y, start.x)).expect("exists") = new_start_field;
        let mut visited = Array2::from_elem(input.0.dim(), false);
        *visited.get_mut((start.y, start.x)).expect("exists") = true;

        let mut queue = VecDeque::from_iter(
            get_valid_pipes(&start, &input.0)
                .into_iter()
                .map(|point| (point, 1usize)),
        );

        let mut max = 0;

        while let Some((current_point, distance)) = queue.pop_front() {
            let field = get_field_from_area(&current_point, &input.0).expect("Point exists");
            if distance > max {
                max = distance;
            }
            *visited
                .get_mut((current_point.y, current_point.x))
                .expect("exists") = true;
            field
                .valid_pipe_directions()
                .into_iter()
                .filter_map(|direction| {
                    current_point
                        .get_adjacent(&direction)
                        .filter(|point| !visited.get((point.y, point.x)).expect("exists"))
                })
                .map(|point| (point, distance + 1))
                .for_each(|item| queue.push_back(item))
        }

        match arguments.loop_stat {
            LoopStat::LengthFromStart => max,
            LoopStat::EnclosedCount => {
                let mut in_loop = false;
                let mut count = 0;

                input.0.indexed_iter().for_each(|(point, field)| {
                    if *visited.get(point).expect("exists") {
                        match field {
                            Field::Vertical | Field::NorthEast | Field::NorthWest => {
                                in_loop = !in_loop;
                            }
                            _ => (),
                        };
                    } else if in_loop {
                        count += 1;
                    }
                });

                count
            }
        }
    }
}

fn find_start_field(point: &BoundedPoint, area: &Array2<Field>) -> Field {
    let valid_up = &point
        .get_adjacent(&PointDirection::Up)
        .and_then(|next| get_field_from_area(&next, area))
        .filter(|field| matches!(field, Field::Vertical | Field::SouthEast | Field::SouthWest))
        .is_some();

    let valid_down = &point
        .get_adjacent(&PointDirection::Down)
        .and_then(|next| get_field_from_area(&next, area))
        .filter(|field| matches!(field, Field::Vertical | Field::NorthEast | Field::NorthWest))
        .is_some();

    let valid_left = &point
        .get_adjacent(&PointDirection::Left)
        .and_then(|next| get_field_from_area(&next, area))
        .filter(|field| {
            matches!(
                field,
                Field::Horizontal | Field::NorthEast | Field::SouthEast
            )
        })
        .is_some();

    let valid_right = &point
        .get_adjacent(&PointDirection::Right)
        .and_then(|next| get_field_from_area(&next, area))
        .filter(|field| {
            matches!(
                field,
                Field::Horizontal | Field::NorthWest | Field::SouthWest
            )
        })
        .is_some();
    match (valid_up, valid_down, valid_left, valid_right) {
        (true, true, false, false) => Field::Vertical,
        (true, false, true, false) => Field::NorthEast,
        (true, false, false, true) => Field::NorthWest,
        (false, true, true, false) => Field::SouthWest,
        (false, true, false, true) => Field::SouthEast,
        (false, false, true, true) => Field::Horizontal,
        _ => unreachable!(),
    }
}

fn find_start(area: &Array2<Field>, max_x: usize, max_y: usize) -> BoundedPoint {
    area.indexed_iter()
        .find_map(|((y, x), field)| match field {
            Field::Start => Some(BoundedPoint { x, y, max_x, max_y }),
            _ => None,
        })
        .expect("Start exists")
}

fn get_field_from_area<'a>(point: &BoundedPoint, area: &'a Array2<Field>) -> Option<&'a Field> {
    area.get((point.y, point.x))
}

fn get_valid_pipes(point: &BoundedPoint, area: &Array2<Field>) -> Vec<BoundedPoint> {
    let field = get_field_from_area(point, area).expect("Point exists");
    match field {
        Field::Start => CARDINAL_DIRECTIONS
            .iter()
            .filter_map(|direction| {
                point.get_adjacent(direction).filter(|other| {
                    get_field_from_area(other, area)
                        .filter(|field| {
                            field
                                .valid_pipe_directions()
                                .contains(&direction.get_opposite())
                        })
                        .is_some()
                })
            })
            .collect(),
        _ => field
            .valid_pipe_directions()
            .into_iter()
            .filter_map(|direction| point.get_adjacent(&direction))
            .collect(),
    }
}
