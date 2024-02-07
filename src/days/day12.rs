use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    IterParser, Parser,
};
use clap::Args;
use ndarray::Array3;
use std::cell::LazyCell;
use tap::Tap;

pub const DAY_12: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day12>::new(
            "day12",
            "Finds the number of valid arrangements for each spring",
            "A newline separated list of spring arrangements",
        )
        .with_part("Repeats the springs 1 time", CommandLineArguments { n: 1 })
        .with_part("Repeats the springs 5 times", CommandLineArguments { n: 5 }),
    )
});

struct Input(Vec<ConditionRecord>);

#[derive(Debug)]
struct ConditionRecord {
    springs: Vec<SpringCondition>,
    key: Vec<usize>,
}

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord, Copy, Hash)]
enum SpringCondition {
    Operational,
    Broken,
    Unknown,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let springs = choice((
            just(".").to(SpringCondition::Operational),
            just("#").to(SpringCondition::Broken),
            just("?").to(SpringCondition::Unknown),
        ));
        let key = parse_usize()
            .separated_by(just(","))
            .at_least(1)
            .collect::<Vec<_>>();
        let condition_record = springs
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then_ignore(just(" "))
            .then(key)
            .map(|(springs, key)| ConditionRecord { springs, key });
        parse_lines(condition_record).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "Number of times to repeat the spring conditions.")]
    n: usize,
}

struct Day12 {}

impl Problem<Input, CommandLineArguments> for Day12 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .iter()
            .map(|condition_record| ConditionRecord {
                springs: condition_record
                    .springs
                    .clone()
                    .tap_mut(|springs| springs.push(SpringCondition::Unknown))
                    .repeat(arguments.n)
                    .tap_mut(|springs| {
                        springs.pop();
                    }),
                key: condition_record.key.repeat(arguments.n),
            })
            .map(find_groupings)
            .sum()
    }
}

fn find_groupings(condition_record: ConditionRecord) -> usize {
    let max_remaining = condition_record.key.iter().max().expect("exists");
    let mut cache = Array3::from_elem(
        (
            condition_record.springs.len() + 1,
            condition_record.key.len() + 1,
            *max_remaining + 1,
        ),
        None,
    );
    let result = count_valid_states(
        condition_record.springs.as_slice(),
        condition_record.key.as_slice(),
        0,
        &mut cache,
    );
    result
}

fn count_valid_states(
    remaining: &[SpringCondition],
    remaining_key: &[usize],
    current_key_count: usize,
    cache: &mut Array3<Option<usize>>,
) -> usize {
    match cache
        .get((remaining.len(), remaining_key.len(), current_key_count))
        .expect("exists")
    {
        Some(result) => *result,
        None => {
            let result = if let Some(next) = remaining.first() {
                match next {
                    SpringCondition::Operational => calculate_operational_spring(
                        remaining,
                        remaining_key,
                        current_key_count,
                        cache,
                    ),
                    SpringCondition::Broken => {
                        calculate_broken_spring(remaining, remaining_key, current_key_count, cache)
                    }
                    SpringCondition::Unknown => {
                        let operational = calculate_operational_spring(
                            remaining,
                            remaining_key,
                            current_key_count,
                            cache,
                        );
                        let broken = calculate_broken_spring(
                            remaining,
                            remaining_key,
                            current_key_count,
                            cache,
                        );

                        operational + broken
                    }
                }
            } else if remaining_key.len() > 1 {
                0
            } else if remaining_key.len() == 1 {
                if remaining_key[0] == current_key_count {
                    1
                } else {
                    0
                }
            } else {
                1
            };
            *cache
                .get_mut((remaining.len(), remaining_key.len(), current_key_count))
                .expect("exists") = Some(result);
            result
        }
    }
}

fn calculate_operational_spring(
    remaining: &[SpringCondition],
    remaining_key: &[usize],
    current_key_count: usize,
    cache: &mut Array3<Option<usize>>,
) -> usize {
    if current_key_count == 0 {
        count_valid_states(&remaining[1..], remaining_key, current_key_count, cache)
    } else if let Some(key) = remaining_key.first() {
        if current_key_count == *key {
            count_valid_states(&remaining[1..], &remaining_key[1..], 0, cache)
        } else {
            0
        }
    } else {
        0
    }
}

fn calculate_broken_spring(
    remaining: &[SpringCondition],
    remaining_key: &[usize],
    current_key_count: usize,
    cache: &mut Array3<Option<usize>>,
) -> usize {
    let new_current_key_count = current_key_count + 1;
    if let Some(key) = remaining_key.first() {
        if new_current_key_count > *key {
            0
        } else {
            count_valid_states(&remaining[1..], remaining_key, new_current_key_count, cache)
        }
    } else {
        0
    }
}
