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
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    cell::LazyCell,
    collections::{HashMap, VecDeque},
};
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
            .par_iter()
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
    count_valid_states(
        condition_record.springs.into_iter().collect(),
        &condition_record.key.into_iter().collect(),
        0,
        &mut HashMap::new(),
    )
}

fn count_valid_states(
    remaining: VecDeque<SpringCondition>,
    remaining_key: &VecDeque<usize>,
    current_key_count: usize,
    cache: &mut HashMap<(VecDeque<SpringCondition>, VecDeque<usize>, usize), usize>,
) -> usize {
    match cache.get(&(remaining.clone(), remaining_key.clone(), current_key_count)) {
        Some(result) => *result,
        None => {
            let mut new_remaining = remaining.clone();

            let result = if let Some(next) = new_remaining.pop_front() {
                match next {
                    SpringCondition::Operational => {
                        if current_key_count == 0 {
                            count_valid_states(
                                new_remaining,
                                remaining_key,
                                current_key_count,
                                cache,
                            )
                        } else if let Some(key) = remaining_key.front() {
                            let mut new_remaining_key = remaining_key.clone();
                            new_remaining_key.pop_front();
                            if current_key_count == *key {
                                count_valid_states(new_remaining, &new_remaining_key, 0, cache)
                            } else {
                                0
                            }
                        } else {
                            0
                        }
                    }
                    SpringCondition::Broken => {
                        let new_current_key_count = current_key_count + 1;
                        if let Some(key) = remaining_key.front() {
                            if new_current_key_count > *key {
                                0
                            } else {
                                count_valid_states(
                                    new_remaining,
                                    remaining_key,
                                    new_current_key_count,
                                    cache,
                                )
                            }
                        } else {
                            0
                        }
                    }
                    SpringCondition::Unknown => {
                        let mut operational = new_remaining.clone();
                        operational.push_front(SpringCondition::Operational);
                        new_remaining.push_front(SpringCondition::Broken);
                        count_valid_states(operational, remaining_key, current_key_count, cache)
                            + count_valid_states(
                                new_remaining,
                                remaining_key,
                                current_key_count,
                                cache,
                            )
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
            cache.insert(
                (remaining, remaining_key.clone(), current_key_count),
                result,
            );
            return result;
        }
    }
}
