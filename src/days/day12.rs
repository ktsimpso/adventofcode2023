use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_isize, parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, end, just},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use factorial::Factorial;
use integer_sqrt::IntegerSquareRoot;
use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    cell::LazyCell,
    collections::{BTreeSet, HashMap, VecDeque},
};
use tap::Tap;

pub const DAY_12: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day12>::new("day12", "help", "file help")
            .with_part("part1", CommandLineArguments {})
            .with_part("part2", CommandLineArguments {}),
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
struct CommandLineArguments {}

struct Day12 {}

impl Problem<Input, CommandLineArguments> for Day12 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let repeat = 5;
        input
            .0
            .iter()
            .map(|condition_record| ConditionRecord {
                springs: condition_record
                    .springs
                    .clone()
                    .tap_mut(|springs| springs.push(SpringCondition::Unknown))
                    .repeat(repeat)
                    .tap_mut(|springs| {
                        springs.pop();
                    }),
                key: condition_record.key.repeat(repeat),
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
    //build_all_congruant_springs_multi(&condition_record.key, &condition_record.springs)
    /*dbg!("NEW LAP RECORD");
    dbg!(condition_record);
    let groups = condition_record
        .springs
        .iter()
        .group_by(|spring| match spring {
            SpringCondition::Operational => true,
            _ => false,
        })
        .into_iter()
        //.filter(|(operational, _)| !operational)
        .map(|(_, springs)| springs.cloned().collect::<Vec<_>>())
        .collect::<VecDeque<_>>();

    /*condition_record.key.iter().map(|key| {
        (
            *key,
            groups
                .iter()
                .map(|springs| times_key_can_be_in_group(*key, springs)),
        )
    }).collect();*/

    /*condition_record.key.into_iter().map(|key| {
        (
            key,
            groups
                .iter()
                .enumerate()
                .map(|(index, springs)| (index, can_fit_in_group(key, springs)))
                .collect::<Vec<_>>(),
        )
    });*/

    dbg!(&groups);

    let mut count: Vec<Vec<SpringCondition>> = Vec::new();

    for (index, key) in condition_record.key.iter().enumerate() {
        dbg!("====================");
        //dbg!((index, key));
        let mut current_groups = groups.clone();
        let mut final_config = Vec::new();
        for left in 0..index {
            dbg!("Left!: ", left);
            let left_key = condition_record.key[left];
            final_config.append(
                &mut current_groups
                    .iter()
                    .take_while(|springs| !can_fit_in_group(left_key, springs))
                    .map(|springs| vec![SpringCondition::Operational; springs.len()])
                    .collect::<Vec<_>>(),
            );
            current_groups = current_groups
                .into_iter()
                .skip_while(|springs| !can_fit_in_group(left_key, springs))
                .collect();

            let (mut left_part, residual) =
                find_residual(left_key, &current_groups.pop_front().expect("exists"));
            let has_residual = dbg!(&residual).is_some();
            residual.into_iter().for_each(|res| {
                current_groups.push_front(res);
            });

            let left_part_len = dbg!(&left_part).len();

            if left_part_len > 0 {
                if has_residual {
                    left_part[left_part_len - 1] = SpringCondition::Operational;
                    left_part[(left_part_len - 1 - left_key)..(left_part_len - 1)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Broken);
                    left_part[0..(left_part_len - 1 - left_key)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Operational);
                } else {
                    left_part[(left_part_len - left_key)..left_part_len]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Broken);
                    left_part[0..(left_part_len - left_key)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Operational);
                }
            }
            dbg!(&left_part);

            final_config.push(left_part);
        }
        dbg!("Left end");

        //dbg!(&current_groups);

        current_groups
            .iter_mut()
            .for_each(|springs| springs.reverse());
        current_groups = current_groups.into_iter().rev().collect();

        //dbg!(&current_groups);
        let mut final_config_right = Vec::new();
        for right in ((index + 1)..condition_record.key.len()).rev() {
            dbg!("Right!", right);
            let right_key = condition_record.key[right];
            final_config_right.append(
                &mut current_groups
                    .iter()
                    .take_while(|springs| !can_fit_in_group(right_key, springs))
                    .map(|springs| vec![SpringCondition::Operational; springs.len()])
                    .collect::<Vec<_>>(),
            );
            current_groups = current_groups
                .into_iter()
                .skip_while(|springs| !can_fit_in_group(right_key, springs))
                .collect();

            let (mut right_part, residual) =
                find_residual(right_key, &current_groups.pop_front().expect("exists"));
            let has_residual = residual.is_some();
            residual.into_iter().for_each(|res| {
                current_groups.push_front(dbg!(res));
            });

            let right_part_len = dbg!(&right_part).len();
            dbg!(right_part_len, right_key);

            if right_part_len > 0 {
                if has_residual {
                    right_part[right_part_len - 1] = SpringCondition::Operational;
                    right_part[(right_part_len - 1 - right_key)..(right_part_len - 1)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Broken);
                    right_part[0..(right_part_len - 1 - right_key)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Operational);
                } else {
                    right_part[(right_part_len - right_key)..right_part_len]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Broken);
                    right_part[0..(right_part_len - right_key)]
                        .iter_mut()
                        .for_each(|part| *part = SpringCondition::Operational);
                }
            }

            dbg!(&right_part);

            final_config_right.push(right_part);
        }

        final_config_right
            .iter_mut()
            .for_each(|springs| springs.reverse());
        final_config_right = final_config_right.into_iter().rev().collect();

        current_groups
            .iter_mut()
            .for_each(|springs| springs.reverse());
        current_groups = current_groups.into_iter().rev().collect();

        let middle_springs = current_groups
            .iter()
            .map(|springs| {
                (
                    vec![SpringCondition::Operational; springs.len()],
                    build_all_congruant_springs(*key, &springs),
                )
            })
            .collect::<Vec<_>>();

        for index in 0..middle_springs.len() {
            let left = final_config.clone().tap_mut(|fc| {
                fc.append(
                    &mut middle_springs[0..index]
                        .iter()
                        .map(|(operational, _)| operational.clone())
                        .collect::<Vec<_>>(),
                )
            });
            let right = middle_springs[index + 1..middle_springs.len()]
                .iter()
                .map(|(operational, _)| operational.clone())
                .collect::<Vec<_>>()
                .tap_mut(|r| r.extend(final_config_right.clone()));

            count.extend(dbg!(middle_springs[index]
                .1
                .iter()
                .map(|middle| {
                    left.clone()
                        .tap_mut(|l| l.extend(vec![middle.clone()]))
                        .tap_mut(|l| l.extend(right.clone()))
                })
                .map(|result| result.into_iter().flat_map(|r| r).collect())
                .filter(|result| are_congruant_springs(result, &condition_record.springs))
                .collect::<Vec<_>>()));
        }

        /*if current_groups.iter().any(|springs| {
            springs
                .into_iter()
                .any(|spring| spring == &SpringCondition::Broken)
        }) {
            count += current_groups
                .iter()
                .find(|springs| {
                    springs
                        .into_iter()
                        .any(|spring| spring == &SpringCondition::Broken)
                })
                .into_iter()
                .map(|springs| times_key_can_be_in_group(*key, springs))
                .sum::<usize>()
        } else {
            count += current_groups
                .iter()
                .map(|springs| times_key_can_be_in_group(*key, springs))
                .sum::<usize>();
        }*/
    }

    count
        .into_iter()
        .fold(BTreeSet::new(), |acc, result| {
            acc.tap_mut(|a| {
                a.insert(result);
            })
        })
        .len()*/
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
                //dbg!("Next");
                match next {
                    SpringCondition::Operational => {
                        //dbg!("op");
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
                        //dbg!("broke");
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
                        //dbg!("shrug");
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
                //dbg!("too many keys at end");
                0
            } else if remaining_key.len() == 1 {
                if remaining_key[0] == current_key_count {
                    //dbg!("last key correct ");
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
    //dbg!("==========================");
    //dbg!(&remaining, remaining_key, current_key_count);
}

fn build_all_congruant_springs(
    length: usize,
    key: &Vec<SpringCondition>,
) -> Vec<Vec<SpringCondition>> {
    if key.len() < length {
        return Vec::new();
    }
    let mut test = vec![SpringCondition::Broken; length]
        .tap_mut(|t| t.extend(vec![SpringCondition::Operational; key.len() - length]))
        .into_iter()
        .collect::<VecDeque<_>>();
    let mut result = Vec::new();

    if are_congruant_springs(&test.iter().cloned().collect(), &key) {
        result.push(test.iter().cloned().collect());
    }

    for _ in 0..(key.len() - length) {
        test.rotate_right(1);
        if are_congruant_springs(&test.iter().cloned().collect(), &key) {
            result.push(test.iter().cloned().collect());
        }
    }

    result
}

fn build_all_congruant_springs_multi(lengths: &Vec<usize>, key: &Vec<SpringCondition>) -> usize {
    let number_of_spaces = key.len() - lengths.iter().sum::<usize>();
    let min_spacing = number_of_spaces - (lengths.len() - 1);

    (0..=(lengths.len()))
        .map(|index| {
            if index == 0 || index == lengths.len() {
                0..=min_spacing
            } else {
                1..=(min_spacing + 1)
            }
        })
        .multi_cartesian_product()
        .filter(|result: &Vec<usize>| result.into_iter().sum::<usize>() == number_of_spaces)
        .map(|spaces| spaces.into_iter().interleave(lengths.iter().copied()))
        .map(|specification| {
            specification
                .into_iter()
                .enumerate()
                .flat_map(|(index, item_length)| {
                    if index % 2 == 0 {
                        vec![SpringCondition::Operational; item_length]
                    } else {
                        vec![SpringCondition::Broken; item_length]
                    }
                })
                .collect::<Vec<_>>()
        })
        .filter(|filled| are_congruant_springs(filled, key))
        .count()
}

fn are_congruant_springs(filled: &Vec<SpringCondition>, key: &Vec<SpringCondition>) -> bool {
    if filled.len() != key.len() {
        false
    } else {
        filled
            .into_iter()
            .zip(key.into_iter())
            .all(|(test, k)| match (test, k) {
                (SpringCondition::Broken, SpringCondition::Broken) => true,
                (_, SpringCondition::Broken) => false,
                (SpringCondition::Operational, SpringCondition::Operational) => true,
                (_, SpringCondition::Operational) => false,
                (_, SpringCondition::Unknown) => true,
            })
    }
}

fn find_residual(
    key: usize,
    springs: &Vec<SpringCondition>,
) -> (Vec<SpringCondition>, Option<Vec<SpringCondition>>) {
    if !can_fit_in_group(key, springs) {
        (springs.clone(), None)
    } else if springs.len() < key {
        (springs.clone(), None)
    } else if springs.len() == key {
        (springs.clone(), None)
    } else if springs.len() == key + 1 {
        if springs.first().expect("exists") == &SpringCondition::Unknown {
            (springs.clone(), None)
        } else {
            (springs.clone(), None)
        }
    } else {
        if springs.get(key).expect("exists") == &SpringCondition::Unknown {
            (
                springs[..=key].into_iter().cloned().collect(),
                Some(
                    springs[(key + 1)..]
                        .into_iter()
                        .cloned()
                        .collect::<Vec<_>>(),
                )
                .and_then(|s| if s.is_empty() { None } else { Some(s) }),
            )
        } else if springs.windows(key + 2).any(|new_springs| {
            new_springs.first().expect("exists") == &SpringCondition::Unknown
                && new_springs.last().expect("exists") == &SpringCondition::Unknown
        }) {
            (
                springs
                    .windows(key + 2)
                    .take_while(|new_springs| {
                        !(new_springs.first().expect("exists") == &SpringCondition::Unknown
                            && new_springs.last().expect("exists") == &SpringCondition::Unknown)
                    })
                    .fold(Vec::new(), |mut acc, new_springs| {
                        acc.tap_mut(|acc| acc.push(new_springs.last().expect("exists").clone()))
                    }),
                Some(
                    springs
                        .windows(key + 2)
                        .skip_while(|new_springs| {
                            !(new_springs.first().expect("exists") == &SpringCondition::Unknown
                                && new_springs.last().expect("exists") == &SpringCondition::Unknown)
                        })
                        .fold(Vec::new(), |mut acc, new_springs| {
                            acc.tap_mut(|acc| acc.push(new_springs.last().expect("exists").clone()))
                        }),
                )
                .and_then(|s| if s.is_empty() { None } else { Some(s) }),
            )
        } else if springs.get(springs.len() - key - 1).expect("exists") == &SpringCondition::Unknown
        {
            (springs.clone(), None)
        } else {
            (springs.clone(), None)
        }
    }
}

fn can_fit_in_group(key: usize, springs: &Vec<SpringCondition>) -> bool {
    if springs
        .iter()
        .any(|spring| spring == &SpringCondition::Operational)
    {
        return false;
    }

    if springs.len() < key {
        false
    } else if springs.len() == key {
        true
    } else if springs.len() == key + 1 {
        springs.first().expect("exists") == &SpringCondition::Unknown
            || springs.last().expect("exists") == &SpringCondition::Unknown
    } else {
        springs.get(key).expect("exists") == &SpringCondition::Unknown
            || springs.get(springs.len() - key - 1).expect("exists") == &SpringCondition::Unknown
            || springs.windows(key + 2).any(|new_springs| {
                new_springs.first().expect("exists") == &SpringCondition::Unknown
                    && new_springs.last().expect("exists") == &SpringCondition::Unknown
            })
    }
}

fn times_key_can_be_in_group(key: usize, springs: &Vec<SpringCondition>) -> usize {
    if key > springs.len() {
        return 0;
    }
    let groups = springs
        .iter()
        .group_by(|spring| match spring {
            SpringCondition::Broken => true,
            SpringCondition::Unknown => false,
            _ => unreachable!(),
        })
        .into_iter()
        .map(|(is_broken, group)| (is_broken, group.into_iter().count()))
        .collect::<Vec<_>>();

    let number_of_groups = groups.len();
    if number_of_groups == 1 {
        let (is_broken, group) = groups.get(0).expect("One group");
        if *is_broken {
            if group == &key {
                return 1;
            } else {
                return 0;
            }
        } else {
            if group >= &key {
                group - key + 1
            } else {
                return 0;
            }
        }
    } else {
        let mut concated_groups = Vec::new();
        let mut count = 0;

        for (current, (is_broken, group)) in groups.into_iter().enumerate() {
            let current_max = concated_groups
                .iter()
                .map(|(_, _, count)| count)
                .sum::<usize>()
                + group;
            let mut deductions = 0;

            if !is_broken {
                if current < number_of_groups - 1 {
                    deductions += 1;
                }
            }

            let (start_index, start_broken, start_count) = concated_groups
                .first()
                .map(|(first_index, is_broken, group)| (*first_index, *is_broken, *group))
                .unwrap_or((current, is_broken, group));

            if !start_broken && start_index != 0 {
                deductions += 1;
            }

            if current_max - deductions < key {
                concated_groups.push((current, is_broken, group));
                continue;
            }

            if concated_groups.is_empty() {
                if is_broken {
                    if group == key {
                        count += 1;
                    } else {
                        break;
                    }
                } else {
                    let additions = group - key + 1;

                    if additions > deductions {
                        count += additions - deductions;
                    }
                }
                continue;
            }

            if is_broken {
                if start_broken {
                    if current_max == key {
                        count += 1;
                    } else {
                        break;
                    }
                } else {
                    if current_max - start_count >= key {
                        count += 1;
                    } else {
                        break;
                    }
                }
            } else {
                if start_broken {
                    count += 1;
                } else {
                    count += current_max - deductions - key + 1;
                }
            }

            concated_groups = Vec::new();
        }

        count
    }
}
