use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_lines, parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just, one_of},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use std::{
    cell::LazyCell,
    cmp::{max, min},
    collections::{HashMap, VecDeque},
};

pub const DAY_19: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day19>::new("day19", "Finds valid parts given the list of workflows.", "A newline delimited list of workflows followed by a blank line then a newlin delimited list of parts.")
            .with_part("Finds all valid parts and sums thier xmas rating", CommandLineArguments { range: false })
            .with_part("Finds all valid ranges of parts and gets the total", CommandLineArguments { range: true }),
    )
});

#[derive(Debug)]
struct Input {
    workflows: HashMap<String, Workflow>,
    parts: Vec<Part>,
}

#[derive(Debug)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

#[derive(Debug)]
struct Workflow {
    key: String,
    conditional_workflows: Vec<ConditionalWorkflow>,
    default: WorkflowIdentifier,
}

#[derive(Debug)]
struct ConditionalWorkflow {
    part_identifier: PartIdentifier,
    condition: Condition,
    condition_value: usize,
    workflow_identifier: WorkflowIdentifier,
}

#[derive(Clone, Debug)]
enum PartIdentifier {
    X,
    M,
    A,
    S,
}

#[derive(Clone, Debug)]
enum Condition {
    GreaterThan,
    LessThan,
}

#[derive(Clone, Debug)]
enum WorkflowIdentifier {
    Accepted,
    Rejected,
    Workflow(String),
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let workflow_name = one_of('a'..='z')
            .repeated()
            .at_least(1)
            .to_slice()
            .map(|s: &str| s.to_string());
        let workflow_identifier = choice((
            just("A").to(WorkflowIdentifier::Accepted),
            just("R").to(WorkflowIdentifier::Rejected),
            workflow_name
                .clone()
                .map(|s| WorkflowIdentifier::Workflow(s)),
        ));
        let condition = choice((
            just(">").to(Condition::GreaterThan),
            just("<").to(Condition::LessThan),
        ));
        let part_identifider = choice((
            just("x").to(PartIdentifier::X),
            just("m").to(PartIdentifier::M),
            just("a").to(PartIdentifier::A),
            just("s").to(PartIdentifier::S),
        ));
        let conditional_workflow = part_identifider
            .then(condition)
            .then(parse_usize())
            .then_ignore(just(":"))
            .then(workflow_identifier.clone())
            .map(
                |(((part_identifier, condition), condition_value), workflow_identifier)| {
                    ConditionalWorkflow {
                        part_identifier,
                        condition,
                        condition_value,
                        workflow_identifier,
                    }
                },
            );
        let workflow = workflow_name
            .then_ignore(just("{"))
            .then(
                conditional_workflow
                    .separated_by(just(","))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(","))
            .then(workflow_identifier)
            .then_ignore(just("}"))
            .map(|((key, conditional_workflows), default)| Workflow {
                key,
                conditional_workflows,
                default,
            });
        let part = just("{x=")
            .ignore_then(parse_usize())
            .then_ignore(just(",m="))
            .then(parse_usize())
            .then_ignore(just(",a="))
            .then(parse_usize())
            .then_ignore(just(",s="))
            .then(parse_usize())
            .then_ignore(just("}"))
            .map(|(((x, m), a), s)| Part { x, m, a, s });

        workflow
            .map(|workflow| (workflow.key.clone(), workflow))
            .separated_by(newline())
            .at_least(1)
            .collect::<HashMap<_, _>>()
            .then_ignore(newline().repeated().exactly(2))
            .then(parse_lines(part))
            .map(|(workflows, parts)| Input { workflows, parts })
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "Find all valid part ranges instead of testing individual parts"
    )]
    range: bool,
}

struct Day19 {}

#[derive(Clone, Copy)]
struct Range {
    low: usize,
    high: usize,
}

impl Range {
    fn get_intersection(&self, other: &Range) -> Option<Range> {
        if other.low > self.high || self.low > other.high {
            None
        } else {
            Some(Range {
                low: max(self.low, other.low),
                high: min(self.high, other.high),
            })
        }
    }

    fn get_non_overlapping_and_intsersection(&self, other: &Range) -> (Vec<Range>, Option<Range>) {
        let intersection = self.get_intersection(other);
        let non_overlapping = match intersection {
            Some(overlap) => {
                let mut result = Vec::new();
                if self.low < overlap.low {
                    result.push(Range {
                        low: self.low,
                        high: overlap.low - 1,
                    });
                }

                if self.high > overlap.high {
                    result.push(Range {
                        low: overlap.high + 1,
                        high: self.high,
                    });
                }

                result
            }
            None => vec![Range { ..*self }],
        };

        (non_overlapping, intersection)
    }

    fn get_total(&self) -> usize {
        self.high - self.low + 1
    }
}

struct PartRange {
    x: Range,
    m: Range,
    a: Range,
    s: Range,
}

impl Problem<Input, CommandLineArguments> for Day19 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        if arguments.range {
            let mut accepted = Vec::new();
            let mut rejected = Vec::new();
            let mut queue = VecDeque::new();

            let start = input.workflows.get("in").expect("Start exists");

            queue.push_back((
                start,
                PartRange {
                    x: Range { low: 1, high: 4000 },
                    m: Range { low: 1, high: 4000 },
                    a: Range { low: 1, high: 4000 },
                    s: Range { low: 1, high: 4000 },
                },
            ));

            while let Some((workflow, part_range)) = queue.pop_front() {
                let mut ranges_to_process = vec![part_range];
                let mut range_results = Vec::new();
                for conditional_workflow in workflow.conditional_workflows.iter() {
                    let (completed, to_process): (Vec<_>, Vec<_>) = ranges_to_process
                        .into_iter()
                        .flat_map(|range| {
                            process_ranged_conditional_workflow(&range, conditional_workflow)
                        })
                        .partition(|(workflow_identifier, _)| workflow_identifier.is_some());
                    completed
                        .into_iter()
                        .map(|(workflow_identifier, range)| {
                            (workflow_identifier.expect("Already checked"), range)
                        })
                        .for_each(|item| range_results.push(item));
                    ranges_to_process = to_process.into_iter().map(|(_, range)| range).collect();
                }
                ranges_to_process
                    .into_iter()
                    .map(|range| (workflow.default.clone(), range))
                    .for_each(|item| range_results.push(item));

                range_results
                    .into_iter()
                    .for_each(|(workflow_identifier, range)| match workflow_identifier {
                        WorkflowIdentifier::Accepted => accepted.push(range),
                        WorkflowIdentifier::Rejected => rejected.push(range),
                        WorkflowIdentifier::Workflow(identifier) => queue.push_back((
                            input.workflows.get(&identifier).expect("Workflow exists"),
                            range,
                        )),
                    })
            }

            accepted
                .into_iter()
                .map(|part_range| get_xmas_combinations(&part_range))
                .sum()
        } else {
            let mut accepted = Vec::new();
            let mut rejected = Vec::new();
            let mut queue = VecDeque::new();

            let start = input.workflows.get("in").expect("Start exists");

            input
                .parts
                .into_iter()
                .map(|part| (start, part))
                .for_each(|packet| queue.push_back(packet));

            while let Some((workflow, part)) = queue.pop_front() {
                let result = workflow
                    .conditional_workflows
                    .iter()
                    .find_map(|conditional_workflow| {
                        process_conditional_workflow(&part, &conditional_workflow)
                    })
                    .unwrap_or(workflow.default.clone());

                match result {
                    WorkflowIdentifier::Accepted => accepted.push(part),
                    WorkflowIdentifier::Rejected => rejected.push(part),
                    WorkflowIdentifier::Workflow(identifier) => queue.push_back((
                        input.workflows.get(&identifier).expect("Workflow exists"),
                        part,
                    )),
                }
            }

            accepted
                .into_iter()
                .map(|part| get_xmas_rating(&part))
                .sum()
        }
    }
}

fn process_ranged_conditional_workflow(
    part_range: &PartRange,
    conditional_workflow: &ConditionalWorkflow,
) -> Vec<(Option<WorkflowIdentifier>, PartRange)> {
    let operand = match conditional_workflow.part_identifier {
        PartIdentifier::X => part_range.x,
        PartIdentifier::M => part_range.m,
        PartIdentifier::A => part_range.a,
        PartIdentifier::S => part_range.s,
    };

    let match_range = match conditional_workflow.condition {
        Condition::GreaterThan => Range {
            low: conditional_workflow.condition_value + 1,
            high: 4000,
        },
        Condition::LessThan => Range {
            low: 1,
            high: conditional_workflow.condition_value - 1,
        },
    };

    let (non_overlapping, overlapping) =
        operand.get_non_overlapping_and_intsersection(&match_range);

    non_overlapping
        .into_iter()
        .map(|range| match conditional_workflow.part_identifier {
            PartIdentifier::X => PartRange {
                x: range,
                ..*part_range
            },
            PartIdentifier::M => PartRange {
                m: range,
                ..*part_range
            },
            PartIdentifier::A => PartRange {
                a: range,
                ..*part_range
            },
            PartIdentifier::S => PartRange {
                s: range,
                ..*part_range
            },
        })
        .map(|range| (None, range))
        .chain(overlapping.into_iter().map(|range| {
            (
                Some(conditional_workflow.workflow_identifier.clone()),
                match conditional_workflow.part_identifier {
                    PartIdentifier::X => PartRange {
                        x: range,
                        ..*part_range
                    },
                    PartIdentifier::M => PartRange {
                        m: range,
                        ..*part_range
                    },
                    PartIdentifier::A => PartRange {
                        a: range,
                        ..*part_range
                    },
                    PartIdentifier::S => PartRange {
                        s: range,
                        ..*part_range
                    },
                },
            )
        }))
        .collect()
}

fn process_conditional_workflow(
    part: &Part,
    conditional_workflow: &ConditionalWorkflow,
) -> Option<WorkflowIdentifier> {
    let operand = match conditional_workflow.part_identifier {
        PartIdentifier::X => part.x,
        PartIdentifier::M => part.m,
        PartIdentifier::A => part.a,
        PartIdentifier::S => part.s,
    };

    let result = match conditional_workflow.condition {
        Condition::GreaterThan => operand > conditional_workflow.condition_value,
        Condition::LessThan => operand < conditional_workflow.condition_value,
    };

    if result {
        Some(conditional_workflow.workflow_identifier.clone())
    } else {
        None
    }
}

fn get_xmas_rating(part: &Part) -> usize {
    part.x + part.m + part.a + part.s
}

fn get_xmas_combinations(part_range: &PartRange) -> usize {
    part_range.x.get_total()
        * part_range.m.get_total()
        * part_range.a.get_total()
        * part_range.s.get_total()
}
