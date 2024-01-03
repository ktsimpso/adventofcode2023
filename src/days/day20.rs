use crate::libs::{
    cli::{single_arg, CliArgs, CliProblem, Command},
    parse::{parse_alphanumeric, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{
    container::Seq,
    error::Rich,
    extra,
    primitive::{choice, just},
    IterParser, Parser,
};
use clap::{value_parser, Arg, ArgMatches};
use num_integer::Integer;
use std::{
    cell::LazyCell,
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
};
use tap::Tap;

pub const DAY_20: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day20>::new(
            "day20",
            "Calculates properties of a pulse state machine",
            "Newline separated description of each module in the state machine",
        )
        .with_part(
            "Calcuted the product of the high and lower pulses after 1000 button presses.",
            CommandLineArguments {
                module_stat: ModuleStat::PulseCount(1000),
            },
        )
        .with_part(
            "Calculates when rx will recieve a low signal",
            CommandLineArguments {
                module_stat: ModuleStat::Target("rx".to_string()),
            },
        ),
    )
});

struct Input(Vec<Module>);

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
struct Module {
    name: String,
    module_type: ModuleType,
    outputs: Vec<String>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Pulse {
    Low,
    High,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
enum ModuleType {
    Broadcast,
    FlipFlop,
    Conjunction,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let module_type_and_name = choice((
            just("broadcaster").to((ModuleType::Broadcast, "broadcaster".to_string())),
            just("%")
                .ignore_then(parse_alphanumeric())
                .map(|name: &str| (ModuleType::FlipFlop, name.to_string())),
            just("&")
                .ignore_then(parse_alphanumeric())
                .map(|name: &str| (ModuleType::Conjunction, name.to_string())),
        ));

        let module = module_type_and_name
            .then_ignore(just(" -> "))
            .then(
                parse_alphanumeric()
                    .map(|s: &str| s.to_string())
                    .separated_by(just(", "))
                    .collect::<Vec<_>>(),
            )
            .map(|((module_type, name), outputs)| Module {
                module_type,
                name,
                outputs,
            });
        parse_lines(module).map(Input)
    }
}

struct CommandLineArguments {
    module_stat: ModuleStat,
}

impl CliArgs for CommandLineArguments {
    fn get_args() -> Vec<Arg> {
        let count = single_arg(
            "count",
            'c',
            "Calculates the product of the high pulses emitted and low pulses emitted after <COUNT> button presses",
        )
        .value_parser(value_parser!(usize));
        let target = single_arg(
            "target",
            't',
            "Calculates how many button presses it takes before <TARGET> receives a low pulse.",
        )
        .value_parser(value_parser!(String))
        .conflicts_with("count");
        vec![count, target]
    }

    fn parse_output(args: &ArgMatches) -> Self {
        let count = args.get_one::<usize>("count");
        let target = args.get_one::<String>("target");

        let module_stat = match (count, target) {
            (Some(count), None) => ModuleStat::PulseCount(*count),
            (None, Some(target)) => ModuleStat::Target(target.clone()),
            _ => unreachable!(),
        };

        CommandLineArguments { module_stat }
    }
}

enum ModuleStat {
    PulseCount(usize),
    Target(String),
}
struct Day20 {}

impl Problem<Input, CommandLineArguments> for Day20 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let modules = input
            .0
            .into_iter()
            .map(|module| (module.name.clone(), module))
            .collect::<HashMap<_, _>>();
        let conjunction_inputs = modules
            .values()
            .filter(|module| module.module_type == ModuleType::Conjunction)
            .map(|module| module.name.clone())
            .collect::<HashSet<_>>();
        let conjunction_inputs = modules.values().fold(HashMap::new(), |mut acc, module| {
            module
                .outputs
                .iter()
                .filter(|output| conjunction_inputs.contains(*output))
                .for_each(|output| {
                    let inputs = acc.entry(output).or_insert_with(|| Vec::new());
                    inputs.push(module.name.clone());
                });
            acc
        });
        let mut conjuntion_memory = conjunction_inputs
            .clone()
            .into_iter()
            .map(|(key, inputs)| {
                (
                    key,
                    inputs
                        .into_iter()
                        .map(|input| (input, Pulse::Low))
                        .collect(),
                )
            })
            .collect::<BTreeMap<_, _>>();

        let mut flip_flop_memory = modules
            .values()
            .filter(|module| module.module_type == ModuleType::FlipFlop)
            .map(|module| (module.name.clone(), Pulse::Low))
            .collect::<BTreeMap<_, _>>();

        match &arguments.module_stat {
            ModuleStat::PulseCount(presses) => {
                let mut low_pulse_count = 0;
                let mut high_pulse_count = 0;
                let mut target_pulses = BTreeMap::new();

                (0..*presses).into_iter().for_each(|_| {
                    let (low, high) = push_button(
                        &modules,
                        &mut conjuntion_memory,
                        &mut flip_flop_memory,
                        &mut target_pulses,
                    );
                    low_pulse_count += low;
                    high_pulse_count += high;
                });

                low_pulse_count * high_pulse_count
            }
            ModuleStat::Target(target) => {
                let mut count = 0usize;
                let mut target_pulses = BTreeMap::new().tap_mut(|map| {
                    map.insert(target.clone(), false);
                });
                let mut conjuctions = Vec::new();
                let mut conjunction_input_times = BTreeMap::new();

                modules
                    .values()
                    .filter(|module| module.outputs.contains(target))
                    .filter(|module| module.module_type == ModuleType::Conjunction)
                    .for_each(|module| {
                        conjuctions.push(module.name.clone());
                        conjunction_inputs
                            .get(&module.name)
                            .expect("exists")
                            .into_iter()
                            .for_each(|input| {
                                target_pulses.insert(input.clone(), false);
                            })
                    });

                loop {
                    count += 1;
                    let mut next_pulses = target_pulses.clone();
                    push_button(
                        &modules,
                        &mut conjuntion_memory,
                        &mut flip_flop_memory,
                        &mut next_pulses,
                    );

                    if *next_pulses.get(target).expect("exists") {
                        break;
                    }

                    next_pulses
                        .into_iter()
                        .filter(|(_, pulsed)| *pulsed)
                        .for_each(|(key, _)| {
                            target_pulses.remove(&key);
                            conjunction_input_times.insert(key, count);
                        });

                    if let Some(result) = conjuctions.iter().find_map(|conjunction| {
                        conjunction_inputs
                            .get(conjunction)
                            .expect("Exists")
                            .into_iter()
                            .all(|value| conjunction_input_times.contains_key(value))
                            .then(|| {
                                conjunction_inputs
                                    .get(&conjunction)
                                    .expect("Exists")
                                    .into_iter()
                                    .map(|input| {
                                        conjunction_input_times.get(input).expect("Exists")
                                    })
                                    .fold(1, |acc, count| acc.lcm(count))
                            })
                    }) {
                        count = result;
                        break;
                    }
                }

                count
            }
        }
    }
}

fn push_button(
    modules: &HashMap<String, Module>,
    conjuntion_memory: &mut BTreeMap<&String, BTreeMap<String, Pulse>>,
    flip_flop_memory: &mut BTreeMap<String, Pulse>,
    target_pulses: &mut BTreeMap<String, bool>,
) -> (usize, usize) {
    let mut low_pulse_count = 0;
    let mut high_pulse_count = 0;
    let mut pulses = VecDeque::new();
    pulses.push_back(("button".to_string(), Pulse::Low, "broadcaster".to_string()));

    while let Some((source, pulse, module_name)) = pulses.pop_front() {
        match pulse {
            Pulse::Low => low_pulse_count += 1,
            Pulse::High => high_pulse_count += 1,
        }

        match modules.get(&module_name) {
            Some(module) => {
                match module.module_type {
                    ModuleType::Conjunction => {
                        if let Some(target) = target_pulses.get_mut(&source)
                            && pulse == Pulse::High
                        {
                            *target = true;
                        }
                    }
                    _ => (),
                }

                process_pulse(pulse, source, module, conjuntion_memory, flip_flop_memory)
                    .into_iter()
                    .for_each(|item| pulses.push_back(item));
            }
            None => {
                if let Some(target) = target_pulses.get_mut(&source)
                    && pulse == Pulse::Low
                {
                    *target = true;
                }
            }
        }
    }

    (low_pulse_count, high_pulse_count)
}

fn process_pulse(
    pulse: Pulse,
    source: String,
    module: &Module,
    conjuntion_memory: &mut BTreeMap<&String, BTreeMap<String, Pulse>>,
    flip_flop_memory: &mut BTreeMap<String, Pulse>,
) -> Vec<(String, Pulse, String)> {
    match module.module_type {
        ModuleType::Broadcast => module
            .outputs
            .iter()
            .map(|output| (module.name.to_owned(), pulse.clone(), output.to_owned()))
            .collect(),
        ModuleType::FlipFlop => match pulse {
            Pulse::Low => {
                let state = flip_flop_memory
                    .get_mut(&module.name)
                    .expect("module exists");
                match state {
                    Pulse::Low => {
                        *state = Pulse::High;
                        module
                            .outputs
                            .iter()
                            .map(|output| (module.name.to_owned(), Pulse::High, output.to_owned()))
                            .collect()
                    }
                    Pulse::High => {
                        *state = Pulse::Low;
                        module
                            .outputs
                            .iter()
                            .map(|output| (module.name.to_owned(), Pulse::Low, output.to_owned()))
                            .collect()
                    }
                }
            }
            Pulse::High => Vec::new(),
        },
        ModuleType::Conjunction => {
            {
                *conjuntion_memory
                    .get_mut(&module.name)
                    .expect("Module exists")
                    .get_mut(&source)
                    .expect("Exists") = pulse;
            }
            let send = if conjuntion_memory
                .get(&module.name)
                .expect("Module exists")
                .values()
                .all(|pulse| pulse == &Pulse::High)
            {
                Pulse::Low
            } else {
                Pulse::High
            };

            module
                .outputs
                .iter()
                .map(|output| (module.name.to_owned(), send.clone(), output.to_owned()))
                .collect()
        }
    }
}
