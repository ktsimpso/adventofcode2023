use crate::libs::{
    cli::{CliProblem, Command},
    parse::{parse_usize, StringParse},
    problem::Problem,
};
use chumsky::{
    error::Rich,
    extra,
    primitive::{end, just},
    text::newline,
    IterParser, Parser,
};
use clap::Args;
use either::Either;
use itertools::Itertools;
use std::{
    cell::LazyCell,
    cmp::{max, min},
};
use tap::Tap;

pub const DAY_05: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day05>::new(
            "day05",
            "Figures out where the seeds should be planted",
            "The seeds to be planted followed by the almanac to see where to plan them",
        )
        .with_part(
            "Finds the lowest location where seeds should be planted.",
            CommandLineArguments { ranges: false },
        )
        .with_part(
            "Finds the lowest location where the seeds are ranges.",
            CommandLineArguments { ranges: true },
        ),
    )
});

#[derive(Debug)]
struct Input {
    seeds: Vec<usize>,
    almanac: Almanac,
}

#[derive(Debug)]
struct Almanac {
    seed_to_soil: Vec<MapValue>,
    soil_to_fert: Vec<MapValue>,
    fert_to_water: Vec<MapValue>,
    water_to_light: Vec<MapValue>,
    ligh_to_temp: Vec<MapValue>,
    temp_to_humidity: Vec<MapValue>,
    humidity_to_location: Vec<MapValue>,
}

#[derive(Debug)]
struct MapValue {
    source_start: usize,
    destination_start: usize,
    range: usize,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let blank_line = newline().repeated().exactly(2);
        let almanac = parse_map("seed-to-soil")
            .then_ignore(blank_line)
            .then(parse_map("soil-to-fertilizer"))
            .then_ignore(blank_line)
            .then(parse_map("fertilizer-to-water"))
            .then_ignore(blank_line)
            .then(parse_map("water-to-light"))
            .then_ignore(blank_line)
            .then(parse_map("light-to-temperature"))
            .then_ignore(blank_line)
            .then(parse_map("temperature-to-humidity"))
            .then_ignore(blank_line)
            .then(parse_map("humidity-to-location"))
            .then_ignore(newline())
            .then_ignore(end())
            .map(
                |(
                    (
                        (
                            (((seed_to_soil, soil_to_fert), fert_to_water), water_to_light),
                            ligh_to_temp,
                        ),
                        temp_to_humidity,
                    ),
                    humidity_to_location,
                )| {
                    Almanac {
                        seed_to_soil,
                        soil_to_fert,
                        fert_to_water,
                        water_to_light,
                        ligh_to_temp,
                        temp_to_humidity,
                        humidity_to_location,
                    }
                },
            );
        just("seeds: ")
            .ignore_then(parse_usize().separated_by(just(" ")).collect())
            .then_ignore(blank_line)
            .then(almanac)
            .map(|(seeds, almanac)| Input { seeds, almanac })
    }
}

fn parse_map<'a>(
    map_name: &'a str,
) -> impl Parser<'a, &'a str, Vec<MapValue>, extra::Err<Rich<'a, char>>> {
    just(map_name)
        .ignored()
        .then_ignore(just(" map:"))
        .then_ignore(newline())
        .then(
            parse_map_value()
                .separated_by(newline())
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|(_, result)| {
            result.tap_mut(|r| r.sort_by(|a, b| a.source_start.cmp(&b.source_start)))
        })
}

fn parse_map_value<'a>() -> impl Parser<'a, &'a str, MapValue, extra::Err<Rich<'a, char>>> {
    parse_usize()
        .then_ignore(just(" "))
        .then(parse_usize())
        .then_ignore(just(" "))
        .then(parse_usize())
        .map(|((destination_start, source_start), range)| MapValue {
            source_start,
            destination_start,
            range,
        })
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "If the seed input should be interperted as a range"
    )]
    ranges: bool,
}

struct Range {
    start: usize,
    end: usize,
}

struct Day05 {}

impl Problem<Input, CommandLineArguments> for Day05 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let chunks = input.seeds.clone().into_iter().chunks(2);
        if arguments.ranges {
            Either::Left(chunks.into_iter().map(|chunk| {
                let mut chunk = chunk.into_iter();
                let first = chunk.next().expect("Seed start exists");
                let range = chunk.next().expect("Range exists");
                Range {
                    start: first,
                    end: first + range - 1,
                }
            }))
        } else {
            Either::Right(input.seeds.into_iter().map(|seed| Range {
                start: seed,
                end: seed,
            }))
        }
        .flat_map(|seed| {
            map_destination_ranged_from_source_range(seed, &input.almanac.seed_to_soil)
        })
        .flat_map(|soil| {
            map_destination_ranged_from_source_range(soil, &input.almanac.soil_to_fert)
        })
        .flat_map(|fert| {
            map_destination_ranged_from_source_range(fert, &input.almanac.fert_to_water)
        })
        .flat_map(|water| {
            map_destination_ranged_from_source_range(water, &input.almanac.water_to_light)
        })
        .flat_map(|light| {
            map_destination_ranged_from_source_range(light, &input.almanac.ligh_to_temp)
        })
        .flat_map(|temp| {
            map_destination_ranged_from_source_range(temp, &input.almanac.temp_to_humidity)
        })
        .flat_map(|humidity| {
            map_destination_ranged_from_source_range(humidity, &input.almanac.humidity_to_location)
        })
        .map(|range| range.start)
        .min()
        .expect("location found")
    }
}

fn map_destination_ranged_from_source_range(
    source: Range,
    source_map: &Vec<MapValue>,
) -> Vec<Range> {
    // this works because source_map is sorted by the source range
    let (remaining, mut result) = source_map.into_iter().fold(
        (Some(source), Vec::new()),
        |(range, mut acc), destination_range| match range {
            Some(source) => {
                let destination_source_end =
                    destination_range.source_start + destination_range.range - 1;

                if source.start < destination_range.source_start {
                    acc.push(Range {
                        start: source.start,
                        end: min(source.end, destination_range.source_start - 1),
                    });
                }

                if source.end >= destination_range.source_start
                    && source.start <= destination_source_end
                {
                    acc.push(Range {
                        start: max(source.start, destination_range.source_start)
                            - destination_range.source_start
                            + destination_range.destination_start,
                        end: min(source.end, destination_source_end)
                            - destination_range.source_start
                            + destination_range.destination_start,
                    });
                }

                let remaining = if source.end > destination_source_end {
                    Some(Range {
                        start: max(source.start, destination_source_end + 1),
                        end: source.end,
                    })
                } else {
                    None
                };

                (remaining, acc)
            }
            None => (None, acc),
        },
    );
    remaining.into_iter().for_each(|range| result.push(range));

    result
}
