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
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use std::cell::LazyCell;

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
        .map(|value| value.1)
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

struct Day05 {}

impl Problem<Input, CommandLineArguments> for Day05 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        let chunks = input.seeds.clone().into_par_iter().chunks(2);
        if arguments.ranges {
            // TODO: proper range math instead of throwing rayon at the problem
            Either::Left(chunks.into_par_iter().flat_map(|chunk| {
                let mut chunk = chunk.into_iter();
                let first = chunk.next().expect("Seed start exists");
                let range = chunk.next().expect("Range exists");
                first..(first + range)
            }))
        } else {
            Either::Right(input.seeds.into_par_iter())
        }
        .map(|seed| get_destination_from_source(seed, &input.almanac.seed_to_soil))
        .map(|soil| get_destination_from_source(soil, &input.almanac.soil_to_fert))
        .map(|fert| get_destination_from_source(fert, &input.almanac.fert_to_water))
        .map(|water| get_destination_from_source(water, &input.almanac.water_to_light))
        .map(|light| get_destination_from_source(light, &input.almanac.ligh_to_temp))
        .map(|temp| get_destination_from_source(temp, &input.almanac.temp_to_humidity))
        .map(|humidity| get_destination_from_source(humidity, &input.almanac.humidity_to_location))
        .min()
        .expect("location found")
    }
}

fn get_destination_from_source(source: usize, source_map: &Vec<MapValue>) -> usize {
    source_map
        .into_iter()
        .find(|map_value| {
            source >= map_value.source_start && source <= map_value.source_start + map_value.range
        })
        .map(|map_value| source - map_value.source_start + map_value.destination_start)
        .unwrap_or(source)
}
