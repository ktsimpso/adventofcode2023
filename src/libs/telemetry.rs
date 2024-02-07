use itertools::Itertools;
use minitrace::collector::Reporter;
use std::{
    borrow::Cow,
    sync::{Arc, Mutex},
    time::Duration,
};

pub struct DayCollector {
    day_results: Vec<DayResult>,
}

struct DayResult {
    day: Cow<'static, str>,
    run_value: Cow<'static, str>,
    parse_time: Duration,
    run_time: Duration,
    total_time: Duration,
}

impl DayCollector {
    pub const fn new() -> Self {
        DayCollector {
            day_results: Vec::new(),
        }
    }

    fn add_results(&mut self, to_add: impl Iterator<Item = DayResult>) {
        self.day_results.extend(to_add)
    }

    pub fn print_results(&self) {
        self.day_results
            .iter()
            .sorted_by(|results1, results2| match results1.day.cmp(&results2.day) {
                std::cmp::Ordering::Equal => results1.run_value.cmp(&results2.run_value),
                result => result,
            })
            .for_each(|result| {
                println!(
                    "{} {}, parse: {}, run: {}, total: {}",
                    result.day,
                    result.run_value,
                    formatted_duration(&result.parse_time, 1),
                    formatted_duration(&result.run_time, 19),
                    formatted_duration(&result.total_time, 20)
                );
            });
        if self.day_results.len() > 1 {
            let (total_parse, total_run, total) = self.day_results.iter().fold(
                (Duration::ZERO, Duration::ZERO, Duration::ZERO),
                |(total_parse, total_run, total), result| {
                    (
                        total_parse + result.parse_time,
                        total_run + result.run_time,
                        total + result.total_time,
                    )
                },
            );
            println!(
                "Totals,      parse: {}, run: {}, total: {}",
                formatted_duration(&total_parse, 50),
                formatted_duration(&total_run, 950),
                formatted_duration(&total, 1000)
            )
        }
    }
}

pub struct DayReporter {
    pub collector: Arc<Mutex<DayCollector>>,
}

impl Reporter for DayReporter {
    fn report(&mut self, spans: &[minitrace::prelude::SpanRecord]) {
        let results = spans
            .iter()
            .map(|span| (span.trace_id, span))
            .into_group_map()
            .into_values()
            .map(|record| {
                let parse_time = record
                    .iter()
                    .find(|span| span.name == "parse_input")
                    .map(|span| Duration::from_nanos(span.duration_ns))
                    .expect("Parse exists");
                let run_time = record
                    .iter()
                    .find(|span| span.name == "run_time")
                    .map(|span| Duration::from_nanos(span.duration_ns))
                    .expect("Runtime exists");
                let (day, run_value, total_time) = record
                    .into_iter()
                    .find(|span| span.name == "run_part_total")
                    .map(|span| {
                        (
                            &span.properties[0].1,
                            &span.properties[1].1,
                            Duration::from_nanos(span.duration_ns),
                        )
                    })
                    .expect("Total exists");
                DayResult {
                    day: day.clone(),
                    run_value: run_value.clone(),
                    parse_time,
                    run_time,
                    total_time,
                }
            });
        self.collector
            .lock()
            .expect("No panics")
            .add_results(results);
    }
}

fn formatted_duration(duration: &Duration, baseline_ms: u64) -> String {
    let baseline = Duration::from_millis(baseline_ms);

    let time_color = match duration {
        x if x <= &(baseline / 4) => "\x1b[92m",
        x if x <= &baseline => "\x1b[32m",
        x if x <= &(baseline * 2) => "\x1b[93m",
        x if x <= &(baseline * 3) => "\x1b[33m",
        x if x <= &(baseline * 5) => "\x1b[91m",
        _ => "\x1b[31m",
    };

    format!("{}{:>12?}\x1b[0m", time_color, duration)
}
