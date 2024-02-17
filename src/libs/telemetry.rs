use itertools::Itertools;
use minitrace::{
    collector::{Config, Reporter, SpanContext},
    local::{LocalParentGuard, LocalSpan},
    Span,
};
use std::{borrow::Cow, time::Duration};

#[cfg(feature = "memory-analysis")]
use dhat::Profiler;
#[cfg(feature = "memory-analysis")]
use minitrace::Event;
#[cfg(feature = "memory-analysis")]
use size::Size;

#[cfg(feature = "memory-analysis")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

pub struct Telemetry {
    #[cfg(feature = "memory-analysis")]
    _memory_profiler: Profiler,
}

impl Telemetry {
    pub fn init_telemetry() -> Self {
        #[cfg(feature = "memory-analysis")]
        let profiler = Profiler::builder().testing().build();
        minitrace::set_reporter(
            DayReporter {
                collector: DayCollector::new(),
            },
            Config::default(),
        );
        #[cfg(not(feature = "memory-analysis"))]
        {
            Telemetry {}
        }
        #[cfg(feature = "memory-analysis")]
        {
            Telemetry {
                _memory_profiler: profiler,
            }
        }
    }
}

impl Drop for Telemetry {
    fn drop(&mut self) {
        minitrace::flush();
        minitrace::set_reporter(NopReporter {}, Config::default());
    }
}

pub struct RunPartTelemetry {
    #[cfg(feature = "memory-analysis")]
    _heap_tracker: HeapTracker,
    _root_guard: LocalParentGuard,
    _root: Span,
}

impl RunPartTelemetry {
    pub fn new(day: &'static str, run_value: &'static str) -> Self {
        let root = Span::root("run_part_total", SpanContext::random())
            .with_properties(|| [("day", day), ("run_value", run_value)]);
        #[cfg(not(feature = "memory-analysis"))]
        {
            RunPartTelemetry {
                _root_guard: root.set_local_parent(),
                _root: root,
            }
        }
        #[cfg(feature = "memory-analysis")]
        {
            RunPartTelemetry {
                _heap_tracker: HeapTracker::new(),
                _root_guard: root.set_local_parent(),
                _root: root,
            }
        }
    }

    pub fn time_parse(&self) -> LocalSpan {
        LocalSpan::enter_with_local_parent("parse_input")
    }

    pub fn time_run(&self) -> LocalSpan {
        LocalSpan::enter_with_local_parent("run_time")
    }
}

#[cfg(feature = "memory-analysis")]
struct HeapTracker {
    start_bytes: u64,
}

#[cfg(feature = "memory-analysis")]
impl HeapTracker {
    fn new() -> Self {
        HeapTracker {
            start_bytes: dhat::HeapStats::get().total_bytes,
        }
    }
}

#[cfg(feature = "memory-analysis")]
impl Drop for HeapTracker {
    fn drop(&mut self) {
        let end_stats = dhat::HeapStats::get();
        Event::add_to_local_parent("memory", || {
            [(
                "memory".into(),
                format!(
                    "{}",
                    Size::from_bytes(end_stats.total_bytes - self.start_bytes)
                )
                .into(),
            )]
        });
    }
}

struct DayResult {
    day: Cow<'static, str>,
    run_value: Cow<'static, str>,
    parse_time: Duration,
    run_time: Duration,
    total_time: Duration,
    #[cfg(feature = "memory-analysis")]
    memory: Cow<'static, str>,
}

struct DayCollector {
    day_results: Vec<DayResult>,
}

impl Drop for DayCollector {
    fn drop(&mut self) {
        self.print_results()
    }
}

impl DayCollector {
    const fn new() -> Self {
        DayCollector {
            day_results: Vec::new(),
        }
    }

    fn add_results(&mut self, to_add: impl Iterator<Item = DayResult>) {
        self.day_results.extend(to_add)
    }

    fn print_results(&self) {
        self.day_results
            .iter()
            .sorted_by(|results1, results2| match results1.day.cmp(&results2.day) {
                std::cmp::Ordering::Equal => results1.run_value.cmp(&results2.run_value),
                result => result,
            })
            .for_each(|result| {
                #[cfg(not(feature = "memory-analysis"))]
                println!(
                    "{} {}, parse: {}, run: {}, total: {}",
                    result.day,
                    result.run_value,
                    formatted_duration(&result.parse_time, 1),
                    formatted_duration(&result.run_time, 19),
                    formatted_duration(&result.total_time, 20),
                );
                #[cfg(feature = "memory-analysis")]
                println!(
                    "{} {}, parse: {}, run: {}, total: {}, memory: {:>8}",
                    result.day,
                    result.run_value,
                    formatted_duration(&result.parse_time, 1),
                    formatted_duration(&result.run_time, 19),
                    formatted_duration(&result.total_time, 20),
                    result.memory,
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

struct DayReporter {
    collector: DayCollector,
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
                #[cfg(feature = "memory-analysis")]
                {
                    let (day, run_value, total_time, memory) = record
                        .into_iter()
                        .find(|span| span.name == "run_part_total")
                        .map(|span| {
                            (
                                &span.properties[0].1,
                                &span.properties[1].1,
                                Duration::from_nanos(span.duration_ns),
                                span.events
                                    .iter()
                                    .find(|record| record.name == "memory")
                                    .and_then(|record| {
                                        record
                                            .properties
                                            .iter()
                                            .find(|(name, _)| name == "memory")
                                            .map(|(_, size)| size.clone())
                                    })
                                    .expect("memory recorded"),
                            )
                        })
                        .expect("Total exists");
                    DayResult {
                        day: day.clone(),
                        run_value: run_value.clone(),
                        parse_time,
                        run_time,
                        total_time,
                        memory,
                    }
                }
                #[cfg(not(feature = "memory-analysis"))]
                {
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
                }
            });
        self.collector.add_results(results);
    }
}

struct NopReporter;

impl Reporter for NopReporter {
    fn report(&mut self, _spans: &[minitrace::prelude::SpanRecord]) {}
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

    format!("{}{:>14?}\x1b[0m", time_color, duration)
}
