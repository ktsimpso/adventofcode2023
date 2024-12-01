use std::{io, path::PathBuf, thread, time::Duration};

use anyhow::{Ok, Result};
use chrono::{Local, NaiveTime};
use clap::{ArgMatches, Args, Command};
use cookie_store::CookieStore;
use dialoguer::Confirm;
use scraper::{Html, Selector};
use tap::Tap;
use ureq::{Agent, AgentBuilder, Cookie};
use url::Url;

use crate::libs::{cli::CliArgs, file_system::save_string_to_file};

#[derive(Args)]
struct CommandLineArguments {
    #[arg(short, long, help = "The day to download the input from")]
    day: usize,

    #[arg(
        short,
        long,
        env = "AOC_SESSION",
        help = "The advent of code session token that can be found in your cookies."
    )]
    session: String,

    #[arg(short, long, help = "Always download the input file")]
    force: bool,

    #[arg(
        short = 't',
        long = "time",
        help = "Wait until the specified time before attempting to download the file. Always assumes this time is in the future."
    )]
    download_time: Option<String>,

    #[arg(short, long, help = "Also attempt to parse the sample input")]
    parse_sample: bool,
}

pub fn command() -> Command {
    CommandLineArguments::augment_args(Command::new("download_input"))
        .about("Downloads the input file for a particular problem day and saves it to input/day{}/input.txt")
        .arg_required_else_help(true)
        .subcommand_negates_reqs(true)
}

pub fn run(args: &ArgMatches) -> Result<()> {
    let arguments = CommandLineArguments::parse_output(args);

    let url = Url::parse("https://adventofcode.com")?;
    let cookie = Cookie::build(("session", arguments.session))
        .domain(url.domain().expect("Domain exists"))
        .build();
    let mut cookie_store = CookieStore::default();
    cookie_store.insert_raw(&cookie, &url)?;
    let agent = AgentBuilder::new().cookie_store(cookie_store).build();

    match arguments.download_time {
        Some(time) => {
            let target_time = NaiveTime::parse_from_str(&time, "%I:%M:%S%p")?;
            let current_time = Local::now().time();

            let wait = target_time
                .signed_duration_since(current_time)
                .num_seconds();

            let wait = if wait < 0 { wait + 60 * 60 * 24 } else { wait } as u64;
            let wait = Duration::from_secs(wait);

            println!(
                "Current time: {}, target time: {}, waiting {:#?} before download",
                current_time, target_time, wait
            );

            thread::sleep(wait);

            Ok(())
        }
        None => Ok(()),
    }?;

    fetch_and_save_input_file(&agent, &url, arguments.day, arguments.force)?;

    if arguments.parse_sample {
        fetch_and_save_samples(&agent, &url, arguments.day, arguments.force)
    } else {
        Ok(())
    }
}

fn fetch_and_save_input_file(agent: &Agent, url: &Url, day: usize, force: bool) -> Result<()> {
    let input_file =
        &PathBuf::new().tap_mut(|path| path.push(format!("input/day{:0>2}/input.txt", day)));

    if input_file.exists() && !force {
        let confirm = Confirm::new()
            .with_prompt("Input file for this day already exists, overwrite?")
            .interact()?;

        if !confirm {
            println!("Not downloading the input file");
            return Ok(());
        }
    }

    println!("Downloading the input file");
    let result = agent
        .get(&format!("{}2023/day/{}/input", url.as_str(), day))
        .call()?
        .into_string()?;

    println!("Saving file to disk");
    save_string_to_file(&result, input_file).map_err(|e| e.into())
}

fn fetch_and_save_samples(agent: &Agent, url: &Url, day: usize, force: bool) -> Result<()> {
    let sample_file = sample_file_from_index(day, 0);

    if sample_file.exists() && !force {
        let confirm = Confirm::new()
            .with_prompt("Sample file for this day already exists, overwrite?")
            .interact()?;

        if !confirm {
            println!("Not downloading the sample file");
            return Ok(());
        }
    }

    println!("Downloading the page information");
    let result = agent
        .get(&format!("{}2023/day/{}", url.as_str(), day))
        .call()?
        .into_string()?;

    let html = Html::parse_document(&result);
    let code_blocks_selector = Selector::parse("code")
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e.to_string()))?;

    let mut sample_index = 0;

    html.select(&code_blocks_selector).find_map(|code_block| {
        code_block.text().last().and_then(|code_text| {
            println!("Found:");
            println!("{}", code_text);
            Confirm::new()
                .with_prompt("Is this a sample?")
                .interact()
                .map_err(anyhow::Error::new)
                .and_then(|is_sample| {
                    if is_sample {
                        let file_name = sample_file_from_index(day, sample_index);
                        println!(
                            "Saving file to {}",
                            file_name.to_str().expect("path exists")
                        );

                        save_string_to_file(code_text, &file_name)?;
                        sample_index += 1;

                        Confirm::new()
                            .with_prompt("Are there more samples?")
                            .interact()
                            .map_err(anyhow::Error::new)
                    } else {
                        Ok(true)
                    }
                })
                .map(|next| if !next { Some(()) } else { None })
                .unwrap_or(None)
        })
    });

    Ok(())
}

fn sample_file_from_index(day: usize, index: usize) -> PathBuf {
    let sample_number = if index == 0 {
        "".to_string()
    } else {
        (index + 1).to_string()
    };

    PathBuf::new()
        .tap_mut(|path| path.push(format!("input/day{:0>2}/sample{}.txt", day, sample_number)))
}
