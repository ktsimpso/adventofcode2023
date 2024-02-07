use std::{
    error::Error,
    fmt::{self, Display},
};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    error::{Error as ChumskyError, Rich},
    extra::{self, ParserExtra},
    input::{StrInput, ValueInput},
    primitive::{any, end, just, one_of},
    text::{self, newline, Char},
    util::MaybeRef,
    IterParser, Parser,
};
use itertools::Itertools;
use ndarray::Array2;
use tap::Tap;

pub trait StringParse: Sized {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>>;
}

pub struct StringParser<T: StringParse>(pub T);

impl<T> TryFrom<String> for StringParser<T>
where
    T: StringParse,
{
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        T::parse()
            .map(StringParser)
            .parse(&value)
            .into_result()
            .map_err(|e| ParseError::new(&value, e).into())
    }
}

pub fn parse_usize<'a>() -> impl Parser<'a, &'a str, usize, extra::Err<Rich<'a, char>>> {
    parse_usize_with_radix(10)
}

pub fn parse_usize_with_radix<'a>(
    radix: u32,
) -> impl Parser<'a, &'a str, usize, extra::Err<Rich<'a, char>>> {
    text::int(radix).try_map(move |number, span| {
        usize::from_str_radix(number, radix).map_err(|op| Rich::custom(span, op))
    })
}

pub fn parse_isize<'a>() -> impl Parser<'a, &'a str, isize, extra::Err<Rich<'a, char>>> {
    parse_isize_with_radix(10)
}

pub fn parse_isize_with_radix<'a>(
    radix: u32,
) -> impl Parser<'a, &'a str, isize, extra::Err<Rich<'a, char>>> {
    just('-')
        .or_not()
        .then(text::int(radix))
        .try_map(move |(negative, number), span| {
            let combined_number = match negative {
                Some(_) => "-".to_string().tap_mut(|c| c.push_str(number)),
                _ => number.to_string(),
            };
            isize::from_str_radix(&combined_number, radix).map_err(|op| Rich::custom(span, op))
        })
}

pub fn parse_alphanumeric<
    'a,
    I: ValueInput<'a> + StrInput<'a, C>,
    C: Char,
    E: ParserExtra<'a, I>,
>() -> impl Parser<'a, I, &'a C::Str, E> {
    any()
        .try_map(move |c: C, span| {
            if c.to_char().is_alphanumeric() {
                Ok(c)
            } else {
                Err(ChumskyError::expected_found(
                    [],
                    Some(MaybeRef::Val(c)),
                    span,
                ))
            }
        })
        .then(
            any()
                .filter(|c: &C| c.to_char().is_alphanumeric())
                .repeated(),
        )
        .ignored()
        .to_slice()
}

pub fn parse_digit<'a, I: ValueInput<'a> + StrInput<'a, char>, E: ParserExtra<'a, I>>(
) -> impl Parser<'a, I, char, E> {
    one_of('0'..='9')
}

pub fn parse_lines<'a, T>(
    line_parser: impl Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, Vec<T>, extra::Err<Rich<'a, char>>> {
    line_parser
        .separated_by(text::newline())
        .allow_trailing()
        .collect::<Vec<_>>()
        .then_ignore(end())
}

pub fn parse_table<'a, T>(
    item_parser: impl Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, Vec<Vec<T>>, extra::Err<Rich<'a, char>>> {
    parse_lines(item_parser.repeated().at_least(1).collect())
}

pub fn parse_table2<'a, T>(
    item_parser: impl Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, Array2<T>, extra::Err<Rich<'a, char>>> {
    parse_table(item_parser).try_map(|items, span| {
        let columns = items.first().map_or(0, |row| row.len());
        let rows = items.len();

        Array2::from_shape_vec(
            (rows, columns),
            items
                .into_iter()
                .fold(Vec::with_capacity(rows * columns), |mut acc, row| {
                    acc.extend(row);
                    acc
                }),
        )
        .map_err(|op| Rich::custom(span, op))
    })
}

// Note, don't use a parser with a newline delimiter and allow_trailing with this parser
pub fn parse_between_blank_lines<'a, T>(
    chunk_parser: impl Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, Vec<T>, extra::Err<Rich<'a, char>>> {
    let blank_line = newline().repeated().exactly(2).ignored();
    chunk_parser
        .separated_by(blank_line)
        .allow_trailing()
        .collect::<Vec<_>>()
        .then_ignore(newline().repeated())
}

#[derive(Debug)]
pub struct ParseError {
    error: String,
}

impl ParseError {
    pub fn new<'a>(file: &'a str, errors: Vec<Rich<'a, char>>) -> Self {
        ParseError {
            error: combine_parse_errors(file, &errors),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }
}

pub fn combine_parse_errors<'a>(source: &'a str, errors: &[Rich<'a, char>]) -> String {
    errors
        .iter()
        .map(|e| format_parse_error(source, e))
        .join("\n")
}

pub fn format_parse_error<'a>(source: &'a str, error: &Rich<'a, char>) -> String {
    let mut buf = vec![];
    dbg!(error);
    Report::build(ReportKind::Error, (), error.span().start)
        .with_message(error.to_string())
        .with_label(
            Label::new(error.span().into_range())
                .with_message(error.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
        .write(Source::from(&source), &mut buf)
        .expect("Worked");
    std::str::from_utf8(&buf[..]).unwrap().to_string()
}
