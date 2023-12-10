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
use clap::{Args, ValueEnum};
use std::{cell::LazyCell, cmp::Ordering, collections::BTreeMap};
use tap::Tap;

pub const DAY_07: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day07>::new(
            "day07",
            "Orders the best Camel Cards hands and sums their values according to thier bid",
            "Each line should have a Camel Cards hand followed by a bid",
        )
        .with_part(
            "Hands follow regular Camel cards rules, no wilds.",
            CommandLineArguments { wild: None },
        )
        .with_part(
            "Jacks are wild.",
            CommandLineArguments {
                wild: Some(Card::J),
            },
        ),
    )
});

struct Input(Vec<Hand>);

#[derive(PartialEq, Eq, Clone, Debug)]
struct Hand {
    hand: [Card; 5],
    bid: usize,
}

impl Hand {
    fn get_type(&self, wild: &Option<Card>) -> HandType {
        let mut card_set = self.hand.iter().fold(BTreeMap::new(), |mut acc, card| {
            *acc.entry(card).or_insert(0usize) += 1;
            acc
        });

        wild.into_iter().for_each(|wild_card| {
            card_set
                .remove(wild_card)
                .into_iter()
                .for_each(|wild_cards| {
                    let (top_key, top_value) = card_set
                        .iter()
                        .max_by(|(_, a), (_, b)| a.cmp(b))
                        .unwrap_or((&wild_card, &0));
                    card_set.insert(top_key, top_value + wild_cards);
                })
        });

        let max = card_set.values().max().expect("At least one card");
        let pair_count = card_set.values().filter(|v| v == &&2).count();
        match (max, pair_count) {
            (5, _) => HandType::FiveOfAKind,
            (4, _) => HandType::FourOfAKind,
            (3, 1) => HandType::FullHouse,
            (3, _) => HandType::ThreeOfAKind,
            (2, 2) => HandType::TwoPair,
            (2, 1) => HandType::Pair,
            _ => HandType::High,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, PartialOrd, Ord)]
enum HandType {
    High,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(PartialEq, Eq, Clone, Debug, PartialOrd, Ord, ValueEnum)]
enum Card {
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,
    T,
    J,
    Q,
    K,
    A,
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let card = choice((
            just('A').to(Card::A),
            just('K').to(Card::K),
            just('Q').to(Card::Q),
            just('J').to(Card::J),
            just('T').to(Card::T),
            just('9').to(Card::_9),
            just('8').to(Card::_8),
            just('7').to(Card::_7),
            just('6').to(Card::_6),
            just('5').to(Card::_5),
            just('4').to(Card::_4),
            just('3').to(Card::_3),
            just('2').to(Card::_2),
        ));
        let hand = card
            .repeated()
            .exactly(5)
            .collect_exactly()
            .then_ignore(just(" "))
            .then(parse_usize())
            .map(|(hand, bid)| Hand { hand, bid });
        parse_lines(hand).map(Input)
    }
}

#[derive(Args)]
struct CommandLineArguments {
    #[arg(
        short,
        long,
        help = "Card that is a wild card. Wild cards are the least valuable card for tiebreaks."
    )]
    wild: Option<Card>,
}

struct Day07 {}

impl Problem<Input, CommandLineArguments> for Day07 {
    type Output = usize;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        input
            .0
            .tap_mut(|hands| hands.sort_by(compare_hands(arguments.wild.clone())))
            .into_iter()
            .enumerate()
            .map(|(rank, hand)| hand.bid * (rank + 1))
            .sum()
    }
}

fn compare_hands(wild: Option<Card>) -> impl FnMut(&Hand, &Hand) -> Ordering {
    move |first, other| match first.get_type(&wild).cmp(&other.get_type(&wild)) {
        Ordering::Less => Ordering::Less,
        Ordering::Equal => first
            .hand
            .iter()
            .zip(other.hand.iter())
            .map(|(a, b)| {
                wild.as_ref()
                    .map(|wild_card| match (a, b) {
                        (a, b) if a == wild_card && b == wild_card => Ordering::Equal,
                        (a, _) if a == wild_card => Ordering::Less,
                        (_, b) if b == wild_card => Ordering::Greater,
                        (a, b) => a.cmp(&b),
                    })
                    .unwrap_or_else(|| a.cmp(&b))
            })
            .filter(|result| result != &Ordering::Equal)
            .next()
            .unwrap_or(Ordering::Equal),
        Ordering::Greater => Ordering::Greater,
    }
}
