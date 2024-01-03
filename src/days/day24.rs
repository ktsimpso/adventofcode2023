use crate::libs::{
    cli::{flag_arg, single_arg, CliArgs, CliProblem, Command},
    parse::{parse_isize, parse_lines, StringParse},
    problem::Problem,
};
use chumsky::{error::Rich, extra, primitive::just, IterParser, Parser};
use clap::{value_parser, Arg, ArgMatches};
use dashu::{integer::IBig, rational::RBig};
use itertools::Itertools;
use std::{cell::LazyCell, ops::Sub};
use tap::Conv;

pub const DAY_24: LazyCell<Box<dyn Command>> = LazyCell::new(|| {
    Box::new(
        CliProblem::<Input, CommandLineArguments, Day24>::new(
            "day24",
            "Finds information about collisions for a field of hailstones",
            "One line per hailstone. Each stone has a intial position and velocity.",
        )
        .with_part(
            "Searches a field for 2d collisions from 200_000_000_000_000 to 400_000_000_000_000",
            CommandLineArguments {
                search: Search::TwoDimensionalIntersections(
                    200_000_000_000_000,
                    400_000_000_000_000,
                ),
            },
        )
        .with_part(
            "Finds the starting position where you can intersect all hailstones",
            CommandLineArguments {
                search: Search::StoneVector,
            },
        ),
    )
});

struct Input(Vec<Hail>);

#[derive(Debug, Clone)]
struct Hail {
    point: Point,
    velocity: Point,
}

#[derive(Debug, Clone)]
struct TwoDimensionalFunction {
    m: RBig,
    b: RBig,
    x_direction: Direction,
}

#[derive(Debug, Clone)]
enum Direction {
    Positive,
    Negative,
}

impl TwoDimensionalFunction {
    fn intersection(&self, other: &TwoDimensionalFunction) -> Option<(RBig, RBig)> {
        let determinant = &self.m - &other.m;
        if determinant == RBig::ZERO {
            None
        } else {
            let x = (&other.b - &self.b) / determinant;
            let y = &self.m * &x + &self.b;
            Some((x, y))
        }
    }
}

impl Hail {
    fn get_xy_function(&self) -> TwoDimensionalFunction {
        let m = self.point.get_xy_slope(&Point {
            x: &self.point.x + &self.velocity.x,
            y: &self.point.y + &self.velocity.y,
            z: &self.point.z + &self.velocity.z,
        });
        let b = self.point.y.clone().conv::<RBig>() - (&m * self.point.x.clone().conv::<RBig>());

        let x_direction = if self.velocity.x >= IBig::ZERO {
            Direction::Positive
        } else {
            Direction::Negative
        };

        TwoDimensionalFunction { m, b, x_direction }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Point {
    x: IBig,
    y: IBig,
    z: IBig,
}

impl Point {
    fn get_xy_slope(&self, other: &Point) -> RBig {
        RBig::from_parts_signed((&other.y - &self.y).into(), (&other.x - &self.x).into())
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl StringParse for Input {
    fn parse<'a>() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> {
        let point = parse_isize()
            .separated_by(just(",").then_ignore(just(" ").repeated().at_least(1)))
            .exactly(3)
            .collect_exactly::<[isize; 3]>()
            .map(|[x, y, z]| Point {
                x: x.into(),
                y: y.into(),
                z: z.into(),
            })
            .boxed();
        let hail = point
            .clone()
            .then_ignore(just(" @").then_ignore(just(" ").repeated().at_least(1)))
            .then(point)
            .map(|(point, velocity)| Hail { point, velocity });
        parse_lines(hail).map(Input)
    }
}

struct CommandLineArguments {
    search: Search,
}

impl CliArgs for CommandLineArguments {
    fn get_args() -> Vec<Arg> {
        let stone = flag_arg("stone", 's', "Finds the position of a stone which when thown at the correct velocity will intersect all hail").conflicts_with_all(["low", "high"]);
        let low = single_arg(
            "low",
            'l',
            "The lower bound of the window to search for 2d intersections",
        )
        .value_parser(value_parser!(isize));
        let high = single_arg(
            "high",
            'h',
            "The upper bound of the window to search for 2d intersections",
        )
        .value_parser(value_parser!(isize))
        .group("2d");
        vec![stone, low, high]
    }

    fn parse_output(args: &ArgMatches) -> Self {
        let stone = if args.get_flag("stone") {
            Some(Search::StoneVector)
        } else {
            None
        };

        let low = args.get_one::<isize>("low");
        let high = args.get_one::<isize>("high");

        let search = match (stone, low, high) {
            (Some(stone), None, None) => stone,
            (None, Some(low), Some(high)) => Search::TwoDimensionalIntersections(*low, *high),
            _ => unreachable!(),
        };

        CommandLineArguments { search }
    }
}

struct Day24 {}

enum Search {
    TwoDimensionalIntersections(isize, isize),
    StoneVector,
}

impl Problem<Input, CommandLineArguments> for Day24 {
    type Output = String;

    fn run(input: Input, arguments: &CommandLineArguments) -> Self::Output {
        match arguments.search {
            Search::TwoDimensionalIntersections(low, high) => {
                let low_test = low.conv::<RBig>();
                let high_test = high.conv::<RBig>();
                input
                    .0
                    .into_iter()
                    .map(|hail| (hail.point.x.clone().conv::<RBig>(), hail.get_xy_function()))
                    .tuple_combinations()
                    .filter_map(|((ax, a), (bx, b))| {
                        a.intersection(&b).filter(|(x, _)| {
                            let a_in_range = match a.x_direction {
                                Direction::Positive => x >= &ax,
                                Direction::Negative => x <= &ax,
                            };

                            let b_in_range = match b.x_direction {
                                Direction::Positive => x >= &bx,
                                Direction::Negative => x <= &bx,
                            };
                            a_in_range && b_in_range
                        })
                    })
                    .filter(|(x, y)| {
                        x >= &low_test && x <= &high_test && y >= &low_test && y <= &high_test
                    })
                    .count()
                    .to_string()
            }
            Search::StoneVector => input
                .0
                .into_iter()
                .tuple_windows()
                .find_map(|(a, b, c)| find_line_intersection(&a, &b, &c))
                //.map(|r| dbg!(r))
                .map(|result| result.0)
                .map(|Point { x, y, z }| x + y + z)
                .expect("Result exists")
                .to_string(),
        }
    }
}

// Sage input
/*
x, y, z, a, b, c, t, u, v, x1, y1, z1, x2, y2, z2, x3, y3, z3, vx1, vy1, vz1, vx2, vy2, vz2, vx3, vy3, vz3 = var('x y z a b c t u v x1 y1 z1 x2 y2 z2 x3 y3 z3 vx1 vy1 vz1 vx2 vy2 vz2 vx3 vy3 vz3')
eq1 = x == x1 + (vx1 - a)*t
eq2 = y == y1 + (vy1 - b)*t
eq3 = z == z1 + (vz1 - c)*t
eq4 = x == x2 + (vx2 - a)*u
eq5 = y == y2 + (vy2 - b)*u
eq6 = z == z2 + (vz2 - c)*u
eq7 = x == x3 + (vx3 - a)*v
eq8 = y == y3 + (vy3 - b)*v
eq9 = z == z3 + (vz3 - c)*v
solve([eq1,eq2,eq3,eq4,eq5,eq6,eq7,eq8,eq9], x,y,z,a,b,c,t,u,v)
*/
// [[x == 472612107765508, y == 270148844447628, z == 273604689965980, a == -333, b == -5, c == 15, t == 1025376150547, u == 665047733629, v == 532230727866]]
fn find_line_intersection(
    a: &Hail,
    b: &Hail,
    c: &Hail,
) -> Option<(Point, Point, IBig, IBig, IBig)> {
    let Point {
        x: x1,
        y: y1,
        z: z1,
    } = &a.point;
    let Point {
        x: vx1,
        y: vy1,
        z: vz1,
    } = &a.velocity;

    let Point {
        x: x2,
        y: y2,
        z: z2,
    } = &b.point;
    let Point {
        x: vx2,
        y: vy2,
        z: vz2,
    } = &b.velocity;

    let Point {
        x: x3,
        y: y3,
        z: z3,
    } = &c.point;
    let Point {
        x: vx3,
        y: vy3,
        z: vz3,
    } = &c.velocity;

    // Need to borrow to prevent cloning the big ints
    let x1: &IBig = &x1;
    let x2: &IBig = &x2;
    let x3: &IBig = &x3;
    let vx1: &IBig = &vx1;
    let vx2: &IBig = &vx2;
    let vx3: &IBig = &vx3;

    let y1: &IBig = &y1;
    let y2: &IBig = &y2;
    let y3: &IBig = &y3;
    let vy1: &IBig = &vy1;
    let vy2: &IBig = &vy2;
    let vy3: &IBig = &vy3;

    let z1: &IBig = &z1;
    let z2: &IBig = &z2;
    let z3: &IBig = &z3;
    let vz1: &IBig = &vz1;
    let vz2: &IBig = &vz2;
    let vz3: &IBig = &vz3;

    // Simple Combinations
    let dvx12 = &(vx1 - vx2);
    let dvx13 = &(vx1 - vx3);
    let dvx23 = &(vx2 - vx3);
    let pvx12 = &(vx1 + vx2);
    let pvx13 = &(vx1 + vx3);
    let pvx23 = &(vx2 + vx3);
    let rvx12 = &(vx1 * vx2);
    let rvx13 = &(vx1 * vx3);
    let rvx23 = &(vx2 * vx3);

    let dvy12 = &(vy1 - vy2);
    let dvy13 = &(vy1 - vy3);
    let dvy23 = &(vy2 - vy3);
    let pvy12 = &(vy1 + vy2);
    let pvy13 = &(vy1 + vy3);
    let rvy12 = &(vy1 * vy2);
    let rvy13 = &(vy1 * vy3);
    let rvy23 = &(vy2 * vy3);

    let dvz12 = &(vz1 - vz2);
    let dvz13 = &(vz1 - vz3);
    let dvz23 = &(vz2 - vz3);
    let rvz12 = &(vz1 * vz2);
    let rvz13 = &(vz1 * vz3);
    let rvz23 = &(vz2 * vz3);

    let x1s = &(x1 * x1);
    let x2s = &(x2 * x2);
    let x3s = &(x3 * x3);
    let px12 = &(x1 * x2);
    let px13 = &(x1 * x3);
    let px23 = &(x2 * x3);

    let vx1s = &(vx1 * vx1);
    let vx2s = &(vx2 * vx2);
    let vx3s = &(vx3 * vx3);
    let m2vx1 = &(2 * vx1);
    let m2vx2 = &(2 * vx2);
    let m2vx3 = &(2 * vx3);
    let m3vx1 = &(3 * vx1);
    let m3vx2 = &(3 * vx2);
    let m3vx3 = &(3 * vx3);

    let y1s = &(y1 * y1);
    let y2s = &(y2 * y2);
    let y3s = &(y3 * y3);
    let py12 = &(y1 * y2);

    let vy1s = &(vy1 * vy1);
    let vy2s = &(vy2 * vy2);
    let vy3s = &(vy3 * vy3);
    let m2vy1 = &(2 * vy1);
    let m2vy2 = &(2 * vy2);
    let m2vy3 = &(2 * vy3);
    let m3vy1 = &(3 * vy1);
    let m3vy2 = &(3 * vy2);
    let m3vy3 = &(3 * vy3);

    let z1s = &(z1 * z1);
    let z2s = &(z2 * z2);
    let z3s = &(z3 * z3);

    let vz1s = &(vz1 * vz1);
    let vz2s = &(vz2 * vz2);
    let vz3s = &(vz3 * vz3);

    let vxy11 = &(vx1 * vy1);
    let vxy21 = &(vx2 * vy1);
    let vxy31 = &(vx3 * vy1);
    let vxy12 = &(vx1 * vy2);
    let vxy22 = &(vx2 * vy2);
    let vxy32 = &(vx3 * vy2);
    let vxy13 = &(vx1 * vy3);
    let vxy23 = &(vx2 * vy3);
    let vxy33 = &(vx3 * vy3);

    let m2rvy12 = &(3 * rvy12);

    let m2vx1s = &(2 * vx1s);
    let m2vx2s = &(2 * vx2s);
    let m2vx3s = &(2 * vx3s);
    let m2vy1s = &(2 * vy1s);

    let m3rvx12 = &(3 * rvx12);

    // 2nd order Combinations
    let m2vx1_d_dvx23 = &(m2vx1 - vx2 - vx3);
    let m2vy1_d_dvy23 = &(m2vy1 - vy2 - vy3);
    let m2vx1_r_vx2 = &(m2vx1 * vx2);

    let pvx13_d_m2vx2 = &(pvx13 - m2vx2);
    let pvx12_d_m2vx3 = &(pvx12 - m2vx3);
    let pvy13_d_m2vy2 = &(pvy13 - m2vy2);

    let vx1s_d_vx2s = &(vx1s - vx2s);

    let m2vx1_r_vx3 = &(m2vx1 * vx3);
    let m2vx2_r_vx3 = &(m2vx2 * vx3);

    let pvx12_r_vx3 = &(pvx12 * vx3);

    let dvx12_r_vx3 = &(dvx12 * vx3);
    let dvx12_r_vy1 = &(dvx12 * vy1);
    let dvx12_r_vy2 = &(dvx12 * vy2);
    let dvx12_r_vy3 = &(dvx12 * vy3);
    let dvx12_r_vz1 = &(dvx12 * vz1);
    let dvx12_r_vz2 = &(dvx12 * vz2);
    let dvx12_r_vz3 = &(dvx12 * vz3);
    let dvx12_r_vx3s = &(dvx12 * vx3s);
    let dvx12_r_vy3s = &(dvx12 * vy3s);
    let dvx12_r_vz3s = &(dvx12 * vz3s);

    let dvx13_r_vy1 = &(dvx13 * vy1);
    let dvx13_r_vy2 = &(dvx13 * vy2);
    let dvx13_r_vy3 = &(dvx13 * vy3);
    let dvx13_r_vz1 = &(dvx13 * vz1);
    let dvx13_r_vz2 = &(dvx13 * vz2);
    let dvx13_r_vy2s = &(dvx13 * vy2s);
    let dvx13_r_vz2s = &(dvx13 * vz2s);

    let dvx23_r_vy1 = &(dvx23 * vy1);
    let dvx23_r_vy2 = &(dvx23 * vy2);
    let dvx23_r_vy3 = &(dvx23 * vy3);
    let dvx23_r_vz1 = &(dvx23 * vz1);
    let dvx23_r_vz2 = &(dvx23 * vz2);
    let dvx23_r_vy1s = &(dvx23 * vy1s);
    let dvx23_r_vz1s = &(dvx23 * vz1s);

    let dvy12_r_vy3 = &(dvy12 * vy3);
    let dvy12_r_vz1 = &(dvy12 * vz1);
    let dvy12_r_vz2 = &(dvy12 * vz2);
    let dvy12_r_vz3 = &(dvy12 * vz3);
    let dvy12_r_vy3s = &(dvy12 * vy3s);
    let dvy12_r_vz3s = &(dvy12 * vz3s);

    let dvy13_r_vz1 = &(dvy13 * vz1);
    let dvy13_r_vz2 = &(dvy13 * vz2);
    let dvy13_r_vz2s = &(dvy13 * vz2s);

    let dvy23_r_vz1 = &(dvy23 * vz1);
    let dvy23_r_vz2 = &(dvy23 * vz2);
    let dvy23_r_vz1s = &(dvy23 * vz1s);

    let pvy12_r_vy3 = &(pvy12 * vy3);

    let vx1_r_vx2s = &(vx1 * vx2s);
    let vx1_r_vx3s = &(vx1 * vx3s);
    let vx1_r_vy2s = &(vx1 * vy2s);
    let vx1_r_vy3s = &(vx1 * vy3s);

    let vx2_r_vx3s = &(vx2 * vx3s);
    let vx2_r_vy1s = &(vx2 * vy1s);
    let vx2_r_vy3s = &(vx2 * vy3s);

    let vx3_r_vy1s = &(vx3 * vy1s);
    let vx3_r_vy2s = &(vx3 * vy2s);

    let vy1_r_vy2s = &(vy1 * vy2s);
    let vy1_r_vy3s = &(vy1 * vy3s);

    let vy2_r_vy3s = &(vy2 * vy3s);

    let vz1_r_vz2s = &(vz1 * vz2s);

    let vx1s_r_vx2 = &(vx1s * vx2);
    let vx1s_r_vx3 = &(vx1s * vx3);

    let vx2s_r_vx3 = &(vx2s * vx3);

    let vy1s_r_vy2 = &(vy1s * vy2);
    let vy1s_r_vy3 = &(vy1s * vy3);

    let vy2s_r_vy3 = &(vy2s * vy3);

    let vz1s_r_vz2 = &(vz1s * vz2);

    let dvx13_r_rvz12 = &(dvx13 * rvz12);
    let dvx23_r_rvz12 = &(dvx23 * rvz12);
    let dvx12_r_rvz12 = &(dvx12 * rvz12);
    let dvx13_r_rvy12 = &(dvx13 * rvy12);
    let dvx12_r_rvy12 = &(dvx12 * rvy12);
    let dvx23_r_rvy12 = &(dvx23 * rvy12);
    let dvy13_r_rvz12 = &(dvy13 * rvz12);
    let dvy23_r_rvz12 = &(dvy23 * rvz12);
    let dvy12_r_rvz12 = &(dvy12 * rvz12);

    let m2vy1_r_vy2 = &(m2vy1 * vy2);
    let m2vy1_r_vy3 = &(m2vy1 * vy3);
    let m2vy2_r_vy3 = &(m2vy2 * vy3);

    let vy1s_d_vy2s = &(vy1s - vy2s);

    let m2vx1_d_vx2 = &(m2vx1 - vx2);

    // 3rd Order Combinations
    let h1 = &(vy1 + vy2 - m2vy3);
    let h2 = &(pvx12_d_m2vx3 * vy1);
    let h3 = &(pvx12_d_m2vx3 * vy2);
    let h4 = &(2 * dvx13_r_vy2);
    let h5 = &(m2vx1_d_dvx23 * vy1);
    let h6 = &(2 * dvx12_r_vy3);
    let h7 = &(pvx13_d_m2vx2 * vy1);
    let h8 = &(pvx13_d_m2vx2 * rvz12);
    let h9 = &(m2vx1_d_dvx23 * rvy12);
    let ha = &(pvx13_d_m2vx2 * rvy12);
    let hb = &(pvx13_d_m2vx2 * vy2);
    let hc = &(vx1s_d_vx2s * vx3);
    let hd = &(pvx12_d_m2vx3 * rvy12);
    let he = &(pvx12_d_m2vx3 * vy3);
    let hf = &(dvx23_r_vy1 * vy2);
    let hg = &(m2vx1_d_dvx23 * vy2);
    let hh = &(2 * dvx23_r_vy1);
    let hi = &(pvx13_d_m2vx2 * vy3);
    let hj = &(pvx12_d_m2vx3 * rvz12);
    let hk = &(m2vy1_d_dvy23 * rvz12);
    let hl = &(m2vx1_d_dvx23 * vy3);
    let hm = &(2 * dvx12_r_vx3);
    let hn = &(pvy13_d_m2vy2 * rvz12);
    let ho = &(m2vx1_d_vx2 * vx3);
    let hp = &(2 * dvx12_r_vy1);

    let i1 = &(vx1s - rvx12 - dvx12_r_vx3);
    let i2 = &(rvx12 - pvx12_r_vx3 + vx3s);
    let i3 = &(rvx12 - vx2s - dvx12_r_vx3);
    let i4 = &(rvy12 - vy2s - dvy12_r_vy3);
    let i5 = &(rvy12 - pvy12_r_vy3 + vy3s);
    let i6 = &(vy1s - rvy12 - dvy12_r_vy3);
    let i7 = &(vx1s - m2vx1_r_vx3 + vx3s);
    let i9 = &(vx2s - m2vx2_r_vx3 + vx3s);
    let ia = &(m2vx1_d_dvx23 * rvz12);
    let ib = &(vx1s - m2vx1_r_vx2 + vx2s);
    let ic = &(vy1s - m2vy1_r_vy3 + vy3s);
    let id = &(m3vx1 - vx2 - m2vx3);
    let ie = &(m2vx1 - m3vx2 + vx3);
    let ig = &(m2vx1 + vx2 - m3vx3);
    let ih = &(vy1s - m2vy1_r_vy2 + vy2s);
    let ii = &(vy2s - m2vy2_r_vy3 + vy3s);
    let ij = &(vx1 - m3vx2 + m2vx3);
    let ik = &(vx1 + m2vx2 - m3vx3);
    let il = &(m3vx1 - m2vx2 - vx3);

    let j1 = &(dvy23_r_vz1 - m2vy1_d_dvy23 * vz2);
    let j2 = &(dvy12_r_vz1 - dvy12_r_vz2);
    let j3 = &(pvy13_d_m2vy2 * vz1 + dvy13_r_vz2);
    let j4 = &(vx1s - m2vx1_r_vx2 + m2vx2_r_vx3 - vx3s);
    let j5 = &(dvx12_r_vy1 - dvx12_r_vy2);
    let j6 = &(dvx23_r_vy1 + dvx12_r_vy2 - dvx13_r_vy3);
    let j7 = &(m2vx1_r_vx2 - vx2s - m2vx1_r_vx3 + vx3s);
    let j8 = &(dvx13_r_vz1 - dvx13_r_vz2);
    let j9 = &(m2vx1_d_dvx23 * vz1 - dvx23_r_vz2);
    let ja = &(dvx13_r_vz1 + pvx13_d_m2vx2 * vz2);
    let jb = &(dvx23_r_vz1s - dvx23_r_rvz12 - (dvx23_r_vz1 - dvx23_r_vz2) * vz3);
    let jc = &(dvx12_r_vz1 + dvx12_r_vz2);
    let jd = &((pvx12_d_m2vx3 * vz1 - pvx12_d_m2vx3 * vz2) * vz3);
    let je = &(dvx13_r_vy1 - dvx13_r_vy2);
    let jf = &(dvx12_r_vy1 + dvx12_r_vy2);
    let jg = &(dvx13_r_vy1 - dvx13_r_vy3);
    let jh = &((pvx13_d_m2vx2 * vz1 + dvx13_r_vz2) * vz3);
    let ji = &(dvx12_r_vz1 - dvx12_r_vz2);
    let jj = &(dvy13_r_vz1 - (m3vy1 - m2vy2 - vy3) * vz2);
    let jk = &((dvx23_r_vz1 - m2vx1_d_dvx23 * vz2) * vz3);
    let jl = &(h5 - dvx13_r_vy2 - dvx12_r_vy3);
    let jm = &(vy1s_d_vy2s * vy3);
    let jn = &(((vy1 - m3vy2 + m2vy3) * vz1 + (m3vy1 - vy2 - m2vy3) * vz2) * vz3);
    let jo = &((m2vy1_d_dvy23 * vz1 - dvy23_r_vz2) * vz3);
    let jp = &((dvy13_r_vz1 + pvy13_d_m2vy2 * vz2) * vz3);
    let jq = &(vz1s_r_vz2 - vz1_r_vz2s + dvz12 * vz3s - (vz1s - vz2s) * vz3);
    let jr = &((dvx13_r_vy1 - dvx23_r_vy2 - dvx12_r_vy3) * vz3);
    let js = &(rvx12 - m2vx2s - (vx1 - m3vx2) * vx3 - vx3s);
    let jt = &(m2vx1s - rvx12 - (m3vx1 - vx2) * vx3 + vx3s);
    let ju = &(vx1s - m3rvx12 + m2vx2s + dvx12_r_vx3);
    let jv = &(vx1s_r_vx2 + vx1_r_vx3s - (vx1s + rvx12) * vx3);
    let jw = &((vy1s - vy2s - 2 * dvy12_r_vy3) * vz3);
    let jx = &((vy1s - m2vy1_r_vy2 + m2vy2_r_vy3 - vy3s) * vz2);
    let jy = &((m2vy1_r_vy2 - vy2s - m2vy1_r_vy3 + vy3s) * vz1);

    // 4th Order Combinations
    let k1 = &(i2 * vy1);
    let k2 = &(i1 * vy2);
    let k3 = &(i3 * vy1);
    let k4 = &(i1 * vy3);
    let k5 = &(i2 * vy2);
    let k6 = &(i3 * vy3);
    let k7 = &(j7 * vy1);
    let k8 = &(j4 * vy2);
    let k9 = &(i1 * vz2);
    let ka = &(i1 * vz3);
    let kb = &(i2 * vz2);
    let kc = &(i2 * vz1);
    let kd = &(i3 * vz3);
    let ke = &(i3 * vz1);
    let kf = &(j5 * vy3);
    let kg = &(j5 * vz3);
    let kh = &(j5 * vz3s);

    let l1 = &(vx1s - vx2s - hm);

    let m1 = &((dvx13_r_vy1 - h4 + dvx13_r_vy3) * vz2);
    let m2 = &((h5 - h4 + dvx23_r_vy3) * vz2);

    let x_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let y_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let z_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let a_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let b_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let c_denominator = ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2
        - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
        * y1s
        + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y2s
        + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2)
            * y3s
        + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
            - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
            - (i9 * vy1 - k5 + k6) * y2
            + (i9 * vy1 - k5 + k6) * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
            - (k1 - i7 * vy2 + k4) * y1
            + (k1 - i7 * vy2 + k4) * y3)
            * z2s
        + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
            - (k3 - k2 + ib * vy3) * y1
            + (k3 - k2 + ib * vy3) * y2)
            * z3s
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * px12
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1
                - (2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s
                    - dvy12_r_vz3s
                    + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * px12
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x3s
            + ((dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                + 2 * dvy13_r_vz2s
                + dvy12_r_vz3s
                + jj * vz3)
                * x1
                + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                    + (ij * vz1 + id * vz2) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x1s
            - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
            + (dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x2s
            + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1
                - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x2)
                * x3
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                + (dvx23_r_vz1s - ik * rvz12
                    + 2 * dvx13_r_vz2s
                    + dvx12_r_vz3s
                    + (dvx13_r_vz1 - il * vz2) * vz3)
                    * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1
            + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                + (ie * vz1 + dvx23_r_vz2) * vz3)
                * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y2)
            * y3
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * px12
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            - (kc - i7 * vz2 + ka) * y2s
            - (ke - k9 + ib * vz3) * y3s
            - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1
                - (jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                    + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                    * x2)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                - (dvx23_r_vy1 - hg + h6) * vz3)
                * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2
                    - (dvx23_r_vy1 - hg + h6) * vz3)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                + (j6 * vz1 - (hp + dvx13_r_vy2 - il * vy3) * vz2
                    + (ie * vy1 + dvx23_r_vy2 - h6) * vz3)
                    * x3
                - (i9 * vz1 - kb + kd) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (hh - dvx13_r_vy2 + hi) * vz2
                + (hh - h3 + dvx12_r_vy3) * vz3)
                * x1
                - (j6 * vz1 - (ig * vy1 - h4 - dvx23_r_vy3) * vz2
                    + (2 * dvx13_r_vy1 - id * vy2 + dvx12_r_vy3) * vz3)
                    * x2
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (i9 * vz1 - kb + kd) * y1
                + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y2)
                * y3)
            * z1
        - ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - (i5 * vz1 - ic * vz2 + i6 * vz3) * px12
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x3s
            + (i9 * vz1 - kb + kd) * y1s
            - (ke - k9 + ib * vz3) * y3s
            + (((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                * x1
                + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 - 2 * dvx12_r_vy2 + ie * vy3) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (dvx13_r_vy1 - il * vy2 + h6) * vz3)
                    * x3)
                * y1
            + (((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x1
                - ((hh - dvx13_r_vy2 + hi) * vz1 - jg * vz2 + (h7 + dvx13_r_vy2 - h6) * vz3) * x3
                - (kc - i7 * vz2 + ka) * y1)
                * y2
            - (((hh - ik * vy2 + dvx13_r_vy3) * vz1
                - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                + (ij * vy1 + 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * vz1 + jg * vz2 - (h2 - h4 + dvx12_r_vy3) * vz3) * x2
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2 + 2 * kg) * x3
                - (js * vz1 - j4 * vz2 + ju * vz3) * y1
                - (kc - i7 * vz2 + ka) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                    + (ij * vy1 + id * vy2) * vy3)
                    * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (k1 - i7 * vy2 + k4) * y2
                + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                    + l1 * vy3)
                    * y3)
                * z1)
            * z2
        + ((ii * vz1 - i5 * vz2 + i4 * vz3) * x1s
            - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                + jw)
                * px12
            + (i5 * vz1 - ic * vz2 + i6 * vz3) * x2s
            + (i9 * vz1 - kb + kd) * y1s
            + (kc - i7 * vz2 + ka) * y2s
            + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 - (i4 * vz1 - i6 * vz2 + ih * vz3) * x2) * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                + ((dvx23_r_vy1 - ig * vy2 + 2 * dvx13_r_vy3) * vz1
                    + (dvx12_r_vy1 + h4 - id * vy3) * vz2
                    - jr)
                    * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3)
                * y1
            + (((hh - dvx12_r_vy2 + ij * vy3) * vz1
                - (ik * vy1 - dvx13_r_vy2 - 2 * dvx23_r_vy3) * vz2
                + jr)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - 2 * jg * vz2 + jl * vz3) * x2
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - kg) * x3
                - ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                    - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                    + l1 * vz3)
                    * y1)
                * y2
            - (((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x1
                - ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + kg) * x2
                - (ke - k9 + ib * vz3) * y1
                + (ke - k9 + ib * vz3) * y2)
                * y3
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                + (dvx23_r_vy1s - ik * rvy12
                    + 2 * dvx13_r_vy2s
                    + dvx12_r_vy3s
                    + (dvx13_r_vy1 - il * vy2) * vy3)
                    * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (i9 * vy1 - k5 + k6) * y1
                - (js * vy1 - k8 + ju * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1
            - ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                + (ie * vy1 + dvx23_r_vy2) * vy3)
                * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                + (k1 - i7 * vy2 + k4) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z2)
            * z3;

    let t_denominator = (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x2
        - (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x3
        - (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y2
        + (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y3
        + (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z2
        - (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z3;

    let u_denominator = (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x1
        - (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x3
        - (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y1
        + (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y3
        + (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z1
        - (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z3;

    let v_denominator = (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x1
        - (dvy23_r_vz1 - dvy13_r_vz2 + dvy12_r_vz3) * x2
        - (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y1
        + (dvx23_r_vz1 - dvx13_r_vz2 + dvx12_r_vz3) * y2
        + (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z1
        - (dvx23_r_vy1 - dvx13_r_vy2 + dvx12_r_vy3) * z2;

    if x_denominator == IBig::ZERO
        || y_denominator == IBig::ZERO
        || z_denominator == IBig::ZERO
        || a_denominator == IBig::ZERO
        || b_denominator == IBig::ZERO
        || c_denominator == IBig::ZERO
        || t_denominator == IBig::ZERO
        || u_denominator == IBig::ZERO
        || v_denominator == IBig::ZERO
    {
        return None;
    }

    let x_numerator = ((dvx13_r_rvz12 - dvx13_r_vz2s - j8 * vz3) * x2s
        - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * px23
        + (dvx12_r_rvz12 + dvx12_r_vz3s - jc * vz3) * x3s)
        * y1s
        + (jb * x1s - (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * px13
            + (dvx12_r_rvz12 + dvx12_r_vz3s - jc * vz3) * x3s)
            * y2s
        + (jb * x1s - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * px12
            + (dvx13_r_rvz12 - dvx13_r_vz2s - j8 * vz3) * x2s)
            * y3s
        + ((dvx13_r_rvy12 - dvx13_r_vy2s - je * vy3) * x2s
            - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * px23
            + (dvx12_r_rvy12 + dvx12_r_vy3s - jf * vy3) * x3s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y3s
            - ((k1 + k8 - k4) * x2 - (k3 + k2 - l1 * vy3) * x3) * y2
            + ((k1 + k8 - k4) * x2
                - (k3 + k2 - l1 * vy3) * x3
                - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2)
                * y3)
            * z1s
        + ((dvx23_r_vy1s - dvx23_r_rvy12 - (dvx23_r_vy1 - dvx23_r_vy2) * vy3) * x1s
            - (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * px13
            + (dvx12_r_rvy12 + dvx12_r_vy3s - jf * vy3) * x3s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y3s
            - ((k7 - k5 - k6) * x1 - (k3 + k2 - l1 * vy3) * x3) * y1
            + ((k7 - k5 - k6) * x1
                - (k3 + k2 - l1 * vy3) * x3
                - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1)
                * y3)
            * z2s
        + ((dvx23_r_vy1s - dvx23_r_rvy12 - (dvx23_r_vy1 - dvx23_r_vy2) * vy3) * x1s
            - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * px12
            + (dvx13_r_rvy12 - dvx13_r_vy2s - je * vy3) * x2s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1s
            + (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2s
            - ((k7 - k5 - k6) * x1 - (k1 + k8 - k4) * x2) * y1
            + ((k7 - k5 - k6) * x1
                - (k1 + k8 - k4) * x2
                - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1)
                * y2)
            * z3s
        + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1 * x2s
            + ((dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x1
                - (dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x2)
                * x3s
            - ((2 * dvy23_r_vz1s - (m2vy1 + vy2 - m3vy3) * rvz12 + dvy13_r_vz2s - dvy12_r_vz3s
                + ((m2vy1 - m3vy2 + vy3) * vz1 + dvy23_r_vz2) * vz3)
                * px12
                - (dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x2s)
                * x3)
            * y1
        - ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1s * x2
            + ((dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x1
                - (dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x2)
                * x3s
            - ((dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x1s
                + (dvy23_r_vz1s - (vy1 + m2vy2 - m3vy3) * rvz12
                    + 2 * dvy13_r_vz2s
                    + dvy12_r_vz3s
                    + jj * vz3)
                    * px12)
                * x3
            + ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * px12
                + 2 * (dvx12_r_rvz12 + dvx12_r_vz3s - jc * vz3) * x3s
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    + (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2)
                    * x3)
                * y1)
            * y2
        + ((dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1s * x2
            - (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s + j2 * vz3) * x1 * x2s
            - ((dvy23_r_vz1s - dvy13_r_rvz12 - dvy12_r_vz3s + j3 * vz3) * x1s
                - (dvy23_r_vz1s - dvy12_r_rvz12 - dvy13_r_vz2s - 2 * dvy12_r_vz3s + jn) * px12
                + (dvy23_r_rvz12 - dvy13_r_vz2s - dvy12_r_vz3s - j1 * vz3) * x2s)
                * x3
            + ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * px12
                - 2 * (dvx13_r_rvz12 - dvx13_r_vz2s - j8 * vz3) * x2s
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2)
                    * x3)
                * y1
            - (2 * jb * x1s
                - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * px12
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2)
                    * x3)
                * y2)
            * y3
        - ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1 * x2s
            + ((i5 * vz1 - ic * vz2 + i6 * vz3) * x1 - (ii * vz1 - i5 * vz2 + i4 * vz3) * x2)
                * x3s
            + ((j7 * vz1 - kb - kd) * x1 - (ke + k9 - l1 * vz3) * x3) * y2s
            + ((j7 * vz1 - kb - kd) * x1 - (kc + j4 * vz2 - ka) * x2) * y3s
            - ((jy - (m2vy1s - rvy12 - (m3vy1 - vy2) * vy3 + vy3s) * vz2
                + (m2vy1s - m2rvy12 + vy2s - dvy12_r_vy3) * vz3)
                * px12
                - (ii * vz1 - i5 * vz2 + i4 * vz3) * x2s)
                * x3
            + (((dvx13_r_vy2 - dvx13_r_vy3) * vz1 + m1 - je * vz3) * x2s
                - ((hg - hl) * vz1 + m2 - (h5 - dvx23_r_vy2 - h6) * vz3) * px23
                + ((dvx12_r_vy2 - dvx12_r_vy3) * vz1 + (dvx12_r_vy1 - dvx12_r_vy3) * vz2
                    - (dvx12_r_vy1 + dvx12_r_vy2 - h6) * vz3)
                    * x3s)
                * y1
            - (((dvx23_r_vy1 + 2 * dvx12_r_vy2 - hl) * vz1
                - (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                + (h7 - hb) * vz3)
                * px12
                + ((dvx12_r_vy2 - dvx12_r_vy3) * vz1 + (dvx12_r_vy1 - dvx12_r_vy3) * vz2
                    - (dvx12_r_vy1 + dvx12_r_vy2 - h6) * vz3)
                    * x3s
                - (((dvx23_r_vy1 + hg - 2 * dvx13_r_vy3) * vz1 - (h2 - he) * vz2
                    + (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                    * x1
                    + ((hb - hi) * vz1 + (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                        - (h2 - 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3
                + ((kc + j4 * vz2 - ka) * x2 - (ke + k9 - l1 * vz3) * x3) * y1)
                * y2
            + (((dvx23_r_vy1 + 2 * dvx12_r_vy2 - hl) * vz1
                - (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                + (h7 - hb) * vz3)
                * px12
                - ((dvx13_r_vy2 - dvx13_r_vy3) * vz1 + m1 - je * vz3) * x2s
                - (((dvx23_r_vy1 + hg - 2 * dvx13_r_vy3) * vz1 - (h2 - he) * vz2
                    + (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                    * x1
                    - ((h3 - he) * vz1 + (h7 - dvx13_r_vy2 + 2 * dvx23_r_vy3) * vz2
                        - (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3
                + ((kc + j4 * vz2 - ka) * x2 - (ke + k9 - l1 * vz3) * x3) * y1
                - (2 * (j7 * vz1 - kb - kd) * x1
                    - (kc + j4 * vz2 - ka) * x2
                    - (ke + k9 - l1 * vz3) * x3)
                    * y2)
                * y3)
            * z1
        + ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1s * x2
            + ((i5 * vz1 - ic * vz2 + i6 * vz3) * x1 - (ii * vz1 - i5 * vz2 + i4 * vz3) * x2)
                * x3s
            - ((kc + j4 * vz2 - ka) * x2 - (ke + k9 - l1 * vz3) * x3) * y1s
            + ((j7 * vz1 - kb - kd) * x1 - (kc + j4 * vz2 - ka) * x2) * y3s
            - ((i5 * vz1 - ic * vz2 + i6 * vz3) * x1s
                + ((rvy12 - 2 * vy2s - (vy1 - m3vy2) * vy3 - vy3s) * vz1 - jx
                    + (vy1s - m2rvy12 + 2 * vy2s + dvy12_r_vy3) * vz3)
                    * px12)
                * x3
            + (((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1 + (hp - dvx13_r_vy2 - hi) * vz2
                - (h5 - hg) * vz3)
                * px12
                + ((dvx12_r_vy2 - dvx12_r_vy3) * vz1 + (dvx12_r_vy1 - dvx12_r_vy3) * vz2
                    - (dvx12_r_vy1 + dvx12_r_vy2 - h6) * vz3)
                    * x3s
                - (((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 + (h5 - hl) * vz2
                    - (2 * dvx13_r_vy1 - h3 - dvx12_r_vy3) * vz3)
                    * x1
                    + ((h3 - he) * vz1 + (h7 - dvx13_r_vy2 + 2 * dvx23_r_vy3) * vz2
                        - (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3)
                * y1
            - (((hh - dvx23_r_vy2 - dvx23_r_vy3) * vz1
                - (dvx23_r_vy1 - dvx23_r_vy3) * vz2
                - (dvx23_r_vy1 - dvx23_r_vy2) * vz3)
                * x1s
                - ((hh + hb - dvx13_r_vy3) * vz1 + (h7 - hi) * vz2
                    - (dvx13_r_vy1 + hb - h6) * vz3)
                    * px13
                + ((dvx12_r_vy2 - dvx12_r_vy3) * vz1 + (dvx12_r_vy1 - dvx12_r_vy3) * vz2
                    - (dvx12_r_vy1 + dvx12_r_vy2 - h6) * vz3)
                    * x3s
                - ((j7 * vz1 - kb - kd) * x1 - (ke + k9 - l1 * vz3) * x3) * y1)
                * y2
            + (((hh - dvx23_r_vy2 - dvx23_r_vy3) * vz1
                - (dvx23_r_vy1 - dvx23_r_vy3) * vz2
                - (dvx23_r_vy1 - dvx23_r_vy2) * vz3)
                * x1s
                - ((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1 + (hp - dvx13_r_vy2 - hi) * vz2
                    - (h5 - hg) * vz3)
                    * px12
                - (((dvx23_r_vy1 + hg - 2 * dvx13_r_vy3) * vz1 - (h2 - he) * vz2
                    + (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                    * x1
                    - ((h3 - he) * vz1 + (h7 - dvx13_r_vy2 + 2 * dvx23_r_vy3) * vz2
                        - (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3
                - ((j7 * vz1 - kb - kd) * x1 - 2 * (kc + j4 * vz2 - ka) * x2
                    + (ke + k9 - l1 * vz3) * x3)
                    * y1
                - ((j7 * vz1 - kb - kd) * x1 - (ke + k9 - l1 * vz3) * x3) * y2)
                * y3
            - ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * px12
                + 2 * (dvx12_r_rvy12 + dvx12_r_vy3s - jf * vy3) * x3s
                + 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y3s
                - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    + (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2)
                    * x3
                - ((k1 + k8 - k4) * x2 - (k3 + k2 - l1 * vy3) * x3) * y1
                - ((k7 - k5 - k6) * x1
                    - (k3 + k2 - l1 * vy3) * x3
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1)
                    * y2
                + ((k7 - k5 - k6) * x1 + (k1 + k8 - k4) * x2
                    - 2 * (k3 + k2 - l1 * vy3) * x3
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2)
                    * y3)
                * z1)
            * z2
        - ((i4 * vz1 - i6 * vz2 + ih * vz3) * x1s * x2
            - (i4 * vz1 - i6 * vz2 + ih * vz3) * x1 * x2s
            - ((kc + j4 * vz2 - ka) * x2 - (ke + k9 - l1 * vz3) * x3) * y1s
            - ((j7 * vz1 - kb - kd) * x1 - (ke + k9 - l1 * vz3) * x3) * y2s
            - ((i5 * vz1 - ic * vz2 + i6 * vz3) * x1s
                - ((rvy12 + vy2s - (vy1 + m3vy2) * vy3 + 2 * vy3s) * vz1
                    - (vy1s + rvy12 - (m3vy1 + vy2) * vy3 + 2 * vy3s) * vz2
                    + jw)
                    * px12
                + (ii * vz1 - i5 * vz2 + i4 * vz3) * x2s)
                * x3
            + (((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1 + (hp - dvx13_r_vy2 - hi) * vz2
                - (h5 - hg) * vz3)
                * px12
                - ((dvx13_r_vy2 - dvx13_r_vy3) * vz1 + m1 - je * vz3) * x2s
                - (((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 + (h5 - hl) * vz2
                    - (2 * dvx13_r_vy1 - h3 - dvx12_r_vy3) * vz3)
                    * x1
                    - ((hb - hi) * vz1 + (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                        - (h2 - 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3)
                * y1
            - (((hh - dvx23_r_vy2 - dvx23_r_vy3) * vz1
                - (dvx23_r_vy1 - dvx23_r_vy3) * vz2
                - (dvx23_r_vy1 - dvx23_r_vy2) * vz3)
                * x1s
                - ((dvx23_r_vy1 + 2 * dvx12_r_vy2 - hl) * vz1
                    - (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                    + (h7 - hb) * vz3)
                    * px12
                - (((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 + (h5 - hl) * vz2
                    - (2 * dvx13_r_vy1 - h3 - dvx12_r_vy3) * vz3)
                    * x1
                    - ((hb - hi) * vz1 + (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                        - (h2 - 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2)
                    * x3
                - ((j7 * vz1 - kb - kd) * x1 + (kc + j4 * vz2 - ka) * x2
                    - 2 * (ke + k9 - l1 * vz3) * x3)
                    * y1)
                * y2
            + (((hh - dvx23_r_vy2 - dvx23_r_vy3) * vz1
                - (dvx23_r_vy1 - dvx23_r_vy3) * vz2
                - (dvx23_r_vy1 - dvx23_r_vy2) * vz3)
                * x1s
                - ((hh + dvx12_r_vy2 - he) * vz1 + (dvx12_r_vy1 - h4 + he) * vz2
                    - (h2 - h3) * vz3)
                    * px12
                + ((dvx13_r_vy2 - dvx13_r_vy3) * vz1 + m1 - je * vz3) * x2s
                - ((j7 * vz1 - kb - kd) * x1 - (kc + j4 * vz2 - ka) * x2) * y1
                + ((j7 * vz1 - kb - kd) * x1 - (kc + j4 * vz2 - ka) * x2) * y2)
                * y3
            - ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * px12
                - 2 * (dvx13_r_rvy12 - dvx13_r_vy2s - je * vy3) * x2s
                - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2s
                - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2)
                    * x3
                - ((k1 + k8 - k4) * x2 - (k3 + k2 - l1 * vy3) * x3) * y1
                - ((k7 - k5 - k6) * x1 - 2 * (k1 + k8 - k4) * x2 + (k3 + k2 - l1 * vy3) * x3
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1)
                    * y2
                + ((k7 - k5 - k6) * x1
                    - (k1 + k8 - k4) * x2
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1
                    + 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2)
                    * y3)
                * z1
            + (2 * (dvx23_r_vy1s - dvx23_r_rvy12 - (dvx23_r_vy1 - dvx23_r_vy2) * vy3) * x1s
                - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * px12
                + 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1s
                - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2)
                    * x3
                - (2 * (k7 - k5 - k6) * x1 - (k1 + k8 - k4) * x2 - (k3 + k2 - l1 * vy3) * x3)
                    * y1
                + ((k7 - k5 - k6) * x1
                    - (k3 + k2 - l1 * vy3) * x3
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1)
                    * y2
                + ((k7 - k5 - k6) * x1
                    - (k1 + k8 - k4) * x2
                    - 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y1
                    + 2 * (vx1s_r_vx2 - vx1_r_vx2s + dvx12_r_vx3s - hc) * y2)
                    * y3)
                * z2)
            * z3;

    let x = x_numerator / x_denominator;

    let y_numerator = ((dvy23_r_vz1s - dvy23_r_rvz12 - (dvy23_r_vz1 - dvy23_r_vz2) * vz3) * x2s
        - 2 * (dvy23_r_vz1s - dvy23_r_rvz12 - (dvy23_r_vz1 - dvy23_r_vz2) * vz3) * px23
        + (dvy23_r_vz1s - dvy23_r_rvz12 - (dvy23_r_vz1 - dvy23_r_vz2) * vz3) * x3s)
        * y1s
        + ((dvy13_r_rvz12 - dvy13_r_vz2s - (dvy13_r_vz1 - dvy13_r_vz2) * vz3) * x1s
            - 2 * (dvy13_r_rvz12 - dvy13_r_vz2s - (dvy13_r_vz1 - dvy13_r_vz2) * vz3) * px13
            + (dvy13_r_rvz12 - dvy13_r_vz2s - (dvy13_r_vz1 - dvy13_r_vz2) * vz3) * x3s
            + ((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x1
                - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
                * y1)
            * y2s
        + ((dvy12_r_rvz12 + dvy12_r_vz3s - (dvy12_r_vz1 + dvy12_r_vz2) * vz3) * x1s
            - 2 * (dvy12_r_rvz12 + dvy12_r_vz3s - (dvy12_r_vz1 + dvy12_r_vz2) * vz3) * px12
            + (dvy12_r_rvz12 + dvy12_r_vz3s - (dvy12_r_vz1 + dvy12_r_vz2) * vz3) * x2s
            + ((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x1
                - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2)
                * y1
            - ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x2)
                * y2)
            * y3s
        + ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2s
            - 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px23
            + (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x3s
            + (k3 - k6) * y2s
            + (k1 - k5) * y3s
            - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x2
                - (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x3)
                * y2
            + ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x2
                - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x3
                - (k7 - k5 - k6) * y2)
                * y3)
            * z1s
        + ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1s
            - 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px13
            + (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x3s
            + (k2 - k4) * y1s
            + (k1 - k5) * y3s
            - ((h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x1
                - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x3)
                * y1
            + ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x1
                - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x3
                - (k1 + k8 - k4) * y1)
                * y3)
            * z2s
        + ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1s
            - 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px12
            + (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2s
            + (k2 - k4) * y1s
            + (k3 - k6) * y2s
            - ((h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x1
                - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2)
                * y1
            + ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                - (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x2
                - (k3 + k2 - l1 * vy3) * y1)
                * y2)
            * z3s
        - (((dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x2
            - (dvx23_r_vz1s - hj + dvx13_r_vz2s + ji * vz3) * x3)
            * y1s
            + ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3) * px12
                + (dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x3s
                - ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x1
                    + (dvy23_r_vz1s + dvy12_r_rvz12
                        - dvy13_r_vz2s
                        - (h1 * vz1 - h1 * vz2) * vz3)
                        * x2)
                    * x3)
                * y1)
            * y2
        + (((dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x2
            - (dvx23_r_vz1s - dvx13_r_rvz12 - dvx12_r_vz3s + jh) * x3)
            * y1s
            + ((dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x1
                - (dvx23_r_rvz12 - dvx13_r_vz2s - dvx12_r_vz3s - jk) * x3)
                * y2s
            + ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * px12
                - (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x2s
                - ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x1
                    - (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x2)
                    * x3)
                * y1
            - ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x1s
                - (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * px12
                - ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x1
                    - (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x2)
                    * x3
                + ((2 * dvx23_r_vz1s - ig * rvz12 + dvx13_r_vz2s - dvx12_r_vz3s
                    + (ie * vz1 + dvx23_r_vz2) * vz3)
                    * x1
                    - (dvx23_r_vz1s - ik * rvz12
                        + 2 * dvx13_r_vz2s
                        + dvx12_r_vz3s
                        + (dvx13_r_vz1 - il * vz2) * vz3)
                        * x2
                    - (dvx23_r_vz1s - dvx12_r_rvz12 - dvx13_r_vz2s - 2 * dvx12_r_vz3s
                        + (ij * vz1 + id * vz2) * vz3)
                        * x3)
                    * y1)
                * y2)
            * y3
        - ((((dvx23_r_vy1 - dvx23_r_vy3) * vz1 + (h7 - hi) * vz2
            - (dvx12_r_vy1 - dvx12_r_vy3) * vz3)
            * x1
            - ((dvx23_r_vy1 - dvx23_r_vy3) * vz1 + (h7 - hi) * vz2
                - (dvx12_r_vy1 - dvx12_r_vy3) * vz3)
                * x3
            + (ke - k9 + ib * vz3) * y1)
            * y2s
            + (((dvx23_r_vy1 - dvx23_r_vy2) * vz1 + je * vz2 - (h2 - h3) * vz3) * x1
                - ((dvx23_r_vy1 - dvx23_r_vy2) * vz1 + je * vz2 - (h2 - h3) * vz3) * x2
                + (kc - i7 * vz2 + ka) * y1
                - (i9 * vz1 - kb + kd) * y2)
                * y3s
            + ((jy - i5 * vz2 - i4 * vz3) * x2s - 2 * (jy - i5 * vz2 - i4 * vz3) * px23
                + (jy - i5 * vz2 - i4 * vz3) * x3s)
                * y1
            - ((i5 * vz1 + jx - i6 * vz3) * px12 + (i5 * vz1 + jx - i6 * vz3) * x3s
                - ((i5 * vz1 + jx - i6 * vz3) * x1 + (i5 * vz1 + jx - i6 * vz3) * x2) * x3
                + (((hh + hb - dvx13_r_vy3) * vz1 - (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                    + (dvx12_r_vy1 - 2 * dvx12_r_vy2 + dvx12_r_vy3) * vz3)
                    * x2
                    - ((hh + hb - dvx13_r_vy3) * vz1 - (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                        + (dvx12_r_vy1 - 2 * dvx12_r_vy2 + dvx12_r_vy3) * vz3)
                        * x3)
                    * y1)
                * y2
            + ((i4 * vz1 + i6 * vz2 - jw) * px12 - (i4 * vz1 + i6 * vz2 - jw) * x2s
                + (i9 * vz1 - kb + kd) * y2s
                - ((i4 * vz1 + i6 * vz2 - jw) * x1 - (i4 * vz1 + i6 * vz2 - jw) * x2) * x3
                + (((hh + dvx12_r_vy2 - he) * vz1
                    - (dvx13_r_vy1 + dvx13_r_vy2 - 2 * dvx13_r_vy3) * vz2
                    + (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                    * x2
                    - ((hh + dvx12_r_vy2 - he) * vz1
                        - (dvx13_r_vy1 + dvx13_r_vy2 - 2 * dvx13_r_vy3) * vz2
                        + (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x3)
                    * y1
                - (((hh - dvx23_r_vy2 - dvx23_r_vy3) * vz1 + (hp - dvx13_r_vy2 - hi) * vz2
                    - (2 * dvx13_r_vy1 - h3 - dvx12_r_vy3) * vz3)
                    * x1
                    - ((dvx23_r_vy1 - 2 * dvx23_r_vy2 + dvx23_r_vy3) * vz1
                        + (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                        - (dvx13_r_vy1 + hb - h6) * vz3)
                        * x2
                    - ((dvx23_r_vy1 + dvx23_r_vy2 - 2 * dvx23_r_vy3) * vz1
                        + (dvx12_r_vy1 - h4 + he) * vz2
                        - (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                        * x3
                    + (j7 * vz1 - jt * vz2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vz3) * y1)
                    * y2)
                * y3)
            * z1
        - ((((hg - hl) * vz1
            - (dvx13_r_vy2 - dvx13_r_vy3) * vz2
            - (dvx12_r_vy2 - dvx12_r_vy3) * vz3)
            * x2
            - ((hg - hl) * vz1
                - (dvx13_r_vy2 - dvx13_r_vy3) * vz2
                - (dvx12_r_vy2 - dvx12_r_vy3) * vz3)
                * x3)
            * y1s
            - (((dvx23_r_vy1 - dvx23_r_vy2) * vz1 + je * vz2 - (h2 - h3) * vz3) * x1
                - ((dvx23_r_vy1 - dvx23_r_vy2) * vz1 + je * vz2 - (h2 - h3) * vz3) * x2
                + (kc - i7 * vz2 + ka) * y1
                - (i9 * vz1 - kb + kd) * y2)
                * y3s
            - ((jy - i5 * vz2 - i4 * vz3) * px12 + (jy - i5 * vz2 - i4 * vz3) * x3s
                - ((jy - i5 * vz2 - i4 * vz3) * x1 + (jy - i5 * vz2 - i4 * vz3) * x2) * x3)
                * y1
            + ((i5 * vz1 + jx - i6 * vz3) * x1s - 2 * (i5 * vz1 + jx - i6 * vz3) * px13
                + (i5 * vz1 + jx - i6 * vz3) * x3s
                - (ke - k9 + ib * vz3) * y1s
                + (((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 - m2
                    + (hp - dvx12_r_vy2 - dvx12_r_vy3) * vz3)
                    * x1
                    - ((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 - m2
                        + (hp - dvx12_r_vy2 - dvx12_r_vy3) * vz3)
                        * x3)
                    * y1)
                * y2
            - ((i4 * vz1 + i6 * vz2 - jw) * x1s
                - (i4 * vz1 + i6 * vz2 - jw) * px12
                - (kc - i7 * vz2 + ka) * y1s
                - ((i4 * vz1 + i6 * vz2 - jw) * x1 - (i4 * vz1 + i6 * vz2 - jw) * x2) * x3
                + (((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1
                    - (2 * dvx13_r_vy1 - dvx13_r_vy2 - dvx13_r_vy3) * vz2
                    + (h5 - dvx23_r_vy2 - h6) * vz3)
                    * x1
                    + ((dvx23_r_vy1 + 2 * dvx12_r_vy2 - hl) * vz1 + m1
                        - (h2 - 2 * dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x2
                    - ((hh + dvx12_r_vy2 - he) * vz1
                        - (dvx13_r_vy1 + dvx13_r_vy2 - 2 * dvx13_r_vy3) * vz2
                        + (h7 + dvx23_r_vy2 - dvx12_r_vy3) * vz3)
                        * x3)
                    * y1
                - (((dvx23_r_vy1 + dvx23_r_vy2 - 2 * dvx23_r_vy3) * vz1
                    + (dvx12_r_vy1 - h4 + he) * vz2
                    - (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                    * x1
                    - ((dvx23_r_vy1 + dvx23_r_vy2 - 2 * dvx23_r_vy3) * vz1
                        + (dvx12_r_vy1 - h4 + he) * vz2
                        - (dvx13_r_vy1 - hg + dvx12_r_vy3) * vz3)
                        * x3
                    + (js * vz1 - j4 * vz2 + ju * vz3) * y1)
                    * y2)
                * y3
            + (2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px12
                + 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x3s
                + 2 * (k1 - k5) * y3s
                - 2 * ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1
                    + (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2)
                    * x3
                - ((h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x3)
                    * y1
                - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    - (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x3
                    - (k3 + k2 - l1 * vy3) * y1)
                    * y2
                + ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x1
                    + (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x2
                    - 2 * (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3)
                        * x3
                    - (k1 + k8 - k4) * y1
                    - (k7 - k5 - k6) * y2)
                    * y3)
                * z1)
            * z2
        + ((((hg - hl) * vz1
            - (dvx13_r_vy2 - dvx13_r_vy3) * vz2
            - (dvx12_r_vy2 - dvx12_r_vy3) * vz3)
            * x2
            - ((hg - hl) * vz1
                - (dvx13_r_vy2 - dvx13_r_vy3) * vz2
                - (dvx12_r_vy2 - dvx12_r_vy3) * vz3)
                * x3)
            * y1s
            + (((dvx23_r_vy1 - dvx23_r_vy3) * vz1 + (h7 - hi) * vz2
                - (dvx12_r_vy1 - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 - dvx23_r_vy3) * vz1 + (h7 - hi) * vz2
                    - (dvx12_r_vy1 - dvx12_r_vy3) * vz3)
                    * x3
                + (ke - k9 + ib * vz3) * y1)
                * y2s
            - ((jy - i5 * vz2 - i4 * vz3) * px12
                - (jy - i5 * vz2 - i4 * vz3) * x2s
                - ((jy - i5 * vz2 - i4 * vz3) * x1 - (jy - i5 * vz2 - i4 * vz3) * x2) * x3)
                * y1
            + ((i5 * vz1 + jx - i6 * vz3) * x1s
                - (i5 * vz1 + jx - i6 * vz3) * px12
                - (ke - k9 + ib * vz3) * y1s
                - ((i5 * vz1 + jx - i6 * vz3) * x1 - (i5 * vz1 + jx - i6 * vz3) * x2) * x3
                + (((dvx23_r_vy1 - h3 + dvx13_r_vy3) * vz1 - m2
                    + (hp - dvx12_r_vy2 - dvx12_r_vy3) * vz3)
                    * x1
                    - ((hh + hb - dvx13_r_vy3) * vz1 - (h2 - dvx13_r_vy2 - dvx23_r_vy3) * vz2
                        + (dvx12_r_vy1 - 2 * dvx12_r_vy2 + dvx12_r_vy3) * vz3)
                        * x2
                    + ((dvx23_r_vy1 + hg - 2 * dvx13_r_vy3) * vz1
                        + (h7 - dvx13_r_vy2 + 2 * dvx23_r_vy3) * vz2
                        - (dvx12_r_vy1 + dvx12_r_vy2 - h6) * vz3)
                        * x3)
                    * y1)
                * y2
            - ((i4 * vz1 + i6 * vz2 - jw) * x1s - 2 * (i4 * vz1 + i6 * vz2 - jw) * px12
                + (i4 * vz1 + i6 * vz2 - jw) * x2s
                - (kc - i7 * vz2 + ka) * y1s
                - (i9 * vz1 - kb + kd) * y2s
                + (((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1
                    - (2 * dvx13_r_vy1 - dvx13_r_vy2 - dvx13_r_vy3) * vz2
                    + (h5 - dvx23_r_vy2 - h6) * vz3)
                    * x1
                    - ((dvx23_r_vy1 - dvx12_r_vy2 + hi) * vz1
                        - (2 * dvx13_r_vy1 - dvx13_r_vy2 - dvx13_r_vy3) * vz2
                        + (h5 - dvx23_r_vy2 - h6) * vz3)
                        * x2)
                    * y1
                + (((dvx23_r_vy1 - 2 * dvx23_r_vy2 + dvx23_r_vy3) * vz1
                    + (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                    - (dvx13_r_vy1 + hb - h6) * vz3)
                    * x1
                    - ((dvx23_r_vy1 - 2 * dvx23_r_vy2 + dvx23_r_vy3) * vz1
                        + (dvx12_r_vy1 + dvx13_r_vy2 - hl) * vz2
                        - (dvx13_r_vy1 + hb - h6) * vz3)
                        * x2
                    + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vz1
                        - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vz2
                        + l1 * vz3)
                        * y1)
                    * y2)
                * y3
            + (2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px12
                - 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2s
                - 2 * (k3 - k6) * y2s
                - 2 * ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1
                    - (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2)
                    * x3
                - ((h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x3)
                    * y1
                - ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    - 2 * (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x2
                    + (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x3
                    - (k3 + k2 - l1 * vy3) * y1)
                    * y2
                + ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x1
                    - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x2
                    - (k1 + k8 - k4) * y1
                    + (k7 - k5 - k6) * y2)
                    * y3)
                * z1
            - (2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1s
                - 2 * (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * px12
                + 2 * (k2 - k4) * y1s
                - 2 * ((vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x1
                    - (vy1s_r_vy2 - vy1_r_vy2s + dvy12_r_vy3s - jm) * x2)
                    * x3
                - (2 * (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x1
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x2
                    - (h9 - dvx13_r_vy2s + dvx12_r_vy3s - (h5 - dvx23_r_vy2) * vy3) * x3)
                    * y1
                + ((dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x1
                    - (dvx23_r_vy1s + ha + dvx12_r_vy3s - (dvx13_r_vy1 + hb) * vy3) * x3
                    - (k3 + k2 - l1 * vy3) * y1)
                    * y2
                + ((dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x1
                    - (dvx23_r_vy1s + dvx12_r_vy1 * vy2 - dvx13_r_vy2s - (h2 - h3) * vy3) * x2
                    - (k1 + k8 - k4) * y1
                    + (k7 - k5 - k6) * y2)
                    * y3)
                * z2)
            * z3;

    let y = y_numerator / y_denominator;

    let z_numerator = (jq * x2s - 2 * jq * px23 + jq * x3s) * y1s
        - 2 * (jq * px12 + jq * x3s - (jq * x1 + jq * x2) * x3) * py12
        + (jq * x1s - 2 * jq * px13 + jq * x3s) * y2s
        + (jq * x1s - 2 * jq * px12 + jq * x2s) * y3s
        + ((i6 * vz2 - i6 * vz3) * x2s - 2 * (i6 * vz2 - i6 * vz3) * px23
            + (i6 * vz2 - i6 * vz3) * x3s
            + (k9 - ka) * y2s
            + (k9 - ka) * y3s
            - ((jl * vz2 - jl * vz3) * x2 - (jl * vz2 - jl * vz3) * x3) * y2
            + ((jl * vz2 - jl * vz3) * x2 - (jl * vz2 - jl * vz3) * x3 - 2 * (k9 - ka) * y2) * y3)
            * z1s
        + ((i4 * vz1 - i4 * vz3) * x1s - 2 * (i4 * vz1 - i4 * vz3) * px13
            + (i4 * vz1 - i4 * vz3) * x3s
            + (ke - kd) * y1s
            + (ke - kd) * y3s
            - (((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1
                    - (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                    * x3)
                * y1
            + (((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                * x1
                - ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1
                    - (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                    * x3
                - 2 * (ke - kd) * y1)
                * y3
            + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x1
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k3 - k2 + ib * vy3) * y1
                + (k3 - k2 + ib * vy3) * y3)
                * z1)
            * z2s
        + ((i5 * vz1 - i5 * vz2) * x1s - 2 * (i5 * vz1 - i5 * vz2) * px12
            + (i5 * vz1 - i5 * vz2) * x2s
            + (kc - kb) * y1s
            + (kc - kb) * y2s
            - (((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1
                    - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2)
                    * x2)
                * y1
            + (((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1 - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1
                    - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2)
                    * x2
                - 2 * (kc - kb) * y1)
                * y2
            + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (k1 - i7 * vy2 + k4) * y1
                + (k1 - i7 * vy2 + k4) * y2)
                * z1
            - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x2
                - (i9 * vy1 - k5 + k6) * y1
                + (i9 * vy1 - k5 + k6) * y2)
                * z2)
            * z3s
        + 2 * ((jq * px12 - jq * x2s - (jq * x1 - jq * x2) * x3) * y1
            - (jq * x1s - jq * px12 - (jq * x1 - jq * x2) * x3) * y2)
            * y3
        - (((ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x1
            - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x3)
            * y2s
            + ((ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x1
                - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2)
                * y3s
            + ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x2s
                - 2 * (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * px23
                + (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x3s)
                * y1
            - ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * px12
                + (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x3s
                - ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x1
                    + (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x2)
                    * x3
                + ((ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x3)
                    * y1)
                * y2
            + ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * px12
                - (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x2s
                - ((hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x1
                    - (hk - dvy13_r_vz2s + dvy12_r_vz3s - jo) * x2)
                    * x3
                + ((ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x3)
                    * y1
                - (2 * (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x1
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x2
                    - (ia - dvx13_r_vz2s + dvx12_r_vz3s - j9 * vz3) * x3)
                    * y2)
                * y3)
            * z1
        - (((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x2
            - (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x3)
            * y1s
            - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                - (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x2)
                * y3s
            + ((dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * x3
                - (k3 - k2 + ib * vy3) * y2
                + (k3 - k2 + ib * vy3) * y3)
                * z1s
            - ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * px12
                + (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x3s
                - ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x1
                    + (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x2)
                    * x3)
                * y1
            + ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x1s
                - 2 * (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * px13
                + (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x3s
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    - (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x3)
                    * y1)
                * y2
            - ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x1s
                - (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * px12
                - ((dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x1
                    - (dvy23_r_vz1s + hn + dvy12_r_vz3s - jp) * x2)
                    * x3
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    - 2 * (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x2
                    + (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x3)
                    * y1
                - ((dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x1
                    - (dvx23_r_vz1s + h8 + dvx12_r_vz3s - ja * vz3) * x3)
                    * y2)
                * y3
            + ((i4 * vz1 + i6 * vz2 - jw) * px12
                + (i4 * vz1 + i6 * vz2 - jw) * x3s
                + (ke + k9 - l1 * vz3) * y3s
                - ((i4 * vz1 + i6 * vz2 - jw) * x1 + (i4 * vz1 + i6 * vz2 - jw) * x2) * x3
                - (((hh - h3 + dvx12_r_vy3) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - jr) * x2
                    - ((hh - h3 + dvx12_r_vy3) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - jr) * x3)
                    * y1
                + (((dvx23_r_vy1 - hg + h6) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + jr) * x1
                    - ((dvx23_r_vy1 - hg + h6) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + jr) * x3
                    + (ke + k9 - l1 * vz3) * y1)
                    * y2
                - (((dvx23_r_vy1 - hg + h6) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2 + jr) * x1
                    - ((hh - h3 + dvx12_r_vy3) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2 - jr) * x2
                    + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 + jl * vz2 - 2 * jr) * x3
                    + (ke + k9 - l1 * vz3) * y1
                    + (ke + k9 - l1 * vz3) * y2)
                    * y3)
                * z1)
            * z2
        + (((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x2
            - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x3)
            * y1s
            + ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x1
                - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x3)
                * y2s
            + ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x2
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * x3
                - (k1 - i7 * vy2 + k4) * y2
                + (k1 - i7 * vy2 + k4) * y3)
                * z1s
            + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x1
                - (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * x3
                - (i9 * vy1 - k5 + k6) * y1
                + (i9 * vy1 - k5 + k6) * y3)
                * z2s
            - ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3) * px12
                - (dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x2s
                - ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x1
                    - (dvy23_r_vz1s + dvy12_r_rvz12
                        - dvy13_r_vz2s
                        - (h1 * vz1 - h1 * vz2) * vz3)
                        * x2)
                    * x3)
                * y1
            + ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3) * x1s
                - (dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * px12
                - ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x1
                    - (dvy23_r_vz1s + dvy12_r_rvz12
                        - dvy13_r_vz2s
                        - (h1 * vz1 - h1 * vz2) * vz3)
                        * x2)
                    * x3
                - ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x1
                    + (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x2
                    - 2 * (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x3)
                    * y1)
                * y2
            - ((dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3) * x1s
                - 2 * (dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * px12
                + (dvy23_r_vz1s + dvy12_r_rvz12 - dvy13_r_vz2s - (h1 * vz1 - h1 * vz2) * vz3)
                    * x2s
                - ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x1
                    - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x2)
                    * y1
                + ((dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x1
                    - (dvx23_r_vz1s + dvx12_r_rvz12 - dvx13_r_vz2s - jd) * x2)
                    * y2)
                * y3
            + ((i5 * vz1 + jx - i6 * vz3) * px12
                - (i5 * vz1 + jx - i6 * vz3) * x2s
                - (kc + j4 * vz2 - ka) * y2s
                - ((i5 * vz1 + jx - i6 * vz3) * x1 - (i5 * vz1 + jx - i6 * vz3) * x2) * x3
                - (((hh - dvx13_r_vy2 + hi) * vz1
                    + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    - (h2 - h4 + dvx12_r_vy3) * vz3)
                    * x2
                    - ((hh - dvx13_r_vy2 + hi) * vz1
                        + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                        - (h2 - h4 + dvx12_r_vy3) * vz3)
                        * x3)
                    * y1
                + (((dvx23_r_vy1 - h4 + hl) * vz1
                    - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    + (h7 + dvx13_r_vy2 - h6) * vz3)
                    * x1
                    + ((dvx23_r_vy1 + dvx13_r_vy2 - he) * vz1
                        + 2 * (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                        - jl * vz3)
                        * x2
                    - ((hh - dvx13_r_vy2 + hi) * vz1
                        + (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                        - (h2 - h4 + dvx12_r_vy3) * vz3)
                        * x3
                    + (kc + j4 * vz2 - ka) * y1)
                    * y2
                - (((dvx23_r_vy1 - h4 + hl) * vz1
                    - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                    + (h7 + dvx13_r_vy2 - h6) * vz3)
                    * x1
                    - ((dvx23_r_vy1 - h4 + hl) * vz1
                        - (dvx12_r_vy1 - dvx13_r_vy2 + dvx23_r_vy3) * vz2
                        + (h7 + dvx13_r_vy2 - h6) * vz3)
                        * x2
                    + (kc + j4 * vz2 - ka) * y1
                    - (kc + j4 * vz2 - ka) * y2)
                    * y3)
                * z1
            - ((jy - i5 * vz2 - i4 * vz3) * x1s - (jy - i5 * vz2 - i4 * vz3) * px12
                + (j7 * vz1 - kb - kd) * y1s
                - ((jy - i5 * vz2 - i4 * vz3) * x1 - (jy - i5 * vz2 - i4 * vz3) * x2) * x3
                - ((2 * j6 * vz1
                    - (dvx23_r_vy1 + dvx13_r_vy2 - he) * vz2
                    - (dvx23_r_vy1 + hb - dvx12_r_vy3) * vz3)
                    * x1
                    - (j6 * vz1 - (hh - dvx13_r_vy2 + hi) * vz2 + (dvx23_r_vy1 - hg + h6) * vz3)
                        * x2
                    - (j6 * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2 - (hh - h3 + dvx12_r_vy3) * vz3)
                        * x3)
                    * y1
                + ((j6 * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2 - (hh - h3 + dvx12_r_vy3) * vz3)
                    * x1
                    - (j6 * vz1 + (dvx23_r_vy1 - h4 + hl) * vz2 - (hh - h3 + dvx12_r_vy3) * vz3)
                        * x3
                    - (j7 * vz1 - kb - kd) * y1)
                    * y2
                + ((j6 * vz1 - (hh - dvx13_r_vy2 + hi) * vz2 + (dvx23_r_vy1 - hg + h6) * vz3)
                    * x1
                    - (j6 * vz1 - (hh - dvx13_r_vy2 + hi) * vz2 + (dvx23_r_vy1 - hg + h6) * vz3)
                        * x2
                    - (j7 * vz1 - kb - kd) * y1
                    + (j7 * vz1 - kb - kd) * y2)
                    * y3
                + ((2 * dvx23_r_vy1s - ig * rvy12 + dvx13_r_vy2s - dvx12_r_vy3s
                    + (ie * vy1 + dvx23_r_vy2) * vy3)
                    * x1
                    - (dvx23_r_vy1s - ik * rvy12
                        + 2 * dvx13_r_vy2s
                        + dvx12_r_vy3s
                        + (dvx13_r_vy1 - il * vy2) * vy3)
                        * x2
                    - (dvx23_r_vy1s - dvx12_r_rvy12 - dvx13_r_vy2s - 2 * dvx12_r_vy3s
                        + (ij * vy1 + id * vy2) * vy3)
                        * x3
                    - (k7 - jt * vy2 + (m2vx1s - m3rvx12 + vx2s - dvx12_r_vx3) * vy3) * y1
                    + (js * vy1 - k8 + ju * vy3) * y2
                    + ((rvx12 + vx2s - (vx1 + m3vx2) * vx3 + m2vx3s) * vy1
                        - (vx1s + rvx12 - (m3vx1 + vx2) * vx3 + m2vx3s) * vy2
                        + l1 * vy3)
                        * y3)
                    * z1)
                * z2)
            * z3;

    let z = z_numerator / z_denominator;

    let a_numerator = IBig::ZERO
        - ((ii * vz1s - 2 * i5 * rvz12 + ic * vz2s + ih * vz3s + 2 * (i4 * vz1 - i6 * vz2) * vz3)
            * x1s
            * x2
            - (ii * vz1s - 2 * i5 * rvz12
                + ic * vz2s
                + ih * vz3s
                + 2 * (i4 * vz1 - i6 * vz2) * vz3)
                * x1
                * x2s
            + ((ii * vz1s - 2 * i5 * rvz12
                + ic * vz2s
                + ih * vz3s
                + 2 * (i4 * vz1 - i6 * vz2) * vz3)
                * x1
                - (ii * vz1s - 2 * i5 * rvz12
                    + ic * vz2s
                    + ih * vz3s
                    + 2 * (i4 * vz1 - i6 * vz2) * vz3)
                    * x2)
                * x3s
            - (((rvx12 - rvx13) * rvz12
                - (vx1s - rvx13) * vz2s
                - (vx1s - rvx12) * vz3s
                - ((rvx12 - rvx13) * vz1 - (m2vx1s - rvx12 - rvx13) * vz2) * vz3)
                * x2
                - ((rvx12 - rvx13) * rvz12
                    - (vx1s - rvx13) * vz2s
                    - (vx1s - rvx12) * vz3s
                    - ((rvx12 - rvx13) * vz1 - (m2vx1s - rvx12 - rvx13) * vz2) * vz3)
                    * x3)
                * y1s
            - (((vx2s - rvx23) * vz1s - (rvx12 - rvx23) * rvz12 - (rvx12 - vx2s) * vz3s
                + ((rvx12 - m2vx2s + rvx23) * vz1 + (rvx12 - rvx23) * vz2) * vz3)
                * x1
                - ((vx2s - rvx23) * vz1s - (rvx12 - rvx23) * rvz12 - (rvx12 - vx2s) * vz3s
                    + ((rvx12 - m2vx2s + rvx23) * vz1 + (rvx12 - rvx23) * vz2) * vz3)
                    * x3)
                * y2s
            - (((rvx23 - vx3s) * vz1s - (pvx12_r_vx3 - m2vx3s) * rvz12
                + (rvx13 - vx3s) * vz2s
                + (dvx12_r_vx3 * vz1 - dvx12_r_vx3 * vz2) * vz3)
                * x1
                - ((rvx23 - vx3s) * vz1s - (pvx12_r_vx3 - m2vx3s) * rvz12
                    + (rvx13 - vx3s) * vz2s
                    + (dvx12_r_vx3 * vz1 - dvx12_r_vx3 * vz2) * vz3)
                    * x2)
                * y3s
            - (((rvx12 - rvx13) * rvy12
                - (vx1s - rvx13) * vy2s
                - (vx1s - rvx12) * vy3s
                - ((rvx12 - rvx13) * vy1 - (m2vx1s - rvx12 - rvx13) * vy2) * vy3)
                * x2
                - ((rvx12 - rvx13) * rvy12
                    - (vx1s - rvx13) * vy2s
                    - (vx1s - rvx12) * vy3s
                    - ((rvx12 - rvx13) * vy1 - (m2vx1s - rvx12 - rvx13) * vy2) * vy3)
                    * x3
                - ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vy1 - jv * vy2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vy3)
                    * y2
                + ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vy1 - jv * vy2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vy3)
                    * y3)
                * z1s
            - (((vx2s - rvx23) * vy1s - (rvx12 - rvx23) * rvy12 - (rvx12 - vx2s) * vy3s
                + ((rvx12 - m2vx2s + rvx23) * vy1 + (rvx12 - rvx23) * vy2) * vy3)
                * x1
                - ((vx2s - rvx23) * vy1s - (rvx12 - rvx23) * rvy12 - (rvx12 - vx2s) * vy3s
                    + ((rvx12 - m2vx2s + rvx23) * vy1 + (rvx12 - rvx23) * vy2) * vy3)
                    * x3
                - ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vy1
                    - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vy2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vy3)
                    * y1
                + ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vy1
                    - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vy2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vy3)
                    * y3)
                * z2s
            - (((rvx23 - vx3s) * vy1s - (pvx12_r_vx3 - m2vx3s) * rvy12
                + (rvx13 - vx3s) * vy2s
                + (dvx12_r_vx3 * vy1 - dvx12_r_vx3 * vy2) * vy3)
                * x1
                - ((rvx23 - vx3s) * vy1s - (pvx12_r_vx3 - m2vx3s) * rvy12
                    + (rvx13 - vx3s) * vy2s
                    + (dvx12_r_vx3 * vy1 - dvx12_r_vx3 * vy2) * vy3)
                    * x2
                - (ib * vxy33 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vy1
                    + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vy2)
                    * y1
                + (ib * vxy33 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vy1
                    + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vy2)
                    * y2)
                * z3s
            - ((ii * vz1s - 2 * i5 * rvz12
                + ic * vz2s
                + ih * vz3s
                + 2 * (i4 * vz1 - i6 * vz2) * vz3)
                * x1s
                - (ii * vz1s - 2 * i5 * rvz12
                    + ic * vz2s
                    + ih * vz3s
                    + 2 * (i4 * vz1 - i6 * vz2) * vz3)
                    * x2s)
                * x3
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                - (dvx23_r_vy1 + (m2vx1 - vx3) * vy2 - (m2vx1 + vx2 - m2vx3) * vy3) * rvz12
                + ((m2vx1 - vx3) * vy1 - (m2vx1 - vx3) * vy3) * vz2s
                + (m2vx1_d_vx2 * vy1 - m2vx1_d_vx2 * vy2) * vz3s
                + ((dvx23_r_vy1 + (m2vx1 - m2vx2 + vx3) * vy2 - m2vx1_d_vx2 * vy3) * vz1
                    - ((4 * vx1 - vx2 - vx3) * vy1 - (m2vx1 - vx3) * vy2 - m2vx1_d_vx2 * vy3)
                        * vz2)
                    * vz3)
                * px12
                + ((vxy32 - vxy33) * vz1s
                    - (vxy31 - dvx13_r_vy2 + (vx1 - m2vx3) * vy3) * rvz12
                    - jg * vz2s
                    - (vxy11 - vxy12) * vz3s
                    + ((vxy31 - pvx13 * vy2 + vxy13) * vz1
                        + ((m2vx1 - vx3) * vy1 - dvx13_r_vy2 - vxy13) * vz2)
                        * vz3)
                    * x2s
                + ((vxy22 - vxy23) * vz1s - (vxy21 + vxy12 - pvx12 * vy3) * rvz12
                    + (vxy11 - vxy13) * vz2s
                    + kh
                    + ((vxy21 + (vx1 - m2vx2) * vy2 - dvx12_r_vy3) * vz1
                        - (m2vx1_d_vx2 * vy1 - vxy12 - dvx12_r_vy3) * vz2)
                        * vz3)
                    * x3s
                - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                    - (dvx23_r_vy1 + (m2vx1 - vx3) * vy2 - (m2vx1 + vx2 - m2vx3) * vy3)
                        * vz1
                        * vz2
                    + ((m2vx1 - vx3) * vy1 - (m2vx1 - vx3) * vy3) * vz2s
                    + (m2vx1_d_vx2 * vy1 - m2vx1_d_vx2 * vy2) * vz3s
                    + ((dvx23_r_vy1 + (m2vx1 - m2vx2 + vx3) * vy2 - m2vx1_d_vx2 * vy3) * vz1
                        - ((4 * vx1 - vx2 - vx3) * vy1
                            - (m2vx1 - vx3) * vy2
                            - m2vx1_d_vx2 * vy3)
                            * vz2)
                        * vz3)
                    * x1
                    + ((pvx23 * vy2 - pvx23 * vy3) * vz1s
                        - (pvx23 * vy1 + vxy32 - (vx2 + m2vx3) * vy3) * rvz12
                        + (vxy31 - vxy33) * vz2s
                        - (vxy21 - vxy22) * vz3s
                        + ((pvx23 * vy1 - (m2vx2 + vx3) * vy2 + vxy23) * vz1
                            + (dvx23_r_vy1 + vxy32 - vxy23) * vz2)
                            * vz3)
                        * x2)
                    * x3)
                * y1
            - (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                - (dvx23_r_vy1 - vxy32 - (vx2 - m2vx3) * vy3) * rvz12
                - (vxy31 - vxy33) * vz2s
                - (vxy21 - vxy22) * vz3s
                + ((dvx23_r_vy1 - (m2vx2 - vx3) * vy2 + vxy23) * vz1
                    + (pvx23 * vy1 - vxy32 - vxy23) * vz2)
                    * vz3)
                * x1s
                - (((m2vx2 - vx3) * vy2 - (m2vx2 - vx3) * vy3) * vz1s
                    - ((m2vx2 - vx3) * vy1 + dvx13_r_vy2 - (vx1 + m2vx2 - m2vx3) * vy3)
                        * vz1
                        * vz2
                    + jg * vz2s
                    + ((vx1 - m2vx2) * vy1 - (vx1 - m2vx2) * vy2) * vz3s
                    + (((m2vx2 - vx3) * vy1 + (vx1 - 4 * vx2 + vx3) * vy2
                        - (vx1 - m2vx2) * vy3)
                        * vz1
                        - ((m2vx1 - m2vx2 - vx3) * vy1 - dvx13_r_vy2 - (vx1 - m2vx2) * vy3)
                            * vz2)
                        * vz3)
                    * px12
                - ((vxy22 - vxy23) * vz1s - (vxy21 + vxy12 - pvx12 * vy3) * rvz12
                    + (vxy11 - vxy13) * vz2s
                    + kh
                    + ((vxy21 + (vx1 - m2vx2) * vy2 - dvx12_r_vy3) * vz1
                        - (m2vx1_d_vx2 * vy1 - vxy12 - dvx12_r_vy3) * vz2)
                        * vz3)
                    * x3s
                + (((vxy32 - vxy33) * vz1s
                    - (vxy31 + pvx13 * vy2 - (vx1 + m2vx3) * vy3) * rvz12
                    + (pvx13 * vy1 - pvx13 * vy3) * vz2s
                    + (vxy11 - vxy12) * vz3s
                    + ((vxy31 + dvx13_r_vy2 - vxy13) * vz1
                        - ((m2vx1 + vx3) * vy1 - pvx13 * vy2 - vxy13) * vz2)
                        * vz3)
                    * x1
                    + (((m2vx2 - vx3) * vy2 - (m2vx2 - vx3) * vy3) * vz1s
                        - ((m2vx2 - vx3) * vy1 + dvx13_r_vy2 - (vx1 + m2vx2 - m2vx3) * vy3)
                            * vz1
                            * vz2
                        + jg * vz2s
                        + ((vx1 - m2vx2) * vy1 - (vx1 - m2vx2) * vy2) * vz3s
                        + (((m2vx2 - vx3) * vy1 + (vx1 - 4 * vx2 + vx3) * vy2
                            - (vx1 - m2vx2) * vy3)
                            * vz1
                            - ((m2vx1 - m2vx2 - vx3) * vy1
                                - dvx13_r_vy2
                                - (vx1 - m2vx2) * vy3)
                                * vz2)
                            * vz3)
                        * x2)
                    * x3
                - ((i9 * vz1s
                    - (rvx12 - (vx1 + m2vx2) * vx3 + m2vx3s) * rvz12
                    - (rvx13 - vx3s) * vz2s
                    - (rvx12 - vx2s) * vz3s
                    + ((rvx12 - m2vx2s - (vx1 - m2vx2) * vx3) * vz1
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vz2)
                        * vz3)
                    * x1
                    + ((rvx23 - vx3s) * vz1s + (rvx12 - (m2vx1 + vx2) * vx3 + m2vx3s) * rvz12
                        - i7 * vz2s
                        - (vx1s - rvx12) * vz3s
                        - ((rvx12 - ho) * vz1 - (m2vx1s - rvx12 - ho) * vz2) * vz3)
                        * x2
                    + (dvx12_r_vx3 * rvz12 - (vx2s - rvx23) * vz1s
                        + (vx1s - rvx13) * vz2s
                        + vx1s_d_vx2s * vz3s
                        + ((m2vx2s - pvx12_r_vx3) * vz1 - (m2vx1s - pvx12_r_vx3) * vz2) * vz3)
                        * x3)
                    * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                - (dvx23_r_vy1 - vxy32 - (vx2 - m2vx3) * vy3) * rvz12
                - (vxy31 - vxy33) * vz2s
                - (vxy21 - vxy22) * vz3s
                + ((dvx23_r_vy1 - (m2vx2 - vx3) * vy2 + vxy23) * vz1
                    + (pvx23 * vy1 - vxy32 - vxy23) * vz2)
                    * vz3)
                * x1s
                - ((vxy22 - vxy23) * vz1s
                    - (vxy21 - vxy12 + dvx12_r_vy3) * rvz12
                    - (vxy11 - vxy13) * vz2s
                    - (pvx12 * vy1 - pvx12 * vy2) * vz3s
                    + ((vxy21 - (vx1 + m2vx2) * vy2 + pvx12 * vy3) * vz1
                        + ((m2vx1 + vx2) * vy1 - vxy12 - pvx12 * vy3) * vz2)
                        * vz3)
                    * px12
                + ((vxy32 - vxy33) * vz1s
                    - (vxy31 - dvx13_r_vy2 + (vx1 - m2vx3) * vy3) * rvz12
                    - jg * vz2s
                    - (vxy11 - vxy12) * vz3s
                    + ((vxy31 - pvx13 * vy2 + vxy13) * vz1
                        + ((m2vx1 - vx3) * vy1 - dvx13_r_vy2 - vxy13) * vz2)
                        * vz3)
                    * x2s
                - ((((vx2 - m2vx3) * vy2 - (vx2 - m2vx3) * vy3) * vz1s
                    - ((vx2 - m2vx3) * vy1 + (vx1 - m2vx3) * vy2 - (vx1 + vx2 - 4 * vx3) * vy3)
                        * vz1
                        * vz2
                    + ((vx1 - m2vx3) * vy1 - (vx1 - m2vx3) * vy3) * vz2s
                    + kh
                    + (((vx2 - m2vx3) * vy1 + (vx1 - m2vx2 + m2vx3) * vy2 - dvx12_r_vy3) * vz1
                        - ((m2vx1 - vx2 - m2vx3) * vy1 - (vx1 - m2vx3) * vy2 - dvx12_r_vy3)
                            * vz2)
                        * vz3)
                    * x1
                    - (((vx2 - m2vx3) * vy2 - (vx2 - m2vx3) * vy3) * vz1s
                        - ((vx2 - m2vx3) * vy1 + (vx1 - m2vx3) * vy2
                            - (vx1 + vx2 - 4 * vx3) * vy3)
                            * vz1
                            * vz2
                        + ((vx1 - m2vx3) * vy1 - (vx1 - m2vx3) * vy3) * vz2s
                        + kh
                        + (((vx2 - m2vx3) * vy1 + (vx1 - m2vx2 + m2vx3) * vy2 - dvx12_r_vy3)
                            * vz1
                            - ((m2vx1 - vx2 - m2vx3) * vy1
                                - (vx1 - m2vx3) * vy2
                                - dvx12_r_vy3)
                                * vz2)
                            * vz3)
                        * x2)
                    * x3
                - ((i9 * vz1s
                    - (rvx12 - (vx1 + m2vx2) * vx3 + m2vx3s) * rvz12
                    - (rvx13 - vx3s) * vz2s
                    - (rvx12 - vx2s) * vz3s
                    + ((rvx12 - m2vx2s - (vx1 - m2vx2) * vx3) * vz1
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vz2)
                        * vz3)
                    * x1
                    + ((rvx23 - vx3s) * vz1s - (rvx12 + rvx23 - m2vx3s) * rvz12
                        + (vx1s - vx3s) * vz2s
                        + (vx1s - rvx12) * vz3s
                        + ((rvx12 - rvx23) * vz1 - (m2vx1s - rvx12 - rvx23) * vz2) * vz3)
                        * x2
                    - ((vx2s - rvx23) * vz1s - (m2vx1_r_vx2 - pvx12_r_vx3) * rvz12
                        + (vx1s - rvx13) * vz2s
                        + ib * vz3s
                        + ((m2vx1_r_vx2 - m2vx2s - dvx12_r_vx3) * vz1
                            - (m2vx1s - m2vx1_r_vx2 - dvx12_r_vx3) * vz2)
                            * vz3)
                        * x3)
                    * y1
                + (((vx2s - vx3s) * vz1s - (rvx12 + rvx13 - m2vx3s) * rvz12
                    + (rvx13 - vx3s) * vz2s
                    - (rvx12 - vx2s) * vz3s
                    + ((rvx12 - m2vx2s + rvx13) * vz1 + (rvx12 - rvx13) * vz2) * vz3)
                    * x1
                    - ((rvx23 - vx3s) * vz1s + (rvx12 - (m2vx1 + vx2) * vx3 + m2vx3s) * rvz12
                        - i7 * vz2s
                        - (vx1s - rvx12) * vz3s
                        - ((rvx12 - ho) * vz1 - (m2vx1s - rvx12 - ho) * vz2) * vz3)
                        * x2
                    - ((vx2s - rvx23) * vz1s - (m2vx1_r_vx2 - pvx12_r_vx3) * rvz12
                        + (vx1s - rvx13) * vz2s
                        + ib * vz3s
                        + ((m2vx1_r_vx2 - m2vx2s - dvx12_r_vx3) * vz1
                            - (m2vx1s - m2vx1_r_vx2 - dvx12_r_vx3) * vz2)
                            * vz3)
                        * x3)
                    * y2)
                * y3
            + (((hf
                - (m2vx1 - vx3) * vy2s
                - m2vx1_d_vx2 * vy3s
                - (dvx23_r_vy1 - (4 * vx1 - vx2 - vx3) * vy2) * vy3)
                * vz1
                - (dvx23_r_vy1s - (m2vx1 - vx3) * rvy12 - m2vx1_d_vx2 * vy3s
                    + ((m2vx1 - m2vx2 + vx3) * vy1 + (m2vx1 - vx3) * vy2) * vy3)
                    * vz2
                + (dvx23_r_vy1s - (m2vx1 + vx2 - m2vx3) * rvy12
                    + (m2vx1 - vx3) * vy2s
                    + (m2vx1_d_vx2 * vy1 - m2vx1_d_vx2 * vy2) * vy3)
                    * vz3)
                * px12
                + ((vx3 * rvy12 + dvx13_r_vy2s + vx1_r_vy3s
                    - (vxy31 + (m2vx1 - vx3) * vy2) * vy3)
                    * vz1
                    - (vx3_r_vy1s + dvx13_r_rvy12 + vx1_r_vy3s
                        - (pvx13 * vy1 + dvx13_r_vy2) * vy3)
                        * vz2
                    + (vx3_r_vy1s + (vx1 - m2vx3) * rvy12
                        - dvx13_r_vy2s
                        - (vxy11 - vxy12) * vy3)
                        * vz3)
                    * x2s
                + ((vxy21 * vy2 - vx1_r_vy2s - dvx12_r_vy3s - (vxy21 - m2vx1_d_vx2 * vy2) * vy3)
                    * vz1
                    - (vx2_r_vy1s - vx1 * rvy12 - dvx12_r_vy3s
                        + ((vx1 - m2vx2) * vy1 + vxy12) * vy3)
                        * vz2
                    + (vx2_r_vy1s - pvx12 * rvy12 + vx1_r_vy2s + kf) * vz3)
                    * x3s
                + ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vz1
                    - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vz2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vz3)
                    * y2s
                + (ib * vx3 * vz3 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vz1
                    + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vz2)
                    * y3s
                - (((hf
                    - (m2vx1 - vx3) * vy2s
                    - m2vx1_d_vx2 * vy3s
                    - (dvx23_r_vy1 - (4 * vx1 - vx2 - vx3) * vy2) * vy3)
                    * vz1
                    - (dvx23_r_vy1s - (m2vx1 - vx3) * rvy12 - m2vx1_d_vx2 * vy3s
                        + ((m2vx1 - m2vx2 + vx3) * vy1 + (m2vx1 - vx3) * vy2) * vy3)
                        * vz2
                    + (dvx23_r_vy1s - (m2vx1 + vx2 - m2vx3) * rvy12
                        + (m2vx1 - vx3) * vy2s
                        + (m2vx1_d_vx2 * vy1 - m2vx1_d_vx2 * vy2) * vy3)
                        * vz3)
                    * x1
                    + ((pvx23 * rvy12 - vx3_r_vy2s + vx2_r_vy3s
                        - (pvx23 * vy1 + dvx23_r_vy2) * vy3)
                        * vz1
                        - (pvx23 * vy1s - vx3 * rvy12 + vx2_r_vy3s
                            - ((m2vx2 + vx3) * vy1 - vxy32) * vy3)
                            * vz2
                        + (pvx23 * vy1s - (vx2 + m2vx3) * rvy12 + vx3_r_vy2s
                            - (vxy21 - vxy22) * vy3)
                            * vz3)
                        * x2)
                    * x3
                + ((((rvx12 - rvx13) * vy2 - (rvx12 - rvx13) * vy3) * vz1
                    + ((rvx12 - rvx13) * vy1 - 2 * (vx1s - rvx13) * vy2
                        + (m2vx1s - rvx12 - rvx13) * vy3)
                        * vz2
                    - ((rvx12 - rvx13) * vy1 - (m2vx1s - rvx12 - rvx13) * vy2
                        + 2 * (vx1s - rvx12) * vy3)
                        * vz3)
                    * x2
                    - (((rvx12 - rvx13) * vy2 - (rvx12 - rvx13) * vy3) * vz1
                        + ((rvx12 - rvx13) * vy1 - 2 * (vx1s - rvx13) * vy2
                            + (m2vx1s - rvx12 - rvx13) * vy3)
                            * vz2
                        - ((rvx12 - rvx13) * vy1 - (m2vx1s - rvx12 - rvx13) * vy2
                            + 2 * (vx1s - rvx12) * vy3)
                            * vz3)
                        * x3)
                    * y1
                - (((i9 * vy1 - (m2vx1_r_vx2 - (m2vx1 + vx2) * vx3 + vx3s) * vy2
                    + (m2vx1_r_vx2 - vx2s - ho) * vy3)
                    * vz1
                    + ((rvx12 - dvx12_r_vx3 - vx3s) * vy1
                        - (rvx13 - vx3s) * vy2
                        - (rvx12 - ho) * vy3)
                        * vz2
                    - ((rvx12 + vx2s - pvx12_r_vx3) * vy1 - (m2vx1_r_vx2 - pvx12_r_vx3) * vy2
                        + (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    + (((rvx23 - vx3s) * vy1
                        + (m2vx1_r_vx2 - (vx1 + m2vx2) * vx3 + vx3s) * vy2
                        - (m2vx1_r_vx2 - pvx12_r_vx3) * vy3)
                        * vz1
                        - ((rvx12 + dvx12_r_vx3 - vx3s) * vy1 + i7 * vy2
                            - (vx1s + rvx12 - pvx12_r_vx3) * vy3)
                            * vz2
                        + ((rvx12 + (vx1 - m2vx2) * vx3) * vy1
                            + (vx1s - m2vx1_r_vx2 - (vx1 - m2vx2) * vx3) * vy2
                            - (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    - ((dvx12_r_vx3 * vy2 + (vx2s - rvx23) * vy1
                        - (vx2s + (vx1 - m2vx2) * vx3) * vy3)
                        * vz1
                        - (hm * vy1 + (vx1s - rvx13) * vy2
                            - (vx1s + (vx1 - m2vx2) * vx3) * vy3)
                            * vz2
                        - ((vx2s - ho) * vy1 - (vx1s - ho) * vy2 + vx1s_d_vx2s * vy3) * vz3)
                        * x3
                    + ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vz1 - jv * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vz3)
                        * y1)
                    * y2
                + (((i9 * vy1 - (m2vx1_r_vx2 - (m2vx1 + vx2) * vx3 + vx3s) * vy2
                    + (m2vx1_r_vx2 - vx2s - ho) * vy3)
                    * vz1
                    + ((rvx12 - dvx12_r_vx3 - vx3s) * vy1
                        - (rvx13 - vx3s) * vy2
                        - (rvx12 - ho) * vy3)
                        * vz2
                    - ((rvx12 + vx2s - pvx12_r_vx3) * vy1 - (m2vx1_r_vx2 - pvx12_r_vx3) * vy2
                        + (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    + (((rvx23 - vx3s) * vy1 + (rvx12 - m2vx2_r_vx3 + vx3s) * vy2
                        - (rvx12 - rvx23) * vy3)
                        * vz1
                        - ((m2vx1_r_vx2 - rvx23 - vx3s) * vy1 - (vx1s - vx3s) * vy2
                            + (vx1s - m2vx1_r_vx2 + rvx23) * vy3)
                            * vz2
                        + (2 * (rvx12 - rvx23) * vy1 - (vx1s + rvx12 - m2vx2_r_vx3) * vy2
                            + (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    - (((vx2s - rvx23) * vy1 - (rvx12 - ho) * vy2 + (rvx12 - vx2s - hm) * vy3)
                        * vz1
                        - ((rvx12 + (vx1 - m2vx2) * vx3) * vy1 - (vx1s - rvx13) * vy2
                            + (vx1s - rvx12 - hm) * vy3)
                            * vz2
                        + ((rvx12 - vx2s + dvx12_r_vx3) * vy1
                            - (vx1s - rvx12 + dvx12_r_vx3) * vy2
                            + ib * vy3)
                            * vz3)
                        * x3
                    + ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vz1 - jv * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vz3)
                        * y1
                    - ((vx1_r_vx2s - 2 * vx2s_r_vx3 - (vx1 - m2vx2) * vx3s) * vz1
                        - (vx1s_r_vx2 - (vx1 - m2vx2) * vx3s + (vx1s - m3rvx12) * vx3) * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s + (vx1s - m3rvx12 + m2vx2s) * vx3) * vz3)
                        * y2)
                    * y3)
                * z1
            + (((dvx23_r_rvy12 + vx3_r_vy2s + vx2_r_vy3s - (dvx23_r_vy1 + pvx23 * vy2) * vy3)
                * vz1
                - (dvx23_r_vy1s + vx3 * rvy12 + vx2_r_vy3s
                    - ((m2vx2 - vx3) * vy1 + vxy32) * vy3)
                    * vz2
                + (dvx23_r_vy1s - (vx2 - m2vx3) * rvy12 - vx3_r_vy2s - (vxy21 - vxy22) * vy3)
                    * vz3)
                * x1s
                - (((m2vx2 - vx3) * rvy12
                    - dvx13_r_vy2s
                    - (vx1 - m2vx2) * vy3s
                    - ((m2vx2 - vx3) * vy1 - (m2vx1 - m2vx2 - vx3) * vy2) * vy3)
                    * vz1
                    - ((m2vx2 - vx3) * vy1s - dvx13_r_rvy12 - (vx1 - m2vx2) * vy3s
                        + ((vx1 - 4 * vx2 + vx3) * vy1 + dvx13_r_vy2) * vy3)
                        * vz2
                    + ((m2vx2 - vx3) * vy1s - (vx1 + m2vx2 - m2vx3) * rvy12
                        + dvx13_r_vy2s
                        + ((vx1 - m2vx2) * vy1 - (vx1 - m2vx2) * vy2) * vy3)
                        * vz3)
                    * px12
                - ((vxy21 * vy2 - vx1_r_vy2s - dvx12_r_vy3s - (vxy21 - m2vx1_d_vx2 * vy2) * vy3)
                    * vz1
                    - (vx2_r_vy1s - vx1 * rvy12 - dvx12_r_vy3s
                        + ((vx1 - m2vx2) * vy1 + vxy12) * vy3)
                        * vz2
                    + (vx2_r_vy1s - pvx12 * rvy12 + vx1_r_vy2s + kf) * vz3)
                    * x3s
                + ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vz1 - jv * vz2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vz3)
                    * y1s
                - (ib * vx3 * vz3 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vz1
                    + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vz2)
                    * y3s
                + (((vxy31 * vy2
                    - pvx13 * vy2s
                    - vx1_r_vy3s
                    - (vxy31 - (m2vx1 + vx3) * vy2) * vy3)
                    * vz1
                    - (vx3_r_vy1s - pvx13 * rvy12 - vx1_r_vy3s
                        + (dvx13_r_vy1 + pvx13 * vy2) * vy3)
                        * vz2
                    + (vx3_r_vy1s - (vx1 + m2vx3) * rvy12
                        + pvx13 * vy2s
                        + (vxy11 - vxy12) * vy3)
                        * vz3)
                    * x1
                    + (((m2vx2 - vx3) * rvy12
                        - dvx13_r_vy2s
                        - (vx1 - m2vx2) * vy3s
                        - ((m2vx2 - vx3) * vy1 - (m2vx1 - m2vx2 - vx3) * vy2) * vy3)
                        * vz1
                        - ((m2vx2 - vx3) * vy1s - dvx13_r_rvy12 - (vx1 - m2vx2) * vy3s
                            + ((vx1 - 4 * vx2 + vx3) * vy1 + dvx13_r_vy2) * vy3)
                            * vz2
                        + ((m2vx2 - vx3) * vy1s - (vx1 + m2vx2 - m2vx3) * rvy12
                            + dvx13_r_vy2s
                            + ((vx1 - m2vx2) * vy1 - (vx1 - m2vx2) * vy2) * vy3)
                            * vz3)
                        * x2)
                    * x3
                - (((i9 * vy1 + (rvx12 - dvx12_r_vx3 - vx3s) * vy2
                    - (rvx12 + vx2s - pvx12_r_vx3) * vy3)
                    * vz1
                    - ((m2vx1_r_vx2 - (m2vx1 + vx2) * vx3 + vx3s) * vy1 + (rvx13 - vx3s) * vy2
                        - (m2vx1_r_vx2 - pvx12_r_vx3) * vy3)
                        * vz2
                    + ((m2vx1_r_vx2 - vx2s - ho) * vy1
                        - (rvx12 - ho) * vy2
                        - (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    + (((rvx23 - vx3s) * vy1 - (rvx12 + dvx12_r_vx3 - vx3s) * vy2
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vy3)
                        * vz1
                        + ((m2vx1_r_vx2 - (vx1 + m2vx2) * vx3 + vx3s) * vy1 - i7 * vy2
                            + (vx1s - m2vx1_r_vx2 - (vx1 - m2vx2) * vx3) * vy3)
                            * vz2
                        - ((m2vx1_r_vx2 - pvx12_r_vx3) * vy1
                            - (vx1s + rvx12 - pvx12_r_vx3) * vy2
                            + (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    + ((hm * vy2 - (vx2s - rvx23) * vy1 + (vx2s - ho) * vy3) * vz1
                        - (dvx12_r_vx3 * vy1 - (vx1s - rvx13) * vy2 + (vx1s - ho) * vy3) * vz2
                        + ((vx2s + (vx1 - m2vx2) * vx3) * vy1
                            - (vx1s + (vx1 - m2vx2) * vx3) * vy2
                            + vx1s_d_vx2s * vy3)
                            * vz3)
                        * x3)
                    * y1
                + (((2 * (vx2s - rvx23) * vy1 - (rvx12 - rvx23) * vy2
                    + (rvx12 - m2vx2s + rvx23) * vy3)
                    * vz1
                    - ((rvx12 - rvx23) * vy1 - (rvx12 - rvx23) * vy3) * vz2
                    + ((rvx12 - m2vx2s + rvx23) * vy1 + (rvx12 - rvx23) * vy2
                        - 2 * (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    - ((2 * (vx2s - rvx23) * vy1 - (rvx12 - rvx23) * vy2
                        + (rvx12 - m2vx2s + rvx23) * vy3)
                        * vz1
                        - ((rvx12 - rvx23) * vy1 - (rvx12 - rvx23) * vy3) * vz2
                        + ((rvx12 - m2vx2s + rvx23) * vy1 + (rvx12 - rvx23) * vy2
                            - 2 * (rvx12 - vx2s) * vy3)
                            * vz3)
                        * x3
                    - ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vz1
                        - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vz3)
                        * y1)
                    * y2
                - ((((vx2s - vx3s) * vy1 - (m2vx1_r_vx2 - rvx13 - vx3s) * vy2
                    + (m2vx1_r_vx2 - vx2s - rvx13) * vy3)
                    * vz1
                    + ((rvx12 - m2vx1_r_vx3 + vx3s) * vy1 + (rvx13 - vx3s) * vy2
                        - (rvx12 - rvx13) * vy3)
                        * vz2
                    - ((rvx12 + vx2s - m2vx1_r_vx3) * vy1 - 2 * (rvx12 - rvx13) * vy2
                        + (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    - (((rvx23 - vx3s) * vy1 - (rvx12 + dvx12_r_vx3 - vx3s) * vy2
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vy3)
                        * vz1
                        + ((m2vx1_r_vx2 - (vx1 + m2vx2) * vx3 + vx3s) * vy1 - i7 * vy2
                            + (vx1s - m2vx1_r_vx2 - (vx1 - m2vx2) * vx3) * vy3)
                            * vz2
                        - ((m2vx1_r_vx2 - pvx12_r_vx3) * vy1
                            - (vx1s + rvx12 - pvx12_r_vx3) * vy2
                            + (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    - (((vx2s - rvx23) * vy1 - (rvx12 - ho) * vy2 + (rvx12 - vx2s - hm) * vy3)
                        * vz1
                        - ((rvx12 + (vx1 - m2vx2) * vx3) * vy1 - (vx1s - rvx13) * vy2
                            + (vx1s - rvx12 - hm) * vy3)
                            * vz2
                        + ((rvx12 - vx2s + dvx12_r_vx3) * vy1
                            - (vx1s - rvx12 + dvx12_r_vx3) * vy2
                            + ib * vy3)
                            * vz3)
                        * x3
                    + ((vx1_r_vx2s + m2vx1_d_vx2 * vx3s - (m3rvx12 - vx2s) * vx3) * vz1
                        - (vx1s_r_vx2 - 2 * vx1s_r_vx3 + m2vx1_d_vx2 * vx3s) * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (m2vx1s - m3rvx12 + vx2s) * vx3) * vz3)
                        * y1
                    - ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vz1
                        - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vz2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vz3)
                        * y2)
                    * y3
                + ((i9 * vy1s
                    - (rvx12 - (vx1 + m2vx2) * vx3 + m2vx3s) * rvy12
                    - (rvx13 - vx3s) * vy2s
                    - (rvx12 - vx2s) * vy3s
                    + ((rvx12 - m2vx2s - (vx1 - m2vx2) * vx3) * vy1
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vy2)
                        * vy3)
                    * x1
                    + ((rvx23 - vx3s) * vy1s + (rvx12 - (m2vx1 + vx2) * vx3 + m2vx3s) * rvy12
                        - i7 * vy2s
                        - (vx1s - rvx12) * vy3s
                        - ((rvx12 - ho) * vy1 - (m2vx1s - rvx12 - ho) * vy2) * vy3)
                        * x2
                    + (dvx12_r_vx3 * rvy12 - (vx2s - rvx23) * vy1s
                        + (vx1s - rvx13) * vy2s
                        + vx1s_d_vx2s * vy3s
                        + ((m2vx2s - pvx12_r_vx3) * vy1 - (m2vx1s - pvx12_r_vx3) * vy2) * vy3)
                        * x3
                    - ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vy1 - jv * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vy3)
                        * y1
                    - ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vy1
                        - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vy3)
                        * y2
                    + ((m2vx1 * vx2s + pvx12 * vx3s - (m3rvx12 + vx2s) * vx3) * vy1
                        - (2 * vx1s_r_vx2 + pvx12 * vx3s - (vx1s + m3rvx12) * vx3) * vy2
                        + (2 * vx1s_r_vx2 - m2vx1 * vx2s - hc) * vy3)
                        * y3)
                    * z1)
                * z2
            - (((dvx23_r_rvy12 + vx3_r_vy2s + vx2_r_vy3s - (dvx23_r_vy1 + pvx23 * vy2) * vy3)
                * vz1
                - (dvx23_r_vy1s + vx3 * rvy12 + vx2_r_vy3s
                    - ((m2vx2 - vx3) * vy1 + vxy32) * vy3)
                    * vz2
                + (dvx23_r_vy1s - (vx2 - m2vx3) * rvy12 - vx3_r_vy2s - (vxy21 - vxy22) * vy3)
                    * vz3)
                * x1s
                - ((vx2 * rvy12 + vx1_r_vy2s + pvx12 * vy3s
                    - (vxy21 + (m2vx1 + vx2) * vy2) * vy3)
                    * vz1
                    - (vx2_r_vy1s + vx1 * rvy12 + pvx12 * vy3s
                        - ((vx1 + m2vx2) * vy1 + vxy12) * vy3)
                        * vz2
                    + (vx2_r_vy1s + dvx12_r_vy1 * vy2
                        - vx1_r_vy2s
                        - (pvx12 * vy1 - pvx12 * vy2) * vy3)
                        * vz3)
                    * px12
                + ((vx3 * rvy12 + dvx13_r_vy2s + vx1_r_vy3s
                    - (vxy31 + (m2vx1 - vx3) * vy2) * vy3)
                    * vz1
                    - (vx3_r_vy1s + dvx13_r_rvy12 + vx1_r_vy3s
                        - (pvx13 * vy1 + dvx13_r_vy2) * vy3)
                        * vz2
                    + (vx3_r_vy1s + (vx1 - m2vx3) * rvy12
                        - dvx13_r_vy2s
                        - (vxy11 - vxy12) * vy3)
                        * vz3)
                    * x2s
                + ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vz1 - jv * vz2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vz3)
                    * y1s
                + ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vz1
                    - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vz2
                    + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vz3)
                    * y2s
                - ((((vx2 - m2vx3) * rvy12
                    - (vx1 - m2vx3) * vy2s
                    - dvx12_r_vy3s
                    - ((vx2 - m2vx3) * vy1 - (m2vx1 - vx2 - m2vx3) * vy2) * vy3)
                    * vz1
                    - ((vx2 - m2vx3) * vy1s - (vx1 - m2vx3) * rvy12 - dvx12_r_vy3s
                        + ((vx1 - m2vx2 + m2vx3) * vy1 + (vx1 - m2vx3) * vy2) * vy3)
                        * vz2
                    + ((vx2 - m2vx3) * vy1s - (vx1 + vx2 - 4 * vx3) * rvy12
                        + (vx1 - m2vx3) * vy2s
                        + kf)
                        * vz3)
                    * x1
                    - (((vx2 - m2vx3) * rvy12
                        - (vx1 - m2vx3) * vy2s
                        - dvx12_r_vy3s
                        - ((vx2 - m2vx3) * vy1 - (m2vx1 - vx2 - m2vx3) * vy2) * vy3)
                        * vz1
                        - ((vx2 - m2vx3) * vy1s - (vx1 - m2vx3) * rvy12 - dvx12_r_vy3s
                            + ((vx1 - m2vx2 + m2vx3) * vy1 + (vx1 - m2vx3) * vy2) * vy3)
                            * vz2
                        + ((vx2 - m2vx3) * vy1s - (vx1 + vx2 - 4 * vx3) * rvy12
                            + (vx1 - m2vx3) * vy2s
                            + kf)
                            * vz3)
                        * x2)
                    * x3
                - (((i9 * vy1 + (rvx12 - dvx12_r_vx3 - vx3s) * vy2
                    - (rvx12 + vx2s - pvx12_r_vx3) * vy3)
                    * vz1
                    - ((m2vx1_r_vx2 - (m2vx1 + vx2) * vx3 + vx3s) * vy1 + (rvx13 - vx3s) * vy2
                        - (m2vx1_r_vx2 - pvx12_r_vx3) * vy3)
                        * vz2
                    + ((m2vx1_r_vx2 - vx2s - ho) * vy1
                        - (rvx12 - ho) * vy2
                        - (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    + (((rvx23 - vx3s) * vy1 - (m2vx1_r_vx2 - rvx23 - vx3s) * vy2
                        + 2 * (rvx12 - rvx23) * vy3)
                        * vz1
                        + ((rvx12 - m2vx2_r_vx3 + vx3s) * vy1 + (vx1s - vx3s) * vy2
                            - (vx1s + rvx12 - m2vx2_r_vx3) * vy3)
                            * vz2
                        - ((rvx12 - rvx23) * vy1 + (vx1s - m2vx1_r_vx2 + rvx23) * vy2
                            - (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    - (((vx2s - rvx23) * vy1 - (rvx12 + (vx1 - m2vx2) * vx3) * vy2
                        + (rvx12 - vx2s + dvx12_r_vx3) * vy3)
                        * vz1
                        - ((rvx12 - ho) * vy1 - (vx1s - rvx13) * vy2
                            + (vx1s - rvx12 + dvx12_r_vx3) * vy3)
                            * vz2
                        + ((rvx12 - vx2s - hm) * vy1 - (vx1s - rvx12 - hm) * vy2 + ib * vy3)
                            * vz3)
                        * x3)
                    * y1
                + ((((vx2s - vx3s) * vy1 + (rvx12 - m2vx1_r_vx3 + vx3s) * vy2
                    - (rvx12 + vx2s - m2vx1_r_vx3) * vy3)
                    * vz1
                    - ((m2vx1_r_vx2 - rvx13 - vx3s) * vy1
                        - (rvx13 - vx3s) * vy2
                        - 2 * (rvx12 - rvx13) * vy3)
                        * vz2
                    + ((m2vx1_r_vx2 - vx2s - rvx13) * vy1
                        - (rvx12 - rvx13) * vy2
                        - (rvx12 - vx2s) * vy3)
                        * vz3)
                    * x1
                    - (((rvx23 - vx3s) * vy1
                        + (m2vx1_r_vx2 - (vx1 + m2vx2) * vx3 + vx3s) * vy2
                        - (m2vx1_r_vx2 - pvx12_r_vx3) * vy3)
                        * vz1
                        - ((rvx12 + dvx12_r_vx3 - vx3s) * vy1 + i7 * vy2
                            - (vx1s + rvx12 - pvx12_r_vx3) * vy3)
                            * vz2
                        + ((rvx12 + (vx1 - m2vx2) * vx3) * vy1
                            + (vx1s - m2vx1_r_vx2 - (vx1 - m2vx2) * vx3) * vy2
                            - (vx1s - rvx12) * vy3)
                            * vz3)
                        * x2
                    - (((vx2s - rvx23) * vy1 - (rvx12 + (vx1 - m2vx2) * vx3) * vy2
                        + (rvx12 - vx2s + dvx12_r_vx3) * vy3)
                        * vz1
                        - ((rvx12 - ho) * vy1 - (vx1s - rvx13) * vy2
                            + (vx1s - rvx12 + dvx12_r_vx3) * vy3)
                            * vz2
                        + ((rvx12 - vx2s - hm) * vy1 - (vx1s - rvx12 - hm) * vy2 + ib * vy3)
                            * vz3)
                        * x3
                    - ((m2vx1 * vx2s + pvx12 * vx3s - (m3rvx12 + vx2s) * vx3) * vz1
                        - (2 * vx1s_r_vx2 + pvx12 * vx3s - (vx1s + m3rvx12) * vx3) * vz2
                        + (2 * vx1s_r_vx2 - m2vx1 * vx2s - hc) * vz3)
                        * y1)
                    * y2
                - (((dvx12_r_vx3 * vy3 + 2 * (rvx23 - vx3s) * vy1
                    - (pvx12_r_vx3 - m2vx3s) * vy2)
                    * vz1
                    - (dvx12_r_vx3 * vy3 + (pvx12_r_vx3 - m2vx3s) * vy1
                        - 2 * (rvx13 - vx3s) * vy2)
                        * vz2
                    + (dvx12_r_vx3 * vy1 - dvx12_r_vx3 * vy2) * vz3)
                    * x1
                    - ((dvx12_r_vx3 * vy3 + 2 * (rvx23 - vx3s) * vy1
                        - (pvx12_r_vx3 - m2vx3s) * vy2)
                        * vz1
                        - (dvx12_r_vx3 * vy3 + (pvx12_r_vx3 - m2vx3s) * vy1
                            - 2 * (rvx13 - vx3s) * vy2)
                            * vz2
                        + (dvx12_r_vx3 * vy1 - dvx12_r_vx3 * vy2) * vz3)
                        * x2
                    - (ib * vx3 * vz3 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vz1
                        + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vz2)
                        * y1
                    + (ib * vx3 * vz3 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vz1
                        + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vz2)
                        * y2)
                    * y3
                + ((i9 * vy1s
                    - (rvx12 - (vx1 + m2vx2) * vx3 + m2vx3s) * rvy12
                    - (rvx13 - vx3s) * vy2s
                    - (rvx12 - vx2s) * vy3s
                    + ((rvx12 - m2vx2s - (vx1 - m2vx2) * vx3) * vy1
                        + (rvx12 + (vx1 - m2vx2) * vx3) * vy2)
                        * vy3)
                    * x1
                    + ((rvx23 - vx3s) * vy1s - (rvx12 + rvx23 - m2vx3s) * rvy12
                        + (vx1s - vx3s) * vy2s
                        + (vx1s - rvx12) * vy3s
                        + ((rvx12 - rvx23) * vy1 - (m2vx1s - rvx12 - rvx23) * vy2) * vy3)
                        * x2
                    - ((vx2s - rvx23) * vy1s - (m2vx1_r_vx2 - pvx12_r_vx3) * rvy12
                        + (vx1s - rvx13) * vy2s
                        + ib * vy3s
                        + ((m2vx1_r_vx2 - m2vx2s - dvx12_r_vx3) * vy1
                            - (m2vx1s - m2vx1_r_vx2 - dvx12_r_vx3) * vy2)
                            * vy3)
                        * x3
                    - ((vx1_r_vx2s - m2vx1 * rvx23 + vx1_r_vx3s) * vy1 - jv * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (vx1s - rvx12) * vx3) * vy3)
                        * y1
                    + ((vx1_r_vx2s + m2vx1_d_vx2 * vx3s - (m3rvx12 - vx2s) * vx3) * vy1
                        - (vx1s_r_vx2 - 2 * vx1s_r_vx3 + m2vx1_d_vx2 * vx3s) * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (m2vx1s - m3rvx12 + vx2s) * vx3) * vy3)
                        * y2
                    + (ib * vxy33 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vy1
                        + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vy2)
                        * y3)
                    * z1
                - (((vx2s - vx3s) * vy1s - (rvx12 + rvx13 - m2vx3s) * rvy12
                    + (rvx13 - vx3s) * vy2s
                    - (rvx12 - vx2s) * vy3s
                    + ((rvx12 - m2vx2s + rvx13) * vy1 + (rvx12 - rvx13) * vy2) * vy3)
                    * x1
                    - ((rvx23 - vx3s) * vy1s + (rvx12 - (m2vx1 + vx2) * vx3 + m2vx3s) * rvy12
                        - i7 * vy2s
                        - (vx1s - rvx12) * vy3s
                        - ((rvx12 - ho) * vy1 - (m2vx1s - rvx12 - ho) * vy2) * vy3)
                        * x2
                    - ((vx2s - rvx23) * vy1s - (m2vx1_r_vx2 - pvx12_r_vx3) * rvy12
                        + (vx1s - rvx13) * vy2s
                        + ib * vy3s
                        + ((m2vx1_r_vx2 - m2vx2s - dvx12_r_vx3) * vy1
                            - (m2vx1s - m2vx1_r_vx2 - dvx12_r_vx3) * vy2)
                            * vy3)
                        * x3
                    - ((vx1_r_vx2s - 2 * vx2s_r_vx3 - (vx1 - m2vx2) * vx3s) * vy1
                        - (vx1s_r_vx2 - (vx1 - m2vx2) * vx3s + (vx1s - m3rvx12) * vx3) * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s + (vx1s - m3rvx12 + m2vx2s) * vx3) * vy3)
                        * y1
                    + ((vx1_r_vx2s + vx2_r_vx3s - (rvx12 + vx2s) * vx3) * vy1
                        - (vx1s_r_vx2 - m2vx1 * rvx23 + vx2_r_vx3s) * vy2
                        + (vx1s_r_vx2 - vx1_r_vx2s - (rvx12 - vx2s) * vx3) * vy3)
                        * y2
                    + (ib * vxy33 - (dvx12_r_vx3s - (rvx12 - vx2s) * vx3) * vy1
                        + (dvx12_r_vx3s - (vx1s - rvx12) * vx3) * vy2)
                        * y3)
                    * z2)
                * z3);

    let a = a_numerator / a_denominator;

    let b_numerator = IBig::ZERO
        - (((dvx13_r_vy3 * vz2s + dvx12_r_vy2 * vz3s - (dvx23_r_vy2 - dvx23_r_vy3) * vz1s
            + (dvx13_r_vy2 - he) * rvz12
            - ((hb - dvx12_r_vy3) * vz1 + (dvx13_r_vy2 + dvx12_r_vy3) * vz2) * vz3)
            * x2
            - (dvx13_r_vy3 * vz2s + dvx12_r_vy2 * vz3s - (dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                + (dvx13_r_vy2 - he) * rvz12
                - ((hb - dvx12_r_vy3) * vz1 + (dvx13_r_vy2 + dvx12_r_vy3) * vz2) * vz3)
                * x3)
            * y1s
            - ((dvx23_r_vy3 * vz1s - dvx12_r_vy1 * vz3s + (dvx23_r_vy1 - he) * rvz12
                - jg * vz2s
                - ((dvx23_r_vy1 - dvx12_r_vy3) * vz1 - (h5 - dvx12_r_vy3) * vz2) * vz3)
                * x1
                - (dvx23_r_vy3 * vz1s - dvx12_r_vy1 * vz3s + (dvx23_r_vy1 - he) * rvz12
                    - jg * vz2s
                    - ((dvx23_r_vy1 - dvx12_r_vy3) * vz1 - (h5 - dvx12_r_vy3) * vz2) * vz3)
                    * x3
                + (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3) * y1)
                * y2s
            - ((dvx23_r_vy2 * vz1s + dvx13_r_vy1 * vz2s - (dvx23_r_vy1 + dvx13_r_vy2) * rvz12
                + kh
                + ((dvx23_r_vy1 + hb) * vz1 - (h5 - dvx13_r_vy2) * vz2) * vz3)
                * x1
                - (dvx23_r_vy2 * vz1s + dvx13_r_vy1 * vz2s - (dvx23_r_vy1 + dvx13_r_vy2) * rvz12
                    + kh
                    + ((dvx23_r_vy1 + hb) * vz1 - (h5 - dvx13_r_vy2) * vz2) * vz3)
                    * x2
                - (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3) * y1
                + (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3) * y2)
                * y3s
            - ((dvx23_r_vy1s * vy2
                - dvx13_r_vy1 * vy2s
                - dvx12_r_vy1 * vy3s
                - (dvx23_r_vy1s - h9) * vy3)
                * x2
                - (dvx23_r_vy1s * vy2
                    - dvx13_r_vy1 * vy2s
                    - dvx12_r_vy1 * vy3s
                    - (dvx23_r_vy1s - h9) * vy3)
                    * x3
                - (i9 * vy1s - i2 * rvy12 + i3 * rvy13) * y2
                + (i9 * vy1s - i2 * rvy12 + i3 * rvy13) * y3)
                * z1s
            - ((dvx23_r_vy1s * vy2 - dvx13_r_vy1 * vy2s - dvx12_r_vy2 * vy3s
                + (ha + dvx13_r_vy2s) * vy3)
                * x1
                - (dvx23_r_vy1s * vy2 - dvx13_r_vy1 * vy2s - dvx12_r_vy2 * vy3s
                    + (ha + dvx13_r_vy2s) * vy3)
                    * x3
                - (i2 * rvy12 - i7 * vy2s + i1 * rvy23) * y1
                + (i2 * rvy12 - i7 * vy2s + i1 * rvy23) * y3)
                * z2s
            - ((j5 * vy3s + (dvx23_r_vy1s - hd + dvx13_r_vy2s) * vy3) * x1
                - (j5 * vy3s + (dvx23_r_vy1s - hd + dvx13_r_vy2s) * vy3) * x2
                - (ib * vy3s + (k3 - k2) * vy3) * y1
                + (ib * vy3s + (k3 - k2) * vy3) * y2)
                * z3s
            + ((ii * vz1s
                - (rvy12 - (vy1 + m2vy2) * vy3 + 2 * vy3s) * rvz12
                - (rvy13 - vy3s) * vz2s
                - (rvy12 - vy2s) * vz3s
                + ((rvy12 - 2 * vy2s - (vy1 - m2vy2) * vy3) * vz1
                    + (rvy12 + (vy1 - m2vy2) * vy3) * vz2)
                    * vz3)
                * px12
                - ((vy2s - rvy23) * vz1s - (rvy12 - rvy23) * rvz12 - (rvy12 - vy2s) * vz3s
                    + ((rvy12 - 2 * vy2s + rvy23) * vz1 + (rvy12 - rvy23) * vz2) * vz3)
                    * x2s
                - ((rvy23 - vy3s) * vz1s - (pvy12_r_vy3 - 2 * vy3s) * rvz12
                    + (rvy13 - vy3s) * vz2s
                    + (dvy12_r_vy3 * vz1 - dvy12_r_vy3 * vz2) * vz3)
                    * x3s
                - ((ii * vz1s
                    - (rvy12 - (vy1 + m2vy2) * vy3 + 2 * vy3s) * rvz12
                    - (rvy13 - vy3s) * vz2s
                    - (rvy12 - vy2s) * vz3s
                    + ((rvy12 - 2 * vy2s - (vy1 - m2vy2) * vy3) * vz1
                        + (rvy12 + (vy1 - m2vy2) * vy3) * vz2)
                        * vz3)
                    * x1
                    - ((vy2s - vy3s) * vz1s - (rvy12 + rvy13 - 2 * vy3s) * rvz12
                        + (rvy13 - vy3s) * vz2s
                        - (rvy12 - vy2s) * vz3s
                        + ((rvy12 - 2 * vy2s + rvy13) * vz1 + (rvy12 - rvy13) * vz2) * vz3)
                        * x2)
                    * x3)
                * y1
            - (((rvy12 - rvy13) * rvz12
                - (vy1s - rvy13) * vz2s
                - (vy1s - rvy12) * vz3s
                - ((rvy12 - rvy13) * vz1 - (m2vy1s - rvy12 - rvy13) * vz2) * vz3)
                * x1s
                - ((rvy23 - vy3s) * vz1s + (rvy12 - (m2vy1 + vy2) * vy3 + 2 * vy3s) * rvz12
                    - ic * vz2s
                    - (vy1s - rvy12) * vz3s
                    - ((rvy12 - (m2vy1 - vy2) * vy3) * vz1
                        - (m2vy1s - rvy12 - (m2vy1 - vy2) * vy3) * vz2)
                        * vz3)
                    * px12
                - ((rvy23 - vy3s) * vz1s - (pvy12_r_vy3 - 2 * vy3s) * rvz12
                    + (rvy13 - vy3s) * vz2s
                    + (dvy12_r_vy3 * vz1 - dvy12_r_vy3 * vz2) * vz3)
                    * x3s
                - (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3)
                    * y1s
                + (((rvy23 - vy3s) * vz1s - (rvy12 + rvy23 - 2 * vy3s) * rvz12
                    + (vy1s - vy3s) * vz2s
                    + (vy1s - rvy12) * vz3s
                    + ((rvy12 - rvy23) * vz1 - (m2vy1s - rvy12 - rvy23) * vz2) * vz3)
                    * x1
                    + ((rvy23 - vy3s) * vz1s + (rvy12 - (m2vy1 + vy2) * vy3 + 2 * vy3s) * rvz12
                        - ic * vz2s
                        - (vy1s - rvy12) * vz3s
                        - ((rvy12 - (m2vy1 - vy2) * vy3) * vz1
                            - (m2vy1s - rvy12 - (m2vy1 - vy2) * vy3) * vz2)
                            * vz3)
                        * x2)
                    * x3
                + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (hh + dvx13_r_vy2 - he) * rvz12
                    + (2 * dvx13_r_vy1 - dvx13_r_vy3) * vz2s
                    + (hp - dvx12_r_vy2) * vz3s
                    + ((hh + hb - dvx12_r_vy3) * vz1
                        - (2 * h5 - dvx13_r_vy2 - dvx12_r_vy3) * vz2)
                        * vz3)
                    * x1
                    - ((2 * dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + h4 - he) * rvz12
                        + jg * vz2s
                        + (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vz3s
                        + ((dvx23_r_vy1 + 2 * hb - dvx12_r_vy3) * vz1
                            - (h5 - h4 - dvx12_r_vy3) * vz2)
                            * vz3)
                        * x2
                    + (dvx23_r_vy2 * vz1s - dvx13_r_vy1 * vz2s
                        + (dvx23_r_vy1 - dvx13_r_vy2) * rvz12
                        - jf * vz3s
                        - ((dvx23_r_vy1 - hb) * vz1 - (h5 + dvx13_r_vy2) * vz2) * vz3)
                        * x3)
                    * y1)
                * y2
            + (((rvy12 - rvy13) * rvz12
                - (vy1s - rvy13) * vz2s
                - (vy1s - rvy12) * vz3s
                - ((rvy12 - rvy13) * vz1 - (m2vy1s - rvy12 - rvy13) * vz2) * vz3)
                * x1s
                + (dvy12_r_vy3 * rvz12 - (vy2s - rvy23) * vz1s
                    + (vy1s - rvy13) * vz2s
                    + vy1s_d_vy2s * vz3s
                    + ((2 * vy2s - pvy12_r_vy3) * vz1 - (m2vy1s - pvy12_r_vy3) * vz2) * vz3)
                    * px12
                + ((vy2s - rvy23) * vz1s - (rvy12 - rvy23) * rvz12 - (rvy12 - vy2s) * vz3s
                    + ((rvy12 - 2 * vy2s + rvy23) * vz1 + (rvy12 - rvy23) * vz2) * vz3)
                    * x2s
                - (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3)
                    * y1s
                + (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + ib * vz3s + 2 * (ke - k9) * vz3)
                    * y2s
                + (((vy2s - rvy23) * vz1s - (m2vy1_r_vy2 - pvy12_r_vy3) * rvz12
                    + (vy1s - rvy13) * vz2s
                    + ih * vz3s
                    + ((m2vy1_r_vy2 - 2 * vy2s - dvy12_r_vy3) * vz1
                        - (m2vy1s - m2vy1_r_vy2 - dvy12_r_vy3) * vz2)
                        * vz3)
                    * x1
                    - ((vy2s - rvy23) * vz1s - (m2vy1_r_vy2 - pvy12_r_vy3) * rvz12
                        + (vy1s - rvy13) * vz2s
                        + ih * vz3s
                        + ((m2vy1_r_vy2 - 2 * vy2s - dvy12_r_vy3) * vz1
                            - (m2vy1s - m2vy1_r_vy2 - dvy12_r_vy3) * vz2)
                            * vz3)
                        * x2)
                    * x3
                + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (hh + dvx13_r_vy2 - he) * rvz12
                    + (2 * dvx13_r_vy1 - dvx13_r_vy3) * vz2s
                    + (hp - dvx12_r_vy2) * vz3s
                    + ((hh + hb - dvx12_r_vy3) * vz1
                        - (2 * h5 - dvx13_r_vy2 - dvx12_r_vy3) * vz2)
                        * vz3)
                    * x1
                    - (dvx23_r_vy3 * vz1s + dvx12_r_vy1 * vz3s - (dvx23_r_vy1 + he) * rvz12
                        + (dvx13_r_vy1 + dvx13_r_vy3) * vz2s
                        + ((dvx23_r_vy1 + dvx12_r_vy3) * vz1 - (h5 + dvx12_r_vy3) * vz2) * vz3)
                        * x2
                    - ((dvx23_r_vy2 - 2 * dvx23_r_vy3) * vz1s
                        - (dvx23_r_vy1 + dvx13_r_vy2 - 2 * he) * rvz12
                        + (dvx13_r_vy1 - 2 * dvx13_r_vy3) * vz2s
                        + kh
                        + ((dvx23_r_vy1 + hb - h6) * vz1 - (h5 - dvx13_r_vy2 - h6) * vz2) * vz3)
                        * x3)
                    * y1
                + ((dvx13_r_vy3 * vz2s - dvx12_r_vy2 * vz3s
                    + (dvx23_r_vy2 + dvx23_r_vy3) * vz1s
                    - (dvx13_r_vy2 + he) * rvz12
                    + ((hb + dvx12_r_vy3) * vz1 + (dvx13_r_vy2 - dvx12_r_vy3) * vz2) * vz3)
                    * x1
                    - ((2 * dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + h4 - he) * rvz12
                        + jg * vz2s
                        + (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vz3s
                        + ((dvx23_r_vy1 + 2 * hb - dvx12_r_vy3) * vz1
                            - (h5 - h4 - dvx12_r_vy3) * vz2)
                            * vz3)
                        * x2
                    + ((dvx23_r_vy2 - 2 * dvx23_r_vy3) * vz1s
                        - (dvx23_r_vy1 + dvx13_r_vy2 - 2 * he) * rvz12
                        + (dvx13_r_vy1 - 2 * dvx13_r_vy3) * vz2s
                        + kh
                        + ((dvx23_r_vy1 + hb - h6) * vz1 - (h5 - dvx13_r_vy2 - h6) * vz2) * vz3)
                        * x3)
                    * y2)
                * y3
            - (((vy1_r_vy2s - m2vy1 * rvy23 + vy1_r_vy3s) * vz1
                - (vy1s_r_vy2 + vy1_r_vy3s - (vy1s + rvy12) * vy3) * vz2
                + (vy1s_r_vy2 - vy1_r_vy2s - (vy1s - rvy12) * vy3) * vz3)
                * px12
                - ((vy1_r_vy2s + vy2_r_vy3s - (rvy12 + vy2s) * vy3) * vz1
                    - (vy1s_r_vy2 - m2vy1 * rvy23 + vy2_r_vy3s) * vz2
                    + (vy1s_r_vy2 - vy1_r_vy2s - (rvy12 - vy2s) * vy3) * vz3)
                    * x2s
                - (ih * vy3 * vz3 - (dvy12_r_vy3s - (rvy12 - vy2s) * vy3) * vz1
                    + (dvy12_r_vy3s - (vy1s - rvy12) * vy3) * vz2)
                    * x3s
                - ((i9 * vy1 + k6) * vz1 - (k1 + k4) * vz2 + (k3 + ib * vy3) * vz3) * y2s
                + ((i9 * vy1 - k5) * vz1 - (k1 - i7 * vy2) * vz2 + (k3 - k2) * vz3) * y3s
                - (((vy1_r_vy2s - m2vy1 * rvy23 + vy1_r_vy3s) * vz1
                    - (vy1s_r_vy2 + vy1_r_vy3s - (vy1s + rvy12) * vy3) * vz2
                    + (vy1s_r_vy2 - vy1_r_vy2s - (vy1s - rvy12) * vy3) * vz3)
                    * x1
                    - ((vy1_r_vy2s - 2 * vy2s_r_vy3 - (vy1 - m2vy2) * vy3s) * vz1
                        - (vy1s_r_vy2 - (vy1 - m2vy2) * vy3s + (vy1s - m2rvy12) * vy3) * vz2
                        + (vy1s_r_vy2 - vy1_r_vy2s + (vy1s - m2rvy12 + 2 * vy2s) * vy3) * vz3)
                        * x2)
                    * x3
                - (((2 * hf - dvx13_r_vy2s - dvx12_r_vy3s - (hh - hg) * vy3) * vz1
                    - (dvx13_r_rvy12 - dvx12_r_vy3s - (h2 - dvx13_r_vy2) * vy3) * vz2
                    + (ha + dvx13_r_vy2s - jf * vy3) * vz3)
                    * x2
                    - ((2 * hf - dvx13_r_vy2s - dvx12_r_vy3s - (hh - hg) * vy3) * vz1
                        - (dvx13_r_rvy12 - dvx12_r_vy3s - (h2 - dvx13_r_vy2) * vy3) * vz2
                        + (ha + dvx13_r_vy2s - jf * vy3) * vz3)
                        * x3)
                    * y1
                - (((dvx23_r_rvy12 - dvx23 * rvy13) * vz1
                    - (2 * dvx23_r_vy1s - dvx13_r_rvy12 + pvx13_d_m2vx2 * rvy13) * vz2
                    + (2 * dvx23_r_vy1s - hd + dvx12 * rvy13) * vz3)
                    * x1
                    - ((2 * hf - dvx12_r_vy3s - (dvx23_r_vy1 - hb) * vy3) * vz1
                        - (dvx23_r_vy1s + dvx13_r_rvy12 - dvx12_r_vy3s
                            + (h7 - dvx13_r_vy2) * vy3)
                            * vz2
                        + (dvx23_r_vy1s + ha + (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vy3) * vz3)
                        * x2
                    + ((dvx23_r_rvy12 + pvx13_d_m2vx2 * rvy23 - dvx12_r_vy3s) * vz1
                        + (dvx23_r_vy1s - 2 * dvx13_r_rvy12 + dvx13 * rvy23 + dvx12_r_vy3s)
                            * vz2
                        - (dvx23_r_vy1s - h9 + 2 * dvx12 * rvy23) * vz3)
                        * x3
                    - ((2 * i9 * vy1 - k5 + k6) * vz1 - (2 * k1 - i7 * vy2 + k4) * vz2
                        + (2 * k3 - k2 + ib * vy3) * vz3)
                        * y1)
                    * y2
                + (((dvx23_r_rvy12 - dvx23 * rvy13) * vz1
                    - (2 * dvx23_r_vy1s - dvx13_r_rvy12 + pvx13_d_m2vx2 * rvy13) * vz2
                    + (2 * dvx23_r_vy1s - hd + dvx12 * rvy13) * vz3)
                    * x1
                    - ((dvx13_r_vy2s + (dvx23_r_vy1 - h3) * vy3) * vz1
                        - (dvx23_r_vy1s + (h5 - h4) * vy3) * vz2
                        + (dvx23_r_vy1s - dvx13_r_vy2s + (hp - dvx12_r_vy2) * vy3) * vz3)
                        * x2
                    - ((hf - dvx13_r_vy2s - (hh - h3) * vy3) * vz1
                        - (dvx23_r_vy1s - dvx13_r_rvy12 - (h2 - h4) * vy3) * vz2
                        + (dvx23_r_vy1s - hd + dvx13_r_vy2s - kf) * vz3)
                        * x3
                    - ((2 * i9 * vy1 - k5 + k6) * vz1 - (2 * k1 - i7 * vy2 + k4) * vz2
                        + (2 * k3 - k2 + ib * vy3) * vz3)
                        * y1
                    + ((k5 + k6) * vz1 - (i7 * vy2 + k4) * vz2 + (k2 + ib * vy3) * vz3) * y2)
                    * y3)
                * z1
            + (((vy1_r_vy2s - m2vy1 * rvy23 + vy1_r_vy3s) * vz1
                - (vy1s_r_vy2 + vy1_r_vy3s - (vy1s + rvy12) * vy3) * vz2
                + (vy1s_r_vy2 - vy1_r_vy2s - (vy1s - rvy12) * vy3) * vz3)
                * x1s
                - ((vy1_r_vy2s + vy2_r_vy3s - (rvy12 + vy2s) * vy3) * vz1
                    - (vy1s_r_vy2 - m2vy1 * rvy23 + vy2_r_vy3s) * vz2
                    + (vy1s_r_vy2 - vy1_r_vy2s - (rvy12 - vy2s) * vy3) * vz3)
                    * px12
                - (ih * vy3 * vz3 - (dvy12_r_vy3s - (rvy12 - vy2s) * vy3) * vz1
                    + (dvy12_r_vy3s - (vy1s - rvy12) * vy3) * vz2)
                    * x3s
                + ((k5 - k6) * vz1 - (i7 * vy2 - k4) * vz2 + (k2 - ib * vy3) * vz3) * y1s
                + ((i9 * vy1 - k5) * vz1 - (k1 - i7 * vy2) * vz2 + (k3 - k2) * vz3) * y3s
                - (((vy1_r_vy2s + (m2vy1 - vy2) * vy3s - (m2rvy12 - vy2s) * vy3) * vz1
                    - (vy1s_r_vy2 - 2 * vy1s_r_vy3 + (m2vy1 - vy2) * vy3s) * vz2
                    + (vy1s_r_vy2 - vy1_r_vy2s - (m2vy1s - m2rvy12 + vy2s) * vy3) * vz3)
                    * x1
                    - ((vy1_r_vy2s + vy2_r_vy3s - (rvy12 + vy2s) * vy3) * vz1
                        - (vy1s_r_vy2 - m2vy1 * rvy23 + vy2_r_vy3s) * vz2
                        + (vy1s_r_vy2 - vy1_r_vy2s - (rvy12 - vy2s) * vy3) * vz3)
                        * x2)
                    * x3
                - (((dvx23_r_rvy12 + dvx13_r_vy2s + dvx12_r_vy3s - (dvx23_r_vy1 + hg) * vy3)
                    * vz1
                    - (2 * dvx13_r_rvy12 + dvx12_r_vy3s - (h5 + dvx13_r_vy2) * vy3) * vz2
                    + (h9 - dvx13_r_vy2s - (hp - dvx12_r_vy2) * vy3) * vz3)
                    * x1
                    + ((dvx23_r_rvy12 - 2 * dvx13_r_vy2s + m2vx1_d_dvx23 * rvy23) * vz1
                        + (dvx13_r_rvy12 - dvx13 * rvy23) * vz2
                        - (hd - 2 * dvx13_r_vy2s + dvx12 * rvy23) * vz3)
                        * x2
                    - ((2 * dvx23_r_rvy12 - dvx13_r_vy2s - dvx23 * rvy13 + dvx12_r_vy3s) * vz1
                        - (dvx13_r_rvy12 - m2vx1_d_dvx23 * rvy13 + dvx12_r_vy3s) * vz2
                        + (ha + dvx13_r_vy2s - 2 * dvx12 * rvy13) * vz3)
                        * x3)
                    * y1
                + (((hf + dvx12_r_vy3s + (dvx23_r_vy1 - h3) * vy3) * vz1
                    + (dvx23_r_vy1s - 2 * dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + h4) * vy3) * vz2
                    - (dvx23_r_vy1s - h9 + jf * vy3) * vz3)
                    * x1
                    - ((hf + dvx12_r_vy3s + (dvx23_r_vy1 - h3) * vy3) * vz1
                        + (dvx23_r_vy1s - 2 * dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + h4) * vy3)
                            * vz2
                        - (dvx23_r_vy1s - h9 + jf * vy3) * vz3)
                        * x3
                    + ((i9 * vy1 - 2 * k5 + k6) * vz1 - (k1 - 2 * i7 * vy2 + k4) * vz2
                        + (k3 - 2 * k2 + ib * vy3) * vz3)
                        * y1)
                    * y2
                + (((dvx13_r_vy2s - (hh + hb) * vy3) * vz1
                    - (dvx23_r_vy1s - (h2 - dvx13_r_vy2) * vy3) * vz2
                    + (dvx23_r_vy1s - dvx13_r_vy2s - (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vy3)
                        * vz3)
                    * x1
                    + ((dvx23_r_rvy12 - 2 * dvx13_r_vy2s + m2vx1_d_dvx23 * rvy23) * vz1
                        + (dvx13_r_rvy12 - dvx13 * rvy23) * vz2
                        - (hd - 2 * dvx13_r_vy2s + dvx12 * rvy23) * vz3)
                        * x2
                    - ((hf - dvx13_r_vy2s - (hh - h3) * vy3) * vz1
                        - (dvx23_r_vy1s - dvx13_r_rvy12 - (h2 - h4) * vy3) * vz2
                        + (dvx23_r_vy1s - hd + dvx13_r_vy2s - kf) * vz3)
                        * x3
                    - ((i9 * vy1 - k6) * vz1 - (k1 - k4) * vz2 + (k3 - ib * vy3) * vz3) * y1
                    - ((i9 * vy1 - 2 * k5 + k6) * vz1 - (k1 - 2 * i7 * vy2 + k4) * vz2
                        + (k3 - 2 * k2 + ib * vy3) * vz3)
                        * y2)
                    * y3
                + ((dvx23_r_vy1s * vy2
                    - dvx13_r_vy1 * vy2s
                    - dvx12_r_vy1 * vy3s
                    - (dvx23_r_vy1s - h9) * vy3)
                    * x1
                    + (dvx23_r_vy1s * vy2 - dvx13_r_vy1 * vy2s - dvx12_r_vy2 * vy3s
                        + (ha + dvx13_r_vy2s) * vy3)
                        * x2
                    - (2 * dvx23_r_vy1s * vy2
                        - 2 * dvx13_r_vy1 * vy2s
                        - jf * vy3s
                        - (dvx23_r_vy1s - 3 * dvx12_r_rvy12 - dvx13_r_vy2s) * vy3)
                        * x3
                    - (i2 * rvy12 - i7 * vy2s - ib * vy3s - (k3 - 2 * k2) * vy3) * y1
                    - (i9 * vy1s - i2 * rvy12 + ib * vy3s + (2 * k3 - k2) * vy3) * y2
                    + (i9 * vy1s - i7 * vy2s + (k3 + k2) * vy3) * y3)
                    * z1)
                * z2
            - (((vy1_r_vy2s - m2vy1 * rvy23 + vy1_r_vy3s) * vz1
                - (vy1s_r_vy2 + vy1_r_vy3s - (vy1s + rvy12) * vy3) * vz2
                + (vy1s_r_vy2 - vy1_r_vy2s - (vy1s - rvy12) * vy3) * vz3)
                * x1s
                - ((m2vy1 * vy2s + pvy12 * vy3s - (m2rvy12 + vy2s) * vy3) * vz1
                    - (2 * vy1s_r_vy2 + pvy12 * vy3s - (vy1s + m2rvy12) * vy3) * vz2
                    + (2 * vy1s_r_vy2 - m2vy1 * vy2s - jm) * vz3)
                    * px12
                + ((vy1_r_vy2s + vy2_r_vy3s - (rvy12 + vy2s) * vy3) * vz1
                    - (vy1s_r_vy2 - m2vy1 * rvy23 + vy2_r_vy3s) * vz2
                    + (vy1s_r_vy2 - vy1_r_vy2s - (rvy12 - vy2s) * vy3) * vz3)
                    * x2s
                + ((k5 - k6) * vz1 - (i7 * vy2 - k4) * vz2 + (k2 - ib * vy3) * vz3) * y1s
                + ((i9 * vy1 + k6) * vz1 - (k1 + k4) * vz2 + (k3 + ib * vy3) * vz3) * y2s
                + ((ih * vy3 * vz3 - (dvy12_r_vy3s - (rvy12 - vy2s) * vy3) * vz1
                    + (dvy12_r_vy3s - (vy1s - rvy12) * vy3) * vz2)
                    * x1
                    - (ih * vy3 * vz3 - (dvy12_r_vy3s - (rvy12 - vy2s) * vy3) * vz1
                        + (dvy12_r_vy3s - (vy1s - rvy12) * vy3) * vz2)
                        * x2)
                    * x3
                - (((dvx23_r_rvy12 + dvx13_r_vy2s + dvx12_r_vy3s - (dvx23_r_vy1 + hg) * vy3)
                    * vz1
                    - (2 * dvx13_r_rvy12 + dvx12_r_vy3s - (h5 + dvx13_r_vy2) * vy3) * vz2
                    + (h9 - dvx13_r_vy2s - (hp - dvx12_r_vy2) * vy3) * vz3)
                    * x1
                    - ((dvx23_r_rvy12 + dvx13_r_vy2s - 2 * dvx23 * rvy13 - dvx12_r_vy3s) * vz1
                        - (2 * dvx13_r_rvy12 - pvx12_d_m2vx3 * rvy13 - dvx12_r_vy3s) * vz2
                        + (h9 - dvx13_r_vy2s - dvx12 * rvy13) * vz3)
                        * x2
                    + (kf * vz3 - (2 * dvx12_r_vy3s + (dvx23_r_vy1 - hg) * vy3) * vz1
                        + (2 * dvx12_r_vy3s - (h7 + dvx13_r_vy2) * vy3) * vz2)
                        * x3)
                    * y1
                + (((2 * dvx23_r_rvy12 - pvx12_d_m2vx3 * rvy23 + dvx12_r_vy3s) * vz1
                    - (dvx23_r_vy1s + dvx13_r_rvy12 - 2 * dvx13 * rvy23 + dvx12_r_vy3s) * vz2
                    + (dvx23_r_vy1s + ha - dvx12 * rvy23) * vz3)
                    * x1
                    - ((2 * hf - dvx12_r_vy3s - (dvx23_r_vy1 - hb) * vy3) * vz1
                        - (dvx23_r_vy1s + dvx13_r_rvy12 - dvx12_r_vy3s
                            + (h7 - dvx13_r_vy2) * vy3)
                            * vz2
                        + (dvx23_r_vy1s + ha + (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vy3) * vz3)
                        * x2
                    + (kf * vz3 - (2 * dvx12_r_vy3s + (dvx23_r_vy1 - hg) * vy3) * vz1
                        + (2 * dvx12_r_vy3s - (h7 + dvx13_r_vy2) * vy3) * vz2)
                        * x3
                    - ((i9 * vy1 + k5) * vz1 - (k1 + i7 * vy2) * vz2 + (k3 + k2) * vz3) * y1)
                    * y2
                - (((dvx23_r_rvy12 - dvx13_r_vy2s + (dvx23_r_vy1 + hb) * vy3) * vz1
                    - (dvx23_r_vy1s - dvx13_r_rvy12 + (h5 - dvx13_r_vy2) * vy3) * vz2
                    + (dvx23_r_vy1s - hd + dvx13_r_vy2s + 2 * kf) * vz3)
                    * x1
                    - ((dvx23_r_rvy12 - dvx13_r_vy2s + (dvx23_r_vy1 + hb) * vy3) * vz1
                        - (dvx23_r_vy1s - dvx13_r_rvy12 + (h5 - dvx13_r_vy2) * vy3) * vz2
                        + (dvx23_r_vy1s - hd + dvx13_r_vy2s + 2 * kf) * vz3)
                        * x2
                    - ((i9 * vy1 - k5 + 2 * k6) * vz1 - (k1 - i7 * vy2 + 2 * k4) * vz2
                        + (k3 - k2 + 2 * ib * vy3) * vz3)
                        * y1
                    + ((i9 * vy1 - k5 + 2 * k6) * vz1 - (k1 - i7 * vy2 + 2 * k4) * vz2
                        + (k3 - k2 + 2 * ib * vy3) * vz3)
                        * y2)
                    * y3
                + ((dvx23_r_vy1s * vy2
                    - dvx13_r_vy1 * vy2s
                    - dvx12_r_vy1 * vy3s
                    - (dvx23_r_vy1s - h9) * vy3)
                    * x1
                    - (dvx23_r_vy1s * vy2
                        - dvx13_r_vy1 * vy2s
                        - (hp - dvx12_r_vy2) * vy3s
                        - (2 * dvx23_r_vy1s - 3 * dvx13_r_rvy12 + dvx13_r_vy2s) * vy3)
                        * x2
                    - (j5 * vy3s + (dvx23_r_vy1s - hd + dvx13_r_vy2s) * vy3) * x3
                    - (i2 * rvy12 - i7 * vy2s - ib * vy3s - (k3 - 2 * k2) * vy3) * y1
                    + (i9 * vy1s - i2 * rvy12 + i1 * rvy23 - ib * vy3s) * y2
                    - (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + (k3 - k2) * vy3) * y3)
                    * z1
                - ((dvx23_r_vy1s * vy2 - dvx13_r_vy1 * vy2s
                    + (dvx12_r_vy1 - 2 * dvx12_r_vy2) * vy3s
                    + (dvx23_r_vy1s - 3 * dvx23_r_rvy12 + 2 * dvx13_r_vy2s) * vy3)
                    * x1
                    - (dvx23_r_vy1s * vy2 - dvx13_r_vy1 * vy2s - dvx12_r_vy2 * vy3s
                        + (ha + dvx13_r_vy2s) * vy3)
                        * x2
                    - (j5 * vy3s + (dvx23_r_vy1s - hd + dvx13_r_vy2s) * vy3) * x3
                    - (i2 * rvy12 - i7 * vy2s + i3 * rvy13 + ib * vy3s) * y1
                    + (i9 * vy1s - i2 * rvy12 + ib * vy3s + (2 * k3 - k2) * vy3) * y2
                    - (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + (k3 - k2) * vy3) * y3)
                    * z2)
                * z3);

    let b = b_numerator / b_denominator;

    let c_numerator = ((dvx23_r_vz1s * vz2
        - dvx13_r_vz1 * vz2s
        - dvx12_r_vz1 * vz3s
        - (dvx23_r_vz1s - ia) * vz3)
        * x2
        - (dvx23_r_vz1s * vz2
            - dvx13_r_vz1 * vz2s
            - dvx12_r_vz1 * vz3s
            - (dvx23_r_vz1s - ia) * vz3)
            * x3)
        * y1s
        + ((dvx23_r_vz1s * vz2 - dvx13_r_vz1 * vz2s - dvx12_r_vz2 * vz3s
            + (h8 + dvx13_r_vz2s) * vz3)
            * x1
            - (dvx23_r_vz1s * vz2 - dvx13_r_vz1 * vz2s - dvx12_r_vz2 * vz3s
                + (h8 + dvx13_r_vz2s) * vz3)
                * x3)
            * y2s
        + ((ji * vz3s + (dvx23_r_vz1s - hj + dvx13_r_vz2s) * vz3) * x1
            - (ji * vz3s + (dvx23_r_vz1s - hj + dvx13_r_vz2s) * vz3) * x2)
            * y3s
        + (((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2
            - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
            * x2
            - ((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2
                - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                * x3
            - ((k1 - i7 * vy2 + k4) * vz2 - (k3 - k2 + ib * vy3) * vz3) * y2
            + ((k1 - i7 * vy2 + k4) * vz2 - (k3 - k2 + ib * vy3) * vz3) * y3)
            * z1s
        + (((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
            + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
            * x1
            - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                * x3
            - ((i9 * vy1 - k5 + k6) * vz1 + (k3 - k2 + ib * vy3) * vz3) * y1
            + ((i9 * vy1 - k5 + k6) * vz1 + (k3 - k2 + ib * vy3) * vz3) * y3
            + (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z1)
            * z2s
        - (((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
            - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2)
            * x1
            - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2)
                * x2
            - ((i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2) * y1
            + ((i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2) * y2
            + (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z1
            - (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z2)
            * z3s
        - ((dvy23_r_vz1s * vz2
            - dvy13_r_vz1 * vz2s
            - dvy12_r_vz1 * vz3s
            - (dvy23_r_vz1s - hk) * vz3)
            * px12
            - (dvy23_r_vz1s * vz2 - dvy13_r_vz1 * vz2s - dvy12_r_vz2 * vz3s
                + (hn + dvy13_r_vz2s) * vz3)
                * x2s
            - (j2 * vz3s + (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s) * vz3) * x3s
            - ((dvy23_r_vz1s * vz2
                - dvy13_r_vz1 * vz2s
                - dvy12_r_vz1 * vz3s
                - (dvy23_r_vz1s - hk) * vz3)
                * x1
                - (dvy23_r_vz1s * vz2 - dvy13_r_vz1 * vz2s
                    + (dvy12_r_vz1 - 2 * dvy12_r_vz2) * vz3s
                    + (dvy23_r_vz1s - 3 * dvy23_r_rvz12 + 2 * dvy13_r_vz2s) * vz3)
                    * x2)
                * x3)
            * y1
        + ((dvy23_r_vz1s * vz2
            - dvy13_r_vz1 * vz2s
            - dvy12_r_vz1 * vz3s
            - (dvy23_r_vz1s - hk) * vz3)
            * x1s
            - (dvy23_r_vz1s * vz2 - dvy13_r_vz1 * vz2s - dvy12_r_vz2 * vz3s
                + (hn + dvy13_r_vz2s) * vz3)
                * px12
            - (j2 * vz3s + (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s) * vz3) * x3s
            - ((dvy23_r_vz1s * vz2
                - dvy13_r_vz1 * vz2s
                - (2 * dvy12_r_vz1 - dvy12_r_vz2) * vz3s
                - (2 * dvy23_r_vz1s - 3 * dvy13_r_rvz12 + dvy13_r_vz2s) * vz3)
                * x1
                - (dvy23_r_vz1s * vz2 - dvy13_r_vz1 * vz2s - dvy12_r_vz2 * vz3s
                    + (hn + dvy13_r_vz2s) * vz3)
                    * x2)
                * x3
            - ((dvx23_r_vz1s * vz2
                - dvx13_r_vz1 * vz2s
                - dvx12_r_vz1 * vz3s
                - (dvx23_r_vz1s - ia) * vz3)
                * x1
                + (dvx23_r_vz1s * vz2 - dvx13_r_vz1 * vz2s - dvx12_r_vz2 * vz3s
                    + (h8 + dvx13_r_vz2s) * vz3)
                    * x2
                - (2 * dvx23_r_vz1s * vz2
                    - 2 * dvx13_r_vz1 * vz2s
                    - jc * vz3s
                    - (dvx23_r_vz1s - 3 * dvx12_r_rvz12 - dvx13_r_vz2s) * vz3)
                    * x3)
                * y1)
            * y2
        - ((dvy23_r_vz1s * vz2
            - dvy13_r_vz1 * vz2s
            - dvy12_r_vz1 * vz3s
            - (dvy23_r_vz1s - hk) * vz3)
            * x1s
            - (2 * dvy23_r_vz1s * vz2
                - 2 * dvy13_r_vz1 * vz2s
                - (dvy12_r_vz1 + dvy12_r_vz2) * vz3s
                - (dvy23_r_vz1s - 3 * dvy12_r_rvz12 - dvy13_r_vz2s) * vz3)
                * px12
            + (dvy23_r_vz1s * vz2 - dvy13_r_vz1 * vz2s - dvy12_r_vz2 * vz3s
                + (hn + dvy13_r_vz2s) * vz3)
                * x2s
            + ((j2 * vz3s + (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s) * vz3) * x1
                - (j2 * vz3s + (dvy23_r_vz1s - h1 * rvz12 + dvy13_r_vz2s) * vz3) * x2)
                * x3
            - ((dvx23_r_vz1s * vz2
                - dvx13_r_vz1 * vz2s
                - dvx12_r_vz1 * vz3s
                - (dvx23_r_vz1s - ia) * vz3)
                * x1
                - (dvx23_r_vz1s * vz2
                    - dvx13_r_vz1 * vz2s
                    - (2 * dvx12_r_vz1 - dvx12_r_vz2) * vz3s
                    - (2 * dvx23_r_vz1s - 3 * dvx13_r_rvz12 + dvx13_r_vz2s) * vz3)
                    * x2
                - (ji * vz3s + (dvx23_r_vz1s - hj + dvx13_r_vz2s) * vz3) * x3)
                * y1
            + ((dvx23_r_vz1s * vz2 - dvx13_r_vz1 * vz2s
                + (dvx12_r_vz1 - 2 * dvx12_r_vz2) * vz3s
                + (dvx23_r_vz1s - 3 * dvx23_r_rvz12 + 2 * dvx13_r_vz2s) * vz3)
                * x1
                - (dvx23_r_vz1s * vz2 - dvx13_r_vz1 * vz2s - dvx12_r_vz2 * vz3s
                    + (h8 + dvx13_r_vz2s) * vz3)
                    * x2
                - (ji * vz3s + (dvx23_r_vz1s - hj + dvx13_r_vz2s) * vz3) * x3)
                * y2)
            * y3
        + ((i5 * rvz12 - ic * vz2s - ih * vz3s - (i4 * vz1 - 2 * i6 * vz2) * vz3) * px12
            - (i5 * rvz12 - ic * vz2s + i6 * rvz23) * x2s
            - (ih * vz3s + (i4 * vz1 - i6 * vz2) * vz3) * x3s
            - (i2 * rvz12 - i7 * vz2s + i1 * rvz23) * y2s
            - (ib * vz3s + (ke - k9) * vz3) * y3s
            - ((i5 * rvz12 - ic * vz2s - ih * vz3s - (i4 * vz1 - 2 * i6 * vz2) * vz3) * x1
                - (i5 * rvz12 - ic * vz2s + i4 * rvz13 + ih * vz3s) * x2)
                * x3
            - (((hh - dvx13_r_vy2 + hi) * rvz12
                - jg * vz2s
                - kh
                - ((hh - h3 + dvx12_r_vy3) * vz1 - jl * vz2) * vz3)
                * x2
                - ((hh - dvx13_r_vy2 + hi) * rvz12
                    - jg * vz2s
                    - kh
                    - ((hh - h3 + dvx12_r_vy3) * vz1 - jl * vz2) * vz3)
                    * x3)
                * y1
            + (((dvx23_r_vy1 - h4 + hl) * rvz12 + jg * vz2s + kh
                - ((dvx23_r_vy1 - hg + h6) * vz1 + jl * vz2) * vz3)
                * x1
                + ((dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12 - 2 * jg * vz2s + jl * rvz23) * x2
                - ((hh - dvx13_r_vy2 + hi) * rvz12 - jg * vz2s - (dvx23_r_vy1 - hg + h6) * rvz13
                    + kh)
                    * x3
                + (i2 * rvz12 - i7 * vz2s - ib * vz3s - (ke - 2 * k9) * vz3) * y1)
                * y2
            - (((dvx23_r_vy1 - h4 + hl) * rvz12 + jg * vz2s + kh
                - ((dvx23_r_vy1 - hg + h6) * vz1 + jl * vz2) * vz3)
                * x1
                - ((dvx23_r_vy1 - h4 + hl) * rvz12 + jg * vz2s
                    - (hh - h3 + dvx12_r_vy3) * rvz13
                    - kh)
                    * x2
                - (2 * kh + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2) * vz3) * x3
                + (i2 * rvz12 - i7 * vz2s - ib * vz3s - (ke - 2 * k9) * vz3) * y1
                - (i2 * rvz12 - i7 * vz2s + i3 * rvz13 + ib * vz3s) * y2)
                * y3)
            * z1
        - ((ii * vz1s - i5 * rvz12 + i4 * rvz13) * x1s
            - (ii * vz1s - i5 * rvz12 + ih * vz3s + (2 * i4 * vz1 - i6 * vz2) * vz3) * px12
            - (ih * vz3s + (i4 * vz1 - i6 * vz2) * vz3) * x3s
            + (i9 * vz1s - i2 * rvz12 + i3 * rvz13) * y1s
            - (ib * vz3s + (ke - k9) * vz3) * y3s
            + (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z1s
            - ((ii * vz1s - i5 * rvz12 + i6 * rvz23 - ih * vz3s) * x1
                - (ii * vz1s - i5 * rvz12 + ih * vz3s + (2 * i4 * vz1 - i6 * vz2) * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * rvz13)
                * x1
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (hh - dvx13_r_vy2 + hi) * rvz12
                    + kh
                    + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (h7 + dvx13_r_vy2 - h6) * vz2)
                        * vz3)
                    * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                    + (dvx23_r_vy1 - h4 + hl) * rvz12
                    + (h7 + dvx13_r_vy2 - h6) * rvz23
                    - kh)
                    * x3)
                * y1
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                + (dvx23_r_vy1 - h4 + hl) * rvz12
                + kh
                + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2) * vz3)
                * x1
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                    + (dvx23_r_vy1 - h4 + hl) * rvz12
                    + kh
                    + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2)
                        * vz3)
                    * x3
                - (i9 * vz1s - i2 * rvz12 + ib * vz3s + (2 * ke - k9) * vz3) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (hh - dvx13_r_vy2 + hi) * rvz12
                + (h2 - h4 + dvx12_r_vy3) * rvz23
                - kh)
                * x1
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (hh - dvx13_r_vy2 + hi) * rvz12
                    + kh
                    + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - (h7 + dvx13_r_vy2 - h6) * vz2)
                        * vz3)
                    * x2
                + (2 * kh + ((dvx23_r_vy1 + hb - dvx12_r_vy3) * vz1 - jl * vz2) * vz3) * x3
                - (i9 * vz1s - i2 * rvz12 + i1 * rvz23 - ib * vz3s) * y1
                + (i9 * vz1s - i2 * rvz12 + ib * vz3s + (2 * ke - k9) * vz3) * y2)
                * y3
            + ((2 * (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2
                + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                * x1
                - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    - 2 * (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s
                        + (h7 + dvx13_r_vy2) * vy3)
                        * vz2
                    + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                    * x2
                - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    + (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3)
                        * vz2)
                    * x3
                - (2 * (i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2
                    + (k3 - k2 + ib * vy3) * vz3)
                    * y1
                + ((i9 * vy1 - k5 + k6) * vz1 - 2 * (k1 - i7 * vy2 + k4) * vz2
                    + (k3 - k2 + ib * vy3) * vz3)
                    * y2
                + ((i9 * vy1 - k5 + k6) * vz1 + (k1 - i7 * vy2 + k4) * vz2) * y3)
                * z1)
            * z2
        + ((ii * vz1s - i5 * rvz12 + i4 * rvz13) * x1s
            - (ii * vz1s - ic * vz2s + (i4 * vz1 + i6 * vz2) * vz3) * px12
            + (i5 * rvz12 - ic * vz2s + i6 * rvz23) * x2s
            + (i9 * vz1s - i2 * rvz12 + i3 * rvz13) * y1s
            + (i2 * rvz12 - i7 * vz2s + i1 * rvz23) * y2s
            + (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z1s
            - (i9 * vy1s - 2 * i2 * rvy12 + i7 * vy2s + ib * vy3s + 2 * (k3 - k2) * vy3) * z2s
            - ((ii * vz1s - 2 * i5 * rvz12 + ic * vz2s + (i4 * vz1 - i6 * vz2) * vz3) * x1
                - (ii * vz1s - 2 * i5 * rvz12 + ic * vz2s + (i4 * vz1 - i6 * vz2) * vz3) * x2)
                * x3
            - ((2 * (dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                + (dvx23_r_vy1 + hb - dvx12_r_vy3) * rvz13)
                * x1
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s
                    - jg * vz2s
                    - ((dvx23_r_vy1 - hg + h6) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2) * vz3)
                    * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                    + jg * vz2s
                    + ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2) * vz3)
                    * x3)
                * y1
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - jg * vz2s
                + ((hh - h3 + dvx12_r_vy3) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2) * vz3)
                * x1
                - ((dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12 - 2 * jg * vz2s + jl * rvz23) * x2
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                    + jg * vz2s
                    + ((hh - h3 + dvx12_r_vy3) * vz1 - (h2 - h4 + dvx12_r_vy3) * vz2) * vz3)
                    * x3
                - (i9 * vz1s - i7 * vz2s + (ke + k9) * vz3) * y1)
                * y2
            + (((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                + jg * vz2s
                - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2) * vz3)
                * x1
                - ((dvx23_r_vy2 - dvx23_r_vy3) * vz1s - (dvx23_r_vy1 + dvx13_r_vy2 - he) * rvz12
                    + jg * vz2s
                    - ((dvx23_r_vy1 - hg + h6) * vz1 + (h7 + dvx13_r_vy2 - h6) * vz2) * vz3)
                    * x2
                - (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + (ke - k9) * vz3) * y1
                + (i9 * vz1s - 2 * i2 * rvz12 + i7 * vz2s + (ke - k9) * vz3) * y2)
                * y3
            + ((2 * (hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2
                + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                * x1
                - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    - (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                    * x2
                - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3)
                        * vz2
                    + 2 * (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                    * x3
                - (2 * (i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2
                    + (k3 - k2 + ib * vy3) * vz3)
                    * y1
                + ((i9 * vy1 - k5 + k6) * vz1 - (k3 - k2 + ib * vy3) * vz3) * y2
                + ((i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2
                    + 2 * (k3 - k2 + ib * vy3) * vz3)
                    * y3)
                * z1
            - (((dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3) * vz2
                + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                * x1
                + ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    - 2 * (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s
                        + (h7 + dvx13_r_vy2) * vy3)
                        * vz2
                    + (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                    * x2
                - ((hf - dvx13_r_vy2s - dvx12_r_vy3s - (dvx23_r_vy1 - hg) * vy3) * vz1
                    - (dvx23_r_vy1s - dvx13_r_rvy12 - dvx12_r_vy3s + (h7 + dvx13_r_vy2) * vy3)
                        * vz2
                    + 2 * (dvx23_r_vy1s - hd + dvx13_r_vy2s + kf) * vz3)
                    * x3
                - ((k1 - i7 * vy2 + k4) * vz2 + (k3 - k2 + ib * vy3) * vz3) * y1
                - ((i9 * vy1 - k5 + k6) * vz1 - 2 * (k1 - i7 * vy2 + k4) * vz2
                    + (k3 - k2 + ib * vy3) * vz3)
                    * y2
                + ((i9 * vy1 - k5 + k6) * vz1 - (k1 - i7 * vy2 + k4) * vz2
                    + 2 * (k3 - k2 + ib * vy3) * vz3)
                    * y3)
                * z2)
            * z3;

    let c = c_numerator / c_denominator;

    let t_numerator = (dvz23 * x2 - dvz23 * x3) * y1 - (dvz23 * x1 - dvz23 * x3) * y2
        + (dvz23 * x1 - dvz23 * x2) * y3
        - (dvy23 * x2 - dvy23 * x3 - dvx23 * y2 + dvx23 * y3) * z1
        + (dvy23 * x1 - dvy23 * x3 - dvx23 * y1 + dvx23 * y3) * z2
        - (dvy23 * x1 - dvy23 * x2 - dvx23 * y1 + dvx23 * y2) * z3;

    let t = t_numerator / t_denominator;

    let u_numerator = (dvz13 * x2 - dvz13 * x3) * y1 - (dvz13 * x1 - dvz13 * x3) * y2
        + (dvz13 * x1 - dvz13 * x2) * y3
        - (dvy13 * x2 - dvy13 * x3 - dvx13 * y2 + dvx13 * y3) * z1
        + (dvy13 * x1 - dvy13 * x3 - dvx13 * y1 + dvx13 * y3) * z2
        - (dvy13 * x1 - dvy13 * x2 - dvx13 * y1 + dvx13 * y2) * z3;

    let u = u_numerator / u_denominator;

    let v_numerator = (dvz12 * x2 - dvz12 * x3) * y1 - (dvz12 * x1 - dvz12 * x3) * y2
        + (dvz12 * x1 - dvz12 * x2) * y3
        - (dvy12 * x2 - dvy12 * x3 - dvx12 * y2 + dvx12 * y3) * z1
        + (dvy12 * x1 - dvy12 * x3 - dvx12 * y1 + dvx12 * y3) * z2
        - (dvy12 * x1 - dvy12 * x2 - dvx12 * y1 + dvx12 * y2) * z3;

    let v = v_numerator / v_denominator;

    Some((Point { x, y, z }, Point { x: a, y: b, z: c }, t, u, v))
}
