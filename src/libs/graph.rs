const CARDINAL_DIRECTIONS: [PointDirection; 4] = [
    PointDirection::Down,
    PointDirection::Left,
    PointDirection::Right,
    PointDirection::Up,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BoundedPoint {
    pub x: usize,
    pub y: usize,
    pub max_x: usize,
    pub max_y: usize,
}

impl BoundedPoint {
    pub fn into_iter_direction(self, point_direction: PointDirection) -> BoundedPointIntoIterator {
        BoundedPointIntoIterator {
            point: self,
            direction: point_direction,
        }
    }

    pub fn into_iter_cardinal_adjacent(self) -> CardinalAdjacentIterator {
        CardinalAdjacentIterator {
            point: self,
            index: 0,
        }
    }

    pub fn get_adjacent(self, point_direction: &PointDirection) -> Option<BoundedPoint> {
        match point_direction {
            PointDirection::Up => {
                if self.y > 0 {
                    Some(BoundedPoint {
                        y: self.y - 1,
                        ..self
                    })
                } else {
                    None
                }
            }
            PointDirection::Down => {
                if self.y < self.max_y {
                    Some(BoundedPoint {
                        y: self.y + 1,
                        ..self
                    })
                } else {
                    None
                }
            }
            PointDirection::Left => {
                if self.x > 0 {
                    Some(BoundedPoint {
                        x: self.x - 1,
                        ..self
                    })
                } else {
                    None
                }
            }
            PointDirection::Right => {
                if self.x < self.max_x {
                    Some(BoundedPoint {
                        x: self.x + 1,
                        ..self
                    })
                } else {
                    None
                }
            }
        }
    }

    pub fn get_adjacent_wrapping(self, point_direction: &PointDirection) -> BoundedPoint {
        match point_direction {
            PointDirection::Up => {
                if self.y > 0 {
                    BoundedPoint {
                        y: self.y - 1,
                        ..self
                    }
                } else {
                    BoundedPoint {
                        y: self.max_y,
                        ..self
                    }
                }
            }
            PointDirection::Down => {
                if self.y < self.max_y {
                    BoundedPoint {
                        y: self.y + 1,
                        ..self
                    }
                } else {
                    BoundedPoint { y: 0, ..self }
                }
            }
            PointDirection::Left => {
                if self.x > 0 {
                    BoundedPoint {
                        x: self.x - 1,
                        ..self
                    }
                } else {
                    BoundedPoint {
                        x: self.max_x,
                        ..self
                    }
                }
            }
            PointDirection::Right => {
                if self.x < self.max_x {
                    BoundedPoint {
                        x: self.x + 1,
                        ..self
                    }
                } else {
                    BoundedPoint { x: 0, ..self }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RotationDegrees {
    Zero,
    Ninety,
    OneHundredEighty,
    TwoHundredSeventy,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum PointDirection {
    Up,
    Down,
    Left,
    Right,
}

impl PointDirection {
    pub fn get_rotation(&self, other: &PointDirection) -> RotationDegrees {
        match (self, other) {
            (p1, p2) if p1 == p2 => RotationDegrees::Zero,
            (p1, p2) if &p1.get_opposite() == p2 => RotationDegrees::OneHundredEighty,
            (p1, p2) if &p1.get_counter_clockwise() == p2 => RotationDegrees::TwoHundredSeventy,
            _ => RotationDegrees::Ninety,
        }
    }

    pub fn get_opposite(&self) -> PointDirection {
        match self {
            PointDirection::Up => PointDirection::Down,
            PointDirection::Down => PointDirection::Up,
            PointDirection::Left => PointDirection::Right,
            PointDirection::Right => PointDirection::Left,
        }
    }

    pub fn get_clockwise(&self) -> PointDirection {
        match self {
            PointDirection::Up => PointDirection::Right,
            PointDirection::Down => PointDirection::Left,
            PointDirection::Left => PointDirection::Up,
            PointDirection::Right => PointDirection::Down,
        }
    }

    pub fn get_counter_clockwise(&self) -> PointDirection {
        match self {
            PointDirection::Up => PointDirection::Left,
            PointDirection::Down => PointDirection::Right,
            PointDirection::Left => PointDirection::Down,
            PointDirection::Right => PointDirection::Up,
        }
    }
}

pub struct BoundedPointIntoIterator {
    point: BoundedPoint,
    direction: PointDirection,
}

impl Iterator for BoundedPointIntoIterator {
    type Item = BoundedPoint;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.point.get_adjacent(&self.direction);
        result.iter().for_each(|point| self.point = *point);
        result
    }
}

pub struct CardinalAdjacentIterator {
    point: BoundedPoint,
    index: usize,
}

impl Iterator for CardinalAdjacentIterator {
    type Item = BoundedPoint;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= CARDINAL_DIRECTIONS.len() {
            return None;
        }
        let mut result = self.point.get_adjacent(&CARDINAL_DIRECTIONS[self.index]);
        self.index += 1;

        result = match result {
            None => self.next(),
            _ => result,
        };
        result
    }
}
