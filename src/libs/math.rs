use std::ops::Sub;

pub fn absolute_difference<T>(x: T, y: T) -> T
where
    T: Sub<Output = T> + PartialOrd,
{
    if x > y {
        x - y
    } else {
        y - x
    }
}
