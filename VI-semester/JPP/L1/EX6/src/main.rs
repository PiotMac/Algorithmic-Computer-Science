#[repr(C)]
pub struct Solution {
    pub x: i64,
    pub y: i64,
}

extern "C" {
    pub fn factorial(n: u64) -> u64;
    pub fn gcd(a: u64, b: u64) -> u64;
    pub fn diophantine_equation(a: i64, b: i64, c: i64) -> Solution;
}

fn main() {
    println!("Factorial of 5: {}", unsafe { factorial(5) });
    println!("GCD of 12 and 15: {}", unsafe { gcd(12, 15) });
    let solution = unsafe { diophantine_equation(3, 6, 18) };
    println!(
        "Diophantine solution: x = {}, y = {}",
        solution.x, solution.y
    );
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_factorial() {
        unsafe {
            assert_eq!(factorial(0), 1);
            assert_eq!(factorial(1), 1);
            assert_eq!(factorial(2), 2);
            assert_eq!(factorial(3), 6);
            assert_eq!(factorial(4), 24);
            assert_eq!(factorial(5), 120);
        }
    }

    #[test]
    fn test_gcd() {
        unsafe {
            assert_eq!(gcd(0, 0), 0);
            assert_eq!(gcd(0, 1), 1);
            assert_eq!(gcd(1, 0), 1);
            assert_eq!(gcd(1, 1), 1);
            assert_eq!(gcd(2, 3), 1);
            assert_eq!(gcd(3, 2), 1);
            assert_eq!(gcd(4, 6), 2);
            assert_eq!(gcd(6, 4), 2);
            assert_eq!(gcd(6, 9), 3);
            assert_eq!(gcd(9, 6), 3);
            assert_eq!(gcd(12, 15), 3);
            assert_eq!(gcd(15, 12), 3);
            assert_eq!(gcd(15, 25), 5);
            assert_eq!(gcd(25, 15), 5);
        }
    }

    #[test]
    fn test_diophantine() {
        unsafe {
            for c in 5..25 {
                let s = diophantine_equation(2, 3, c);
                assert_eq!(2 * s.x + 3 * s.y, c);

                let s = diophantine_equation(5, 11, c);
                assert_eq!(5 * s.x + 11 * s.y, c);
            }
        }
    }
}