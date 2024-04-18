#[repr(C)]
pub struct Solution {
    pub x: i64,
    pub y: i64,
}

#[no_mangle]
pub extern "C" fn factorial(n: u64) -> u64 {
    (1..=n).product()
}

#[no_mangle]
pub extern "C" fn gcd(a: u64, b: u64) -> u64 {
    let mut a2 = a;
    let mut b2 = b;

    while b2 != 0 {
        let t = b2;
        b2 = a2 % b2;
        a2 = t;
    }

    a2
}

#[no_mangle]
pub extern "C" fn diophantine_equation(mut a: i64, mut b: i64, c: i64) -> Solution {
    let mut x1 = 1;
    let mut y1 = 0;
    let mut x2 = 0;
    let mut y2 = 1;

    while b != 0 {
        let q = a / b;
        let r = a % b;

        let temp = x1 - q * x2;
        x1 = x2;
        x2 = temp;

        let temp = y1 - q * y2;
        y1 = y2;
        y2 = temp;

        a = b;
        b = r;
    }

    if c % a == 0 {
        let x = x1 * (c / a);
        let y = y1 * (c / a);
        Solution {
            x,
            y,
        }
    }
    else {
        Solution {
            x: 0,
            y: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn factorial_test() {
        let result = factorial(5);
        assert_eq!(result, 120);

        let result = factorial(7);
        assert_eq!(result, 5040);

        let result = factorial(12);
        assert_eq!(result, 479001600);
    }

    #[test]
    fn gcd_test() {
        let result = gcd(12, 15);
        assert_eq!(result, 3);

        let result = gcd(15, 25);
        assert_eq!(result, 5);

        let result = gcd(22, 55);
        assert_eq!(result, 11);

        let result = gcd(0, 15);
        assert_eq!(result, 15);

        let result = gcd(893724,2947228);
        assert_eq!(result, 4);
    }

    #[test]
    fn diophantine_test() {
        let s = diophantine_equation(3, 5, 10);
        assert_eq!(3 * s.x + 5 * s.y, 10);

        let s = diophantine_equation(3, 5, 11);
        assert_eq!(3 * s.x + 5 * s.y, 11);

        let s = diophantine_equation(3, 5, 12);
        assert_eq!(3 * s.x + 5 * s.y, 12);

        let s = diophantine_equation(3, 7, 22);
        assert_eq!(3 * s.x + 7 * s.y, 22);

        let s = diophantine_equation(2718, 3872, 2);
        assert_eq!(2718 * s.x + 3872 * s.y, 2);
    }
}