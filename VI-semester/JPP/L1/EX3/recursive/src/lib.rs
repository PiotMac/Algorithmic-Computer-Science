#[repr(C)]
pub struct Solution {
    pub x: i64,
    pub y: i64,
}

#[no_mangle]
pub extern "C" fn factorial(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

#[no_mangle]
pub extern "C" fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

#[no_mangle]
pub extern "C" fn diophantine_equation(a: i64, b: i64, c: i64) -> Solution {
    if c == 0 {
        Solution { x: 0, y: 0 }
    } else if a == 0 && b == 0 {
        Solution { x: 0, y: 0 }
    } else if a == 0 {
        Solution { x: 0, y: c / b }
    } else if b == 0 {
        Solution { x: c / a, y: 0 }
    } else {
        let s = diophantine_equation(b, a % b, c);
        let x = s.x;
        let y = s.y;
        Solution { x: y, y: x - (a / b) * y }
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