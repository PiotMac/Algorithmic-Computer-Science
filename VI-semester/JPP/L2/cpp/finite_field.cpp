#include <stdbool.h>
#include <math.h>
#include <iostream>
#include <stdexcept>
#include <assert.h>
using namespace std;

class FiniteFieldElement {
    private:
        int value;
        int characteristic;
        bool isPrime(int number) const {
            if (number <= 1) {
                return false;
            }
            if (number == 2) {
                return true;
            }
            if (number % 2 == 0) {
                return false;
            }
            for (int i = 3; i <= sqrt(number); i += 2) {
                if (number % i == 0) {
                    return false;
                }
            }
            return true;
        }

        int extended_gcd(int a, int b, int *x, int *y) const {
            if (a == 0) {
                *x = 0;
                *y = 1;
                return b;
            }

            int x1, y1;
            int gcd = extended_gcd(b % a, a, &x1, &y1);

            *x = y1 - (b / a) * x1;
            *y = x1;

            return gcd;
        }

        int invert_mod(int a, int m) const {
            int x, y;
            int gcd = extended_gcd(a, m, &x, &y);

            if (gcd != 1) {
                // Modular inverse does not exist
                //printf("Nie istnieje liczba odwrotna.\n");
                return -1;
            }
            int inverse = (x % m + m) % m;
        
            return inverse;
        }



    public:
        FiniteFieldElement() {};
        FiniteFieldElement(int val, int number) : value((val % number + number) % number), characteristic(isPrime(number) ? number : throw invalid_argument("The received number in the constructor is NOT a prime number!")) {}
        
        int getCharacteristic() {
            return characteristic;
        }

        int getValue() {
            return value;
        }

        // ################### ASSIGNING OPERATORS ###################
        // 1. "="
        FiniteFieldElement operator=(FiniteFieldElement const& other) {
            if (this != &other) {
                //if (characteristic != other.characteristic) {
                //    throw invalid_argument("Finite field elements have different characteristics");
                //}
                value = other.value;
                characteristic = other.characteristic;
            }
            return *this;
        }

        // 2. "+="
        FiniteFieldElement operator+=(FiniteFieldElement const& other) {
            if (characteristic != other.characteristic) {
                throw invalid_argument("Finite field elements have different characteristics");
            }
            value = value + other.value;

            return *this;
        }

        // 3. "-="
        FiniteFieldElement operator-=(FiniteFieldElement const& other) {
            if (characteristic != other.characteristic) {
                throw invalid_argument("Finite field elements have different characteristics");
            }
            value = value - other.value;
            
            return *this;
        }

        // 4. "*="
        FiniteFieldElement operator*=(FiniteFieldElement const& other) {
            if (characteristic != other.characteristic) {
                throw invalid_argument("Finite field elements have different characteristics");
            }
            value = value * other.value;
            
            return *this;
        }

        // 5. "/="
        FiniteFieldElement operator/=(FiniteFieldElement const& other) {
            if (characteristic != other.characteristic) {
                throw invalid_argument("Finite field elements have different characteristics");
            }
            value = value / other.value;
            
            return *this;
        }

        // ################### ARITHMETIC OPERATORS ###################
        // 1. "+"
        friend const FiniteFieldElement operator+(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('+') - Finite field elements have different characteristics!");
            }
            
            return FiniteFieldElement((a.value + b.value) % a.characteristic, a.characteristic);
        }
        // 2. "-"
        friend const FiniteFieldElement operator-(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('-') - Finite field elements have different characteristics!");
            }
            int result = (a.value - b.value) % a.characteristic;
            if (result < 0) {
                result += a.characteristic;
            }
            
            return FiniteFieldElement(result % a.characteristic, a.characteristic);
        }
        // 3. "*"
        friend const FiniteFieldElement operator*(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('*') - Finite field elements have different characteristics!");
            }
            int first = a.value;
            int second = b.value;
            int result = first;
            for(int i = 1; i < second; i++) {
                result += first;
                result = ((result % a.characteristic) + a.characteristic) % a.characteristic;
            }
            
            return FiniteFieldElement(result, a.characteristic);
        }
        // 4. "/"
        friend const FiniteFieldElement operator/(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('/') - Finite field elements have different characteristics!");
            }
            int inv = a.invert_mod(b.value, a.characteristic);
            if (inv == -1) {
                throw invalid_argument("('/') - Modular inverse does not exist!");
            }
    
            FiniteFieldElement inverse(inv, a.characteristic);
            FiniteFieldElement result = a * inverse;
            
            return result;
        }

        // ################### COMPARING OPERATORS ###################
        bool operator==(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('==') - Finite field elements have different characteristics!");
            }
            return value == other.value;
        }

        bool operator!=(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('!=') - Finite field elements have different characteristics!");
            }
            return value != other.value;
        }

        bool operator<(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('<') - Finite field elements have different characteristics!");
            }
            return value < other.value;
        }

        bool operator>(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('>') - Finite field elements have different characteristics!");
            }
            return value > other.value;
        }

        bool operator<=(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('<=') - Finite field elements have different characteristics!");
            }
            return value <= other.value;
        }

        bool operator>=(FiniteFieldElement const& other) const {
            if (characteristic != other.characteristic) {
                throw invalid_argument("('>=') - Finite field elements have different characteristics!");
            }
            return value >= other.value;
        }

        friend const std::ostream& operator<<(std::ostream& os, FiniteFieldElement const& gf) {
            return os << gf.value;
        }
};

int main() {
    FiniteFieldElement a(2, 1234577);
    FiniteFieldElement b(3, 1234577);
    FiniteFieldElement c(4, 1234577);
    FiniteFieldElement d(5, 1234577);
    FiniteFieldElement y(-580978, 1234577);
    FiniteFieldElement x(1, 1234577);
    FiniteFieldElement result;

    try {
        // 2 + 3 * (4 - 5) = 1234576
        result = a + b * (c - d);
        assert(result.getValue() == 1234576);

        // 2 - 3 - 2 = 1234574
        result = a - b - a;
        assert(result.getValue() == 1234574);

        // 269164 / 123456 = 567890
        FiniteFieldElement aa(269164, 1234577);
        FiniteFieldElement bb(123456, 1234577);
        result = aa / bb;
        assert(result.getValue() == 567890);

        // 1 / -580978 = 123456
        result = x / y;
        assert(result.getValue() == 123456);

        // 123456789 = 1233666
        FiniteFieldElement test(123456789, 1234577);
        assert(test.getValue() == 1233666);

        // -1234567 = 10
        FiniteFieldElement test1(-1234567, 1234577);
        assert(test1.getValue() == 10);

    } catch (const invalid_argument& e) {
        cerr << "Error: " << e.what() << endl;
    }

    return 0;
}