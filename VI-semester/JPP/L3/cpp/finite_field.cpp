#include <stdbool.h>
#include <math.h>
#include <vector>
#include <unordered_set>
#include <iostream>
#include <random>
#include <stdexcept>
#include <assert.h>
using namespace std;

class FiniteFieldElement {
    private:
        long value;
        long characteristic;
        bool isPrime(long number) const {
            if (number <= 1) {
                return false;
            }
            if (number == 2) {
                return true;
            }
            if (number % 2 == 0) {
                return false;
            }
            for (long i = 3; i <= sqrt(number); i += 2) {
                if (number % i == 0) {
                    return false;
                }
            }
            return true;
        }

        long extended_gcd(long a, long b, long *x, long *y) const {
            if (a == 0) {
                *x = 0;
                *y = 1;
                return b;
            }

            long x1, y1;
            long gcd = extended_gcd(b % a, a, &x1, &y1);

            *x = y1 - (b / a) * x1;
            *y = x1;

            return gcd;
        }

        long invert_mod(long a, long m) const {
            long x, y;
            long gcd = extended_gcd(a, m, &x, &y);

            if (gcd != 1) {
                // Modular inverse does not exist
                //printf("Nie istnieje liczba odwrotna.\n");
                return -1;
            }
            long inverse = (x % m + m) % m;
        
            return inverse;
        }



    public:
        FiniteFieldElement() {};
        FiniteFieldElement(long number, long val) : value((val % number + number) % number), characteristic(isPrime(number) ? number : throw invalid_argument("The received number in the constructor is NOT a prime number!")) {}
        
        long getCharacteristic() {
            return characteristic;
        }

        void setValue(long value) {
            this->value = value;
        }

        long getValue() {
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
            
            return FiniteFieldElement(a.characteristic, (a.value + b.value) % a.characteristic);
        }
        // 2. "-"
        friend const FiniteFieldElement operator-(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('-') - Finite field elements have different characteristics!");
            }
            long result = (a.value - b.value) % a.characteristic;
            if (result < 0) {
                result += a.characteristic;
            }
            
            return FiniteFieldElement(a.characteristic, result % a.characteristic);
        }
        // 3. "*"
        friend const FiniteFieldElement operator*(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('*') - Finite field elements have different characteristics!");
            }
            
            return FiniteFieldElement(a.characteristic, (a.value * b.value) % a.characteristic);
        }
        // 4. "/"
        friend const FiniteFieldElement operator/(FiniteFieldElement const& a, FiniteFieldElement const& b) {
            if (a.characteristic != b.characteristic) {
                throw invalid_argument("('/') - Finite field elements have different characteristics!");
            }
            long inv = a.invert_mod(b.value, a.characteristic);
            if (inv == -1) {
                throw invalid_argument("('/') - Modular inverse does not exist!");
            }
    
            FiniteFieldElement inverse(a.characteristic, inv);
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

        friend const ostream& operator<<(ostream& os, FiniteFieldElement const& gf) {
            return os << gf.value;
        }
};

template <class T>
class DHSetup {
    private:
        T generator;
        long characteristic;
        long value;
        vector<long> find_all_primes(long p) const {

            vector<bool> all_primes(p + 1, true);
            all_primes[0] = all_primes[1] = false;
            vector<long> primes_dividing_characteristic;
            
            for (long i = 2; i <= sqrt(p); ++i) {
                if (all_primes[i]) {
                    for (long j = i * i; j <= p; j += i) {
                        all_primes[j] = false;
                    }
                }
                if (all_primes[i] && (p - 1) % i == 0) {
                    primes_dividing_characteristic.push_back(i);
                }
            }

            return primes_dividing_characteristic;
        }

        bool is_generator(long number, long p, const vector<long>& primes_dividing_characteristic) const {
            T a(this->characteristic, number);

            for (long prime : primes_dividing_characteristic) {
                T result = this->power(a, (p - 1) / prime);

                if (result.getValue() == 1) {
                    return false;
                }    
            }

            return true;
        }

    public:
        DHSetup(T field) {
            random_device rd;
            mt19937 rng(rd());
            this->characteristic = field.getCharacteristic();
            this->value = field.getValue();
            uniform_int_distribution<long> dist(2, this->characteristic);
            
            unordered_set<long> chosen_numbers;
            vector<long> primes_dividing_characteristic = find_all_primes(this->characteristic);
            while (chosen_numbers.size() != this->characteristic - 2) {
                long random_number = dist(rng);
                if (chosen_numbers.count(random_number))
                    continue;
                chosen_numbers.insert(random_number);
                if (is_generator(random_number, this->characteristic, primes_dividing_characteristic)) {
                    //generator = field.copy();
                    generator = T(this->characteristic, random_number);
                    //generator->setValue(random_number);
                    break;
                }
            }
        }

        T getGenerator() {
            return generator;
        }

        T power(T a, long b) const {
            T result = a;

            while (b > 0) {
                if (b % 2 == 1) {
                    result = result * a;
                }
                a = a * a;
                b /= 2;
            }

            return result;
        }
};


template <class T>
class User {
    private:
        long secret;
        T cyphering_key;
        bool key_is_set = false;
        DHSetup<T>* dhSetup;
    
    public:
        User(DHSetup<T>* dhSetup) {
            secret = rand();
            while (secret <= 0) {
                secret = rand();
            }
            this->dhSetup = dhSetup;
        }

        T getPublicKey() {
            return dhSetup->power(dhSetup->getGenerator(), secret);
        }

        void setKey(T a) {
            cyphering_key = dhSetup->power(a, secret);
            key_is_set = true;
        }

        T encryptKey(T m) {
            if (!key_is_set)
                throw runtime_error("Key is not set!");
            return m * cyphering_key;
        }

        T decryptKey(T c) {
            if (!key_is_set)
                throw runtime_error("Key is not set!");
            return c / cyphering_key;
        }
};

int main() {
    long p = 1234567891;
    FiniteFieldElement x(p, 2);
    DHSetup dh_setup(x);
    cout << "Generator: " << dh_setup.getGenerator().getValue() << endl;
    User user(&dh_setup);
    cout << "Public key: " << user.getPublicKey().getValue() << endl;
    FiniteFieldElement a(dh_setup.getGenerator().getCharacteristic(), 17);
    user.setKey(a);

    FiniteFieldElement message(dh_setup.getGenerator().getCharacteristic(), 84215);
    cout << "Message: " << message.getValue() << endl;

    FiniteFieldElement encrypted = user.encryptKey(message);
    cout << "Encrypted: " << encrypted.getValue() << endl;

    FiniteFieldElement decrypted = user.decryptKey(encrypted);
    cout << "Decrypted: " << decrypted.getValue() << endl;

    if (message.getValue() == decrypted.getValue()) {
        cout << "SUCCESS!" << endl;
    } else {
        cout << "FAIL!" << endl;
    }

    return 0;
}