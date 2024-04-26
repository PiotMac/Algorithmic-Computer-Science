import secrets
import math

class FiniteField:
    def __init__(self, prime_number, value):
        if self.is_prime(prime_number):
            self.characteristic = prime_number
        else:
            raise Exception("The received number in the constructor is NOT a prime number!")
        self.value = (value % self.characteristic + self.characteristic) % self.characteristic

    def is_prime(self, number):
        if number <= 1:
            return False
        if number == 2:
            return True
        if number % 2 == 0:
            return False
        for i in range(3, int(number ** 0.5) + 1, 2):
            if number % i == 0:
                return False
        return True

    def get_characteristic(self):
        return self.characteristic

    def set_value(self, value):
        self.value = value

    def get_value(self):
        return self.value

    # COMPARING OPERATORS
    def __eq__(self, other):
        return self.value == other.value

    def __ne__(self, other):
        return self.value != other.value

    def __le__(self, other):
        return self.value <= other.value

    def __ge__(self, other):
        return self.value >= other.value

    def __lt__(self, other):
        return self.value < other.value

    def __gt__(self, other):
        return self.value > other.value

    # ARITHMETIC OPERATORS
    def __add__(self, other):
        return FiniteField(self.characteristic, (self.value + other.value) % self.characteristic)

    def __sub__(self, other):
        result = (self.value - other.value) % self.characteristic
        if result < 0:
            result += self.characteristic
        return FiniteField(self.characteristic, result)

    def __mul__(self, other):
        return FiniteField(self.characteristic, (self.value * other.value) % self.characteristic)

    def extended_gcd(self, a, b):
        if a == 0:
            return [0, 1, b]

        gcd = self.extended_gcd(b % a, a)
        x, y = gcd[1] - (b // a) * gcd[0], gcd[0]

        return [x, y, gcd[2]]

    def mod_invert(self, a, m):
        gcd = self.extended_gcd(a, m)
        if gcd[2] != 1:
            # Modular inverse does not exist
            return -1
        else:
            return self.__mod__(gcd[0], m)

    def __truediv__(self, other):
        inv = self.mod_invert(other.value, other.characteristic)
        if inv == -1:
            raise ArithmeticError("Modular inverse does not exist!")
        inverse = FiniteField(self.characteristic, inv)
        return self.__mul__(inverse)

    def __mod__(self, left, right):
        return (left % right + self.characteristic) % self.characteristic

    # ASSIGNING OPERATORS
    def assign(self, other):
        return FiniteField(self.characteristic, other.value)

    def __iadd__(self, other):
        return FiniteField(self.characteristic, self.__add__(other).value)

    def __isub__(self, other):
        return FiniteField(self.characteristic, self.__sub__(other).value)

    def __imul__(self, other):
        return FiniteField(self.characteristic, self.__mul__(other).value)

    def __idiv__(self, other):
        return FiniteField(self.characteristic, self.__truediv__(other).value)

    def copy(self):
        try:
            return FiniteField(self.characteristic, self.value)
        except Exception as e:
            raise RuntimeError(e)


class DHSetup:
    def __init__(self, field):
        self.field = field
        self.generator = None
        primes_dividing_characteristic = self.find_all_primes(field.get_characteristic())

        chosen_numbers = set()
        while len(chosen_numbers) != field.get_characteristic() - 2:
            random_number = secrets.randbelow(field.get_characteristic() - 2) + 2
            if random_number in chosen_numbers:
                continue
            chosen_numbers.add(random_number)
            if self.is_generator(random_number, field.get_characteristic(), primes_dividing_characteristic):
                self.generator = field.copy()
                self.generator.set_value(random_number)
                break

    def find_all_primes(self, p):
        all_primes = bytearray(b'\x01' * (p + 1))
        #all_primes = [True] * (p + 1)
        all_primes[0] = all_primes[1] = False

        primes_dividing_characteristic = []

        for i in range(2, int(math.sqrt(p)) + 1):
            if all_primes[i]:
                for j in range(i * i, p + 1, i):
                    all_primes[j] = False
            if all_primes[i] and (p - 1) % i == 0:
                primes_dividing_characteristic.append(i)

        return primes_dividing_characteristic

    def is_generator(self, number, p, primes_dividing_characteristic):
        a = self.field.copy()
        a.set_value(number)

        for prime in primes_dividing_characteristic:
            result = self.power(a, (p - 1) // prime)
            if result.get_value() == 1:
                return False

        return True

    def get_generator(self):
        return self.generator

    def power(self, a, b):
        result = self.field.copy()
        result.set_value(1)

        a_copy = a.copy()

        while b > 0:
            if b % 2 == 1:
                result *= a_copy
            a_copy *= a_copy
            b //= 2

        return result



class User:
    def __init__(self, dh_setup):
        self.secret = self.generate_secret()
        self.cyphering_key = None
        self.key_is_set = False
        self.dh_setup = dh_setup

    def generate_secret(self):
        found_secret = secrets.randbits(64)
        while found_secret <= 0:
            found_secret = secrets.randbits(64)
        return found_secret

    def get_public_key(self):
        return self.dh_setup.power(self.dh_setup.get_generator(), self.secret)

    def set_key(self, a):
        self.cyphering_key = self.dh_setup.power(a, self.secret)
        self.key_is_set = True

    def encrypt_key(self, m):
        if not self.key_is_set:
            raise Exception("Key is not set!")
        return m * self.cyphering_key

    def decrypt_key(self, c):
        if not self.key_is_set:
            raise Exception("Key is not set!")
        return c / self.cyphering_key


if __name__ == "__main__":
    p = 1234567891
    x = FiniteField(p, 2)
    dh_setup = DHSetup(x)
    generator = dh_setup.get_generator()
    print("Generator:", generator.get_value())
    user = User(dh_setup)
    public_key = user.get_public_key()
    print("Public key:", public_key.get_value())
    a = FiniteField(dh_setup.get_generator().get_characteristic(), 17)
    user.set_key(a)

    message = FiniteField(dh_setup.get_generator().get_characteristic(), 84215)
    print("Message:", message.get_value())

    encrypted = user.encrypt_key(message)
    print("Encrypted:", encrypted.get_value())

    decrypted = user.decrypt_key(encrypted)
    print("Decrypted:", decrypted.get_value())

    if message.get_value() == decrypted.get_value():
        print("SUCCESS!")
    else:
        print("FAIL!")