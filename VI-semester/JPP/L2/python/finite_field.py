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
        first = self.value % self.characteristic
        second = other.value % self.characteristic
        result = first
        for i in range(1, second):
            result += first
            result = ((result % self.characteristic) + self.characteristic) % self.characteristic
        return FiniteField(self.characteristic, result)

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
