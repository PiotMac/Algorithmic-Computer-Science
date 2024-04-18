from finite_field import FiniteField


class Tester:
    @staticmethod
    def main():
        # FiniteField finiteField = new FiniteField(1234577)
        # ##### TESTING FOR GF = 1234577
        Tester.test_1234577()

    # ##### TESTING FOR GF = 1234577
    @staticmethod
    def test_1234577():
        GF = 1234577
        a = FiniteField(GF, 2)
        b = FiniteField(GF, 3)
        c = FiniteField(GF, 4)
        d = FiniteField(GF, 5)
        result = FiniteField(GF, 1234576)

        # TEST1: 2 + 3 * (4 - 5) = 1234576
        assert result == (a + b * (c - d)), "TEST1 FAILED!"

        # TEST2: 2 - 3 - 2 = 1234574
        result = FiniteField(GF, 1234574)
        two1 = FiniteField(GF, 2)
        two2 = FiniteField(GF, 2)
        three = FiniteField(GF, 3)
        assert result == (two1 - two2 - three), "TEST2 FAILED!"

        # TEST3: 269164 / 123456 = 567890
        result = FiniteField(GF, 567890)
        first = FiniteField(GF, 269164)
        second = FiniteField(GF, 123456)
        assert result == (first / second), "TEST3 FAILED!"

        # TEST4: 1 / -580978 = 123456
        result = FiniteField(GF, 123456)
        one = FiniteField(GF, 1)
        divider = FiniteField(GF, -580978)
        assert result == (one / divider), "TEST4 FAILED!"

        # TEST5: 123456789 = 1233666
        result = FiniteField(GF, 1233666)
        huge = FiniteField(GF, 123456789)
        assert result == huge, "TEST5 FAILED!"

        # TEST6: -1234567 = 10
        result = FiniteField(GF, 10)
        minus = FiniteField(GF, -1234567)
        assert result == minus, "TEST6 FAILED!"


tester = Tester()
tester.main()