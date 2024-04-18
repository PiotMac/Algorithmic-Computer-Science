public class Tester {
    public static void main(String[] args) throws Exception {
       // FiniteField finiteField = new FiniteField(1234577);
        // ##### TESTING FOR GF = 1234577
        test1234577();
    }
    // ##### TESTING FOR GF = 1234577
    public static void test1234577() throws Exception {
        final int GF = 1234577;
        FiniteField a = new FiniteField(GF, 2);
        FiniteField b = new FiniteField(GF, 3);
        FiniteField c = new FiniteField(GF, 4);
        FiniteField d = new FiniteField(GF, 5);
        FiniteField result = new FiniteField(GF, 1234576);
        // TEST1: 2 + 3 * (4 - 5) = 1234576
        assert result.isEqual(a.add(b.multiply(c.subtract(d)))) : "TEST1 FAILED!";

        // TEST2: 2 - 3 - 2 = 1234574
        result = new FiniteField(GF, 1234574);
        FiniteField two1 = new FiniteField(GF, 2);
        FiniteField two2 = new FiniteField(GF, 2);
        FiniteField three = new FiniteField(GF, 3);
        assert result.isEqual(two1.subtract(three).subtract(two2)) : "TEST2 FAILED!";

        // TEST3: 269164 / 123456 = 567890
        result = new FiniteField(GF, 567890);
        FiniteField first = new FiniteField(GF, 269164);
        FiniteField second = new FiniteField(GF, 123456);
        assert result.isEqual(first.divide(second)) : "TEST3 FAILED!";

        // TEST4: 1 / -580978 = 123456
        result = new FiniteField(GF, 123456);
        FiniteField one = new FiniteField(GF, 1);
        FiniteField divider = new FiniteField(GF, -580978);
        assert result.isEqual(one.divide(divider)) : "TEST4 FAILED!";

        // TEST5: 123456789 = 1233666
        result = new FiniteField(GF, 1233666);
        FiniteField huge = new FiniteField(GF, 123456789);
        assert result.isEqual(huge) : "TEST5 FAILED!";

        // TEST6: -1234567 = 10
        result = new FiniteField(GF, 10);
        FiniteField minus = new FiniteField(GF, -1234567);
        assert result.isEqual(minus) : "TEST6 FAILED!";
    }
}