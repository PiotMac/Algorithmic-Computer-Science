public abstract class FiniteFieldMethods {
    public abstract long getCharacteristic();
    public abstract void setValue(long value);
    public abstract long getValue();

    // ################### COMPARING OPERATORS ###################
    public abstract boolean isEqual(FiniteFieldMethods other);
    public abstract boolean isNotEqual(FiniteFieldMethods other);
    public abstract boolean isLessOrEqual(FiniteFieldMethods other);
    public abstract boolean isGreaterOrEqual(FiniteFieldMethods other);
    public abstract boolean isLess(FiniteFieldMethods other);
    public abstract boolean isGreater(FiniteFieldMethods other);

    // ################### ARITHMETIC OPERATORS ###################
    public abstract FiniteFieldMethods add(FiniteFieldMethods other) throws Exception;
    public abstract FiniteFieldMethods subtract(FiniteFieldMethods other) throws Exception;
    public abstract FiniteFieldMethods multiply(FiniteFieldMethods other) throws Exception;
    public abstract FiniteFieldMethods divide(FiniteFieldMethods other) throws Exception;
    public abstract long modulo(long left, long right);

    // ################### ASSIGNING OPERATORS ###################
    public abstract void assign(FiniteFieldMethods other);
    public abstract void addAssign(FiniteFieldMethods other) throws Exception;
    public abstract void subtractAssign(FiniteFieldMethods other) throws Exception;
    public abstract void multiplyAssign(FiniteFieldMethods other) throws Exception;
    public abstract void divideAssign(FiniteFieldMethods other) throws Exception;

    public abstract FiniteFieldMethods copy();
}
