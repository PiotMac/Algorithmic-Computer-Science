import java.security.SecureRandom;

public class User<T extends FiniteFieldMethods> {
    long secret;
    private T cypheringKey;
    private boolean keyIsSet = false;
    DHSetup<T> dhSetup;
    public User(DHSetup<T> dhSetup) {
        this.dhSetup = dhSetup;
        SecureRandom secureRandom = new SecureRandom();
        long foundSecret = secureRandom.nextLong();
        while (foundSecret <= 0) {
            foundSecret = secureRandom.nextLong();
        }
        this.secret = foundSecret;
    }
    public T getPublicKey() throws Exception {
        return dhSetup.power(dhSetup.getGenerator(), secret);
    }

    public void setKey(T a) throws Exception {
        cypheringKey = dhSetup.power(a, secret);
        keyIsSet = true;
    }

    public T encryptKey(T m) throws Exception {
        if (!keyIsSet) {
            throw new Exception("Key is not set!");
        }
        return (T) m.multiply(cypheringKey);
    }

    public T decryptKey(T c) throws Exception {
        if (!keyIsSet) {
            throw new Exception("Key is not set!");
        }
        return (T) c.divide(cypheringKey);
    }
}
