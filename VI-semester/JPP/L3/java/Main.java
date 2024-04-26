public class Main {
    public static void main(String[] args) throws Exception {
        FiniteFieldMethods x = new FiniteField(1234567891, 2);
        DHSetup<FiniteFieldMethods> dhSetup = new DHSetup<>(x);
        FiniteFieldMethods generator = dhSetup.getGenerator();
        System.out.println("Generator: " + generator.getValue());
        User<FiniteFieldMethods> user = new User<>(dhSetup);
        FiniteFieldMethods publicKey = user.getPublicKey();
        System.out.println("Public key: " + publicKey.getValue());
        FiniteFieldMethods a = new FiniteField(dhSetup.getGenerator().getCharacteristic(), 17);
        user.setKey(a);

        FiniteFieldMethods message = new FiniteField(dhSetup.getGenerator().getCharacteristic(), 84215);
        System.out.println("Message: " + message.getValue());

        FiniteFieldMethods encrypted = user.encryptKey(message);
        System.out.println("Encrypted: " + encrypted.getValue());

        FiniteFieldMethods decrypted = user.decryptKey(encrypted);
        System.out.println("Decrypted: " + decrypted.getValue());

        if (message.getValue() == decrypted.getValue()) {
            System.out.println("SUCCESS!");
        }
        else {
            System.out.println("FAIL!");
        }
    }
}