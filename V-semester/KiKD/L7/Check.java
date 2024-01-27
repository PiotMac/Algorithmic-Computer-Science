import java.io.FileInputStream;

public class Check {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Check <firstFile> <secondFile>");
            return;
        }
        String firstFile = args[0];
        String secondFile = args[1];

        try {
            FileInputStream firstFis = new FileInputStream(firstFile);
            FileInputStream secondFis = new FileInputStream(secondFile);

            int firstByteRead;
            int secondByteRead;

            int difference = 0;
            while ((firstByteRead = firstFis.read()) != -1 && (secondByteRead = secondFis.read()) != -1) {
                if ((firstByteRead & 0xF0) != (secondByteRead & 0xF0)) {
                    difference++;
                }
                if ((firstByteRead & 0x0F) != (secondByteRead & 0x0F)) {
                    difference++;
                }
            }

            System.out.println("Difference = " + difference);

            firstFis.close();
            secondFis.close();
        }
        catch (Exception ex) {
            System.out.println("Something is wrong with the first/second file!");
        }
    }
}
