import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

public class Single {
    private static Single instance = null;

    protected Single() {
    }

    public static Single getInstance() {
        if (instance == null) {
            // Thread Safe. Might be costly operation in some case
            synchronized (Single.class) {
                if (instance == null) {
                    System.out.println("instance created");
                    instance = new Single();
                }
            }
        }
        return instance;
    }

    final static String fileName = "1.txt";

    public void write(String str) throws IOException {
        Path path = Paths.get(fileName);
        for (int i = 0; i < 20; ++i) {
            String s = str + "_" + String.valueOf(i) + "\n";
            Files.write(path, s.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        }
    }

    public String read() throws IOException {
        String result = "";
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {

            String sCurrentLine;

            while ((sCurrentLine = br.readLine()) != null) {
                result += sCurrentLine;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }
}