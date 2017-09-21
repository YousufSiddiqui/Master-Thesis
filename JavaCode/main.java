import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;


public class main {
    public static void main(String args[]) {
        try {

            List<String> results = new ArrayList<String>();

            File[] files = new File("C:/javaProject/").listFiles(); 

            for (int i = 0; i < files.length; i++) {
                if (files[i].isFile()) {
                    String[] splittedName = files[i].getName().split("\\.");
                    String extension = splittedName[splittedName.length - 1];
                    if (extension.equals("json")) {
                        results.add(files[i].getAbsolutePath());
                    }
                }
            }

            // Reading file and getting no. of files to be generated
            // String inputfile = "C:/javaProject/2015-09-09.json"; //  Source File Name.
            double nol = 1.0; //  No. of lines to be split and saved in each output file.

            for (int k = 0; k < results.size(); k++){
                String inputFile = results.get(k);
                System.out.print(inputFile);
                File file = new File(inputFile);
                Scanner scanner = new Scanner(file);
                int count = 0;
                while (scanner.hasNextLine()) {
                    scanner.nextLine();
                    count++;
                }
                System.out.println("Lines in the file: " + count);     // Displays no. of lines in the input file.

                double temp = (count / nol);
                int temp1 = (int) temp;
                int nof = 0;
                if (temp1 == temp) {
                    nof = temp1;
                } else {
                    nof = temp1 + 1;
                }
                System.out.println("No. of files to be generated :" + nof); // Displays no. of files to be generated.

                //---------------------------------------------------------------------------------------------------------

                // Actual splitting of file into smaller files

                FileInputStream fstream = new FileInputStream(inputFile);
                DataInputStream in = new DataInputStream(fstream);

                BufferedReader br = new BufferedReader(new InputStreamReader(in));
                String strLine;

                for (int j = 1; j <= nof; j++) {
                    FileWriter fstream1 = new FileWriter("C:/Split_Data/File" + j +"_"+k+ ".json");     // Destination File Location
                    BufferedWriter out = new BufferedWriter(fstream1);
                    for (int i = 1; i <= nol; i++) {
                        strLine = br.readLine();
                        if (strLine != null) {
                            out.write(strLine);
                            if (i != nol) {
                                out.newLine();
                            }
                        }
                    }
                    out.close();
                }

                in.close();
            }
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }
    }
}