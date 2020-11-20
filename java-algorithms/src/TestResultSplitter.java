import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * A quick script to read in the test results generated for problems that had no solution,
 * and split them up into problems that failed at graph H creation and those that failed at algorithm running.
 */
public class TestResultSplitter {
    public static void main(String[] args) throws IOException {
        runSplit("GraphData/");
        runSplit("GraphData/batch2");
    }

    /**
     *
     * @param directoryName
     * @throws IOException
     */
    private static void runSplit(String directoryName) throws IOException {
        // We don't want to split the batch files, just the xxx_problems_NO_SOLUTION

        for (File f : new File(directoryName).listFiles()) {
            boolean process = !f.isDirectory()
                    && f.toString().contains("NO_SOLUTION")
                    && f.toString().contains("problems")
                    && !f.toString().contains("NO_H")
                    && !f.toString().contains("FULL_ALG");
            if (process) {
                File fileNoSolutionNoGraphH = new File(f.toString().replace("NO_SOLUTION", "NO_SOLUTION_NO_H"));
                File fileNoSolutionFullAlg = new File(f.toString().replace("NO_SOLUTION", "NO_SOLUTION_FULL_ALG"));

                // First 4 lines need to be replicated across both output files, rest of lines need to be filtered appropriately
                // Assume that -1 in graph H vertices means no graph H.
                BufferedReader noSolution = new BufferedReader(new FileReader(f));


                List<String> noGraphH = new ArrayList<>();
                List<String> fullAlg = new ArrayList<>();
                List<String> fileprefix = new ArrayList<>();


                for (int i = 0; i < 5; i++ ){
                    fileprefix.add(noSolution.readLine());
                }


                for (String s : noSolution.lines().collect(Collectors.toList())) {
                    String[] ss  = s.split(",");
                    if (ss[4].equals("-1")) { // no graph H vertices
                        noGraphH.add(s);
                    }
                    else {
                        fullAlg.add(s);
                    }
                }


                BufferedWriter noGraphHOut = new BufferedWriter(new FileWriter(fileNoSolutionNoGraphH));
                BufferedWriter fullAlgOut = new BufferedWriter(new FileWriter(fileNoSolutionFullAlg));

                writeFiles(noGraphHOut, fileprefix, noGraphH);
                writeFiles(fullAlgOut, fileprefix, fullAlg);

                noGraphHOut.close();
                fullAlgOut.close();
                noSolution.close();
            }
        }
    }

    private static void writeFiles(BufferedWriter bw, List<String> prefix, List<String> toWrite) throws IOException {
        for (String p : prefix) {
            bw.write(p + "\n");
        }
        for (String tw : toWrite) {
            bw.write(tw + "\n");
        }
    }

}
