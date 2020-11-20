import timeconstraints.ILPSolvedTimeConstraintSet;
import timeconstraints.TimeConstraint;
import timeconstraints.TimeConstraintSet;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * A class explicitly for using the random structures and official ILP solver here to create random test files for the Haskell implementation.
 * File format:
 * [test name]
 * #CONSTRAINTS#
 * x1,1<x2,2                x1 + 1 <= x2 + 2
 * x1,1=x2,2                x1 + 1 = x2 + 2
 * .
 * .
 * .
 * #SOLUTION#
 * x1,1                     x1 = 1
 * .
 * .
 * .
 */
public class HaskellTestWriter {

    public static void main(String[] args) throws IOException {
        createTestBatch(5,80,50,5,0.2,0.3);

    }




    private static class TestResultAndName {
        private String testName;
        private String fileName;
        private String indexFileName;

        private ILPSolvedTimeConstraintSet solvedTCS;

        public TestResultAndName(String testName, String fileName, String indexFileName, ILPSolvedTimeConstraintSet solvedTCS) {
            this.testName = testName;
            this.fileName = fileName;
            this.indexFileName = indexFileName;
            this.solvedTCS = solvedTCS;
        }

        String testName() {
            return testName;
        }

        String testFileName() {
            return fileName;
        }

        String nameForTestIndexFile() {
            return indexFileName;
        }

        List<String> getConstraints() {
            List<TimeConstraint> tcs = solvedTCS.getTcs().getConstraints();

            return tcs.stream().map(TimeConstraint::getHaskellRepresentation).collect(Collectors.toList());
        }

        List<String> getSolution() {
            Map<String, Long> solution = solvedTCS.getSolution();

            if (solution == null) return new ArrayList<>();

            return solution.entrySet().stream().map(stringLongEntry -> stringLongEntry.getKey() + "," + stringLongEntry.getValue()).collect(Collectors.toList());

        }
    }

    private static void createTestBatch(int variableCount, int maxConstraints, int repetitionsPerTest, int maxSummand, double fractionZeroSummand, double fractionEquality) throws IOException {
        int batchNum = getNextBatchNum();

        String batchDir = "batch" + batchNum;

        List<List<TimeConstraint>> timeConstraintLists = RandomTester.createRandomTimeConstraintsCustom(variableCount, maxConstraints, repetitionsPerTest, maxSummand, fractionZeroSummand, fractionEquality);

        List<TimeConstraintSet> tcses = timeConstraintLists.stream().map(TimeConstraintSet::new).collect(Collectors.toList());

        List<ILPSolvedTimeConstraintSet> ilpSolvedTimeConstraintSets = tcses.stream().map(timeConstraintSet -> timeConstraintSet.toILP(ILPSolvedTimeConstraintSet.CURRENT_SOLVERFACTORY)).collect(Collectors.toList());

        List<TestResultAndName> trs = new ArrayList<>();
        for (int testNum = 0; testNum < ilpSolvedTimeConstraintSets.size(); testNum++) {
            trs.add(new TestResultAndName("b" + batchNum + "t" + testNum,
                    "b" + batchNum + "t" + testNum + ".t",
                    "testsgen/" + batchDir + "/b" + batchNum + "t" + testNum + ".t",
                    ilpSolvedTimeConstraintSets.get(testNum)));
        }

        runAndWriteTestsAndIndex(batchDir, trs);


    }

    private static int getNextBatchNum() {
        int batch = 1;

        while (new File("tests/haskellgen/batch" + batch).exists()) batch++;

        return batch;
    }


    private static void runAndWriteTestsAndIndex(String batchDirName, List<TestResultAndName> tests) throws IOException {
        writeTestBatchFile(batchDirName, tests.stream().map(TestResultAndName::nameForTestIndexFile).collect(Collectors.toList()));

        for (TestResultAndName trn : tests) {
            runWriteTest(batchDirName, trn);
        }

    }

    private static void runWriteTest(String batchDirName, TestResultAndName trn) throws IOException {
        trn.solvedTCS.runSolver();

        String outPath = "tests/haskellgen/" + batchDirName + "/";

        FileWriter fw = new FileWriter(outPath + trn.testFileName());

        fw.write(trn.testName() + "\n");
        fw.write("#CONSTRAINTS#\n");


        for (String constraintStr : trn.getConstraints()) {
            fw.write(constraintStr + "\n");
        }

        fw.write("#SOLUTION#\n");
        for (String solutionStr : trn.getSolution()) {
            fw.write(solutionStr + "\n");
        }

        fw.close();
    }

    private static void writeTestBatchFile(String batchDirName, List<String> fileNames) throws IOException {

        String outPath = "tests/haskellgen/" + batchDirName + "/";

        File newDir = new File(outPath);
        newDir.mkdir();

        FileWriter fw = new FileWriter(outPath + "index.t");

        for (String fn : fileNames) {
            fw.write(fn + "\n");
        }

        fw.close();

    }
}
