import graph.Graph;
import graph.GraphSolverFactory;
import timeconstraints.*;

import java.io.PrintWriter;
import java.util.*;


public class RandomTester {

    /**
     * Creates a set of problems with repetitionsPerTest tests of  1 to maxConstraints constraints
     * @param variableCount
     * @param maxConstraints
     * @param repetitionsPerTest
     * @return
     */
    public static List<List<TimeConstraint>> createRandomTimeConstraints(int variableCount, int maxConstraints, int repetitionsPerTest) {
        List<List<TimeConstraint>> random = new ArrayList<>();
        for (int constraints = 1; constraints <= maxConstraints; constraints++) {
            RandomConstraintGenerator rcg = new RandomConstraintGenerator(variableCount, 15, 0.5, constraints, 0.5);
            for (int reps = 0; reps < repetitionsPerTest; reps++) {
                random.add(rcg.generateConstraintList());
            }
        }
        return random;
    }

    /**
     * Creates a set of problems with repetitionsPerTest tests of 1 to maxConstraints constraints, allowing a little more customisation
     * @param variableCount
     * @param maxConstraints
     * @param repetitionsPerTest
     * @param maxSummand
     * @param fractionZeroSummand
     * @param fractionEquality
     * @return
     */
    public static List<List<TimeConstraint>> createRandomTimeConstraintsCustom(int variableCount, int maxConstraints, int repetitionsPerTest, int maxSummand, double fractionZeroSummand, double fractionEquality) {
        List<List<TimeConstraint>> random = new ArrayList<>();
        for (int constraints = 1; constraints <= maxConstraints; constraints++) {
            RandomConstraintGenerator rcg = new RandomConstraintGenerator(variableCount, maxSummand, fractionZeroSummand, constraints, fractionEquality);
            for (int reps = 0; reps < repetitionsPerTest; reps++) {
                random.add(rcg.generateConstraintList());
            }
        }
        return random;
    }

    /**
     * Creates a set of problems with a fixed number of constraints per problem
     * @param variableCount
     * @param constraints
     * @param numberOfTestsWithThisSetting
     * @param maxSummand
     * @param fractionZeroSummand
     * @param fractionEquality
     * @return
     */
    public static List<List<TimeConstraint>> createRandomTimeConstraintsCustomFixedConstraintCount(int variableCount, int constraints, int numberOfTestsWithThisSetting, int maxSummand, double fractionZeroSummand, double fractionEquality) {
        List<List<TimeConstraint>> random = new ArrayList<>();
        RandomConstraintGenerator rcg = new RandomConstraintGenerator(variableCount, maxSummand, fractionZeroSummand, constraints, fractionEquality);
        for (int reps = 0; reps < numberOfTestsWithThisSetting; reps++) {
            random.add(rcg.generateConstraintList());
        }
        return random;
    }

    public static boolean runMultiTest(int variableCount, int maxConstraints, int repetitionsPerTest, GraphSolverFactory algorithmFactory) {
        List<List<TimeConstraint>> random = createRandomTimeConstraints(variableCount, maxConstraints, repetitionsPerTest);


        List<ProblemComparison> problems = new ArrayList<>();

        for (int i = 1; i <= random.size(); i++) {
            problems.add(new ProblemComparison(random.get(i-1), "Random_" + i));
        }

        ProblemRunner pr = new ProblemRunner(problems);

        boolean result = pr.runBatch(algorithmFactory);

        if (!result) {
            List<ProblemComparison> failed = pr.getFailingProblems();

            for (int i = 0; i < failed.size(); i++) {
                writeFailedProblemDescription(failed.get(i));
                writeFailedGraphs(failed.get(i), algorithmFactory);
            }
        }

        return true;

    }

    private static void writeFailedGraphs(ProblemComparison problemComparison, GraphSolverFactory algorithmFactory) {
        Graph g = TimeConstraint.buildGraph(problemComparison.getTcs().getConstraints());
        GraphSolvedTimeConstraintSet graphSolvedTimeConstraintSet = new GraphSolvedTimeConstraintSet(problemComparison.getTcs(), g);
        graphSolvedTimeConstraintSet.writeExpandedProblemGraph("failed/" + problemComparison.getName() + "_1_init.dotgraph");
        graphSolvedTimeConstraintSet.writeContractedProblemGraph("failed/" + problemComparison.getName() + "_2_partial.dotgraph");
        graphSolvedTimeConstraintSet.writeFullyCollapsedProblemGraph_v3("failed/" + problemComparison.getName(), problemComparison.getName(), algorithmFactory);
    }

    private static void writeFailedProblemDescription(ProblemComparison pc) {
        StringJoiner sj = new StringJoiner("\n");

        sj.add("Failed problem")
                .add("--------------")
                .add(pc.getName())
                .add("---------------------------")
                .add("Constraint set:");

        for (TimeConstraint tc : pc.getTcs().getConstraints()) {
            sj.add(tc.toString());
        }

        Map<String, Map<String, Long>> differences = new HashMap<>();

        sj.add("---------------------------").add("ILP solutions:");

        if (pc.getSolutionILP() == null) {
            sj.add("ILP FAILED");
        }
        else {
            for (Map.Entry<String, Long> solution : pc.getSolutionILP().entrySet()) {
                sj.add(solution.getKey() + ": " + solution.getValue());
                if (!differences.containsKey(solution.getKey())) differences.put(solution.getKey(), new HashMap<>());
                differences.get(solution.getKey()).put("ILP", solution.getValue());
            }
        }

        sj.add("---------------------------").add("Graph solutions:");
        if (pc.getSolutionGraph() == null) {
            sj.add(pc.getError().message());
        }
        else {
            for (Map.Entry<String, Long> solution : pc.getSolutionGraph().entrySet()) {
                sj.add(solution.getKey() + ": " + solution.getValue());
                if (!differences.containsKey(solution.getKey())) differences.put(solution.getKey(), new HashMap<>());
                differences.get(solution.getKey()).put("GRAPH", solution.getValue());
            }
        }

        sj.add("---------------------------").add("Solution difference:").add("\tILP\tGRAPH");
        for (Map.Entry<String, Map<String, Long>> difference : differences.entrySet()) {
            Map<String, Long> values = difference.getValue();

            String ilpVal = values.containsKey("ILP") ? values.get("ILP") + "\t" : "-\t";
            String graphVal = values.containsKey("GRAPH") ? values.get("GRAPH") + "\t" : "-\t";
            sj.add(difference.getKey() + ":\t" + ilpVal + graphVal);
        }

        try {
            PrintWriter pw = new PrintWriter("failed/" + pc.getName() + ".log");
            pw.write(sj.toString());
            pw.flush();
            pw.close();
        }
        catch (Exception e) {
            System.out.println("Could not write log file");
        }


    }



}
