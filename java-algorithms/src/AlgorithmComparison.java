import graph.GraphSolverFactory;
import timeconstraints.GraphSolvedTimeConstraintSet;
import timeconstraints.TimeConstraint;
import timeconstraints.TimeConstraintSet;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Comparing running times of Algorithm 1 and Algorithm 2 on randomly-generated constraints.
 *
 * We'll need to have M constraints and N variables as our variables
 *
 * A few possibilities:
 * - mean, median, sigma, std dev for given M, N
 * - worst cases for given M, N
 * - direct comparison of how performing on different problems
 * - worst case for given M, N where you take the worst-performing in A1 and try in A2 and vice versa
 */
public class AlgorithmComparison {

    public static final String GRAPH_PATH = "GraphData/batch2/";

    private static class TestParamWithResult {
        public static final String AVERAGE_HEADERS = "VariableCount,ConstraintCount,ResultMeanAlg1,ResultStdDevAlg1,ResultMeanAlg2,ResultStdDevAlg2,MaxAllComponentSizes,MeanAllComponentSizes";
        private int maxSummand;
        private String testsName;
        private double fractionEquality;
        private double fractionZeroSummand;
        private int variableCount;
        private int constraintCount;
        private int numberOfProblemsWithTheseParameters;
        private int repeatEachSolveAttempt;

        private List<TimeConstraintSet> problems;

        private List<Map<String, Object>> algorithmInternalData1;
        private List<Map<String, Object>> algorithmInternalData2;



        private List<Long> execTimesListAlg1;
        private List<Long> execTimesListAlg2;
        private List<Map<String, Long>> resultsListAlg1;
        private List<Map<String, Long>> resultsListAlg2;

        public TestParamWithResult(String testsName, int maxSummand, double fractionEquality, double fractionZeroSummand, int variableCount, int constraintCount, int numberOfProblemsWithTheseParameters, int repeatEachSolveAttempt) {
            this.maxSummand = maxSummand;
            this.fractionEquality = fractionEquality;
            this.fractionZeroSummand = fractionZeroSummand;
            this.variableCount = variableCount;
            this.constraintCount = constraintCount;
            this.numberOfProblemsWithTheseParameters = numberOfProblemsWithTheseParameters;
            this.repeatEachSolveAttempt = repeatEachSolveAttempt;
            this.testsName = testsName;

            List<List<TimeConstraint>> problemsConstraintsList = RandomTester.createRandomTimeConstraintsCustomFixedConstraintCount(variableCount, constraintCount, numberOfProblemsWithTheseParameters, maxSummand, fractionZeroSummand, fractionEquality);
            problems = problemsConstraintsList.stream().map(TimeConstraintSet::new).collect(Collectors.toList());

            execTimesListAlg1 = new ArrayList<>(numberOfProblemsWithTheseParameters);
            execTimesListAlg2 = new ArrayList<>(numberOfProblemsWithTheseParameters);
            resultsListAlg1 = new ArrayList<>(numberOfProblemsWithTheseParameters);
            resultsListAlg2 = new ArrayList<>(numberOfProblemsWithTheseParameters);
            algorithmInternalData1 = new ArrayList<>(numberOfProblemsWithTheseParameters);
            algorithmInternalData2 = new ArrayList<>(numberOfProblemsWithTheseParameters);
        }

        public TestParamWithResult(String testsName,
                                   int maxSummand,
                                   double fractionEquality,
                                   double fractionZeroSummand,
                                   int variableCount,
                                   int constraintCount,
                                   int numberOfProblemsWithTheseParameters,
                                   int repeatEachSolveAttempt,
                                   List<TimeConstraintSet> problems,
                                   List<Long> execTimesListAlg1,
                                   List<Long> execTimesListAlg2,
                                   List<Map<String, Long>> resultsListAlg1,
                                   List<Map<String, Long>> resultsListAlg2,
                                   List<Map<String, Object>> algorithmInternalData1,
                                   List<Map<String, Object>> algorithmInternalData2) {
            this.maxSummand = maxSummand;
            this.fractionEquality = fractionEquality;
            this.fractionZeroSummand = fractionZeroSummand;
            this.variableCount = variableCount;
            this.constraintCount = constraintCount;
            this.numberOfProblemsWithTheseParameters = numberOfProblemsWithTheseParameters;
            this.repeatEachSolveAttempt = repeatEachSolveAttempt;
            this.testsName = testsName;
            this.problems = problems;
            this.execTimesListAlg1 = execTimesListAlg1;
            this.execTimesListAlg2 = execTimesListAlg2;
            this.resultsListAlg1 = resultsListAlg1;
            this.resultsListAlg2 = resultsListAlg2;
            this.algorithmInternalData1 = algorithmInternalData1;
            this.algorithmInternalData2 = algorithmInternalData2;
        }

        private long averageList(List<Long> ls) {
            long sum = 0;
            for (Long l : ls) {
                sum += l / ls.size();
            }
//            return (long) ((double) sum / (double) ls.size());
            return sum;
        }

        public long getMeanOfResults1() {
            return averageList(execTimesListAlg1);
        }

        public long getMeanOfResults2() {
            return averageList(execTimesListAlg2);
        }

        private long getStandardDeviation(List<Long> ls, long mean) {
            List<Long> squaredDiff = ls.stream().map(l -> {
               long diff = l - mean;
               return diff * diff;
            }).collect(Collectors.toList());

            long sumSquaredDiff = squaredDiff.stream().reduce(0l, (aLong, aLong2) -> aLong + aLong2);

            double sqrtsum = Math.sqrt((double) sumSquaredDiff);

            return (long) sqrtsum;
        }

        public long getStandardDeviationResults1() {
            return getStandardDeviation(execTimesListAlg1, getMeanOfResults1());
        }

        public long getStandardDeviationResults2() {
            return getStandardDeviation(execTimesListAlg2, getMeanOfResults2());
        }

        private TestParamWithResult getSuccessfulTestParamWithResult() {
            List<Long> execTimesListAlg1Temp = new ArrayList<>();
            List<Long> execTimesListAlg2Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg1Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg2Temp = new ArrayList<>();
            List<TimeConstraintSet> problemsTemp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData1Temp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData2Temp = new ArrayList<>();

            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                // we're going to assume all produced results are matches
                if (resultsListAlg1.get(i) != null) {
                    execTimesListAlg1Temp.add(execTimesListAlg1.get(i));
                    resultsListAlg1Temp.add(resultsListAlg1.get(i));
                    execTimesListAlg2Temp.add(execTimesListAlg2.get(i));
                    resultsListAlg2Temp.add(resultsListAlg2.get(i));
                    problemsTemp.add(problems.get(i));
                    algorithmInternalData1Temp.add(algorithmInternalData1.get(i));
                    algorithmInternalData2Temp.add(algorithmInternalData2.get(i));
                }
            }

            int newNumberOfProblemsSuccessful = problemsTemp.size();

            TestParamWithResult successful = new TestParamWithResult(
                    this.testsName + " SUCCESSES ONLY",
                    this.maxSummand,
                    this.fractionEquality,
                    this.fractionZeroSummand,
                    this.variableCount,
                    this.constraintCount,
                    newNumberOfProblemsSuccessful,
                    this.repeatEachSolveAttempt,
                    problemsTemp,
                    execTimesListAlg1Temp,
                    execTimesListAlg2Temp,
                    resultsListAlg1Temp,
                    resultsListAlg2Temp,
                    algorithmInternalData1Temp,
                    algorithmInternalData2Temp
            );

            return successful;
        }

        private TestParamWithResult getFailedTestParamWithResult() {
            List<Long> execTimesListAlg1Temp = new ArrayList<>();
            List<Long> execTimesListAlg2Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg1Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg2Temp = new ArrayList<>();
            List<TimeConstraintSet> problemsTemp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData1Temp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData2Temp = new ArrayList<>();

            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                // we're going to assume all produced results are matches
                if (resultsListAlg1.get(i) == null) {
                    execTimesListAlg1Temp.add(execTimesListAlg1.get(i));
                    resultsListAlg1Temp.add(resultsListAlg1.get(i));
                    execTimesListAlg2Temp.add(execTimesListAlg2.get(i));
                    resultsListAlg2Temp.add(resultsListAlg2.get(i));
                    problemsTemp.add(problems.get(i));
                    algorithmInternalData1Temp.add(algorithmInternalData1.get(i));
                    algorithmInternalData2Temp.add(algorithmInternalData2.get(i));
                }
            }

            int newNumberOfProblemsFailed = problemsTemp.size();

            TestParamWithResult failed = new TestParamWithResult(
            this.testsName + " FAILURES ONLY",
                    this.maxSummand,
                    this.fractionEquality,
                    this.fractionZeroSummand,
                    this.variableCount,
                    this.constraintCount,
                    newNumberOfProblemsFailed,
                    this.repeatEachSolveAttempt,
                    problemsTemp,
                    execTimesListAlg1Temp,
                    execTimesListAlg2Temp,
                    resultsListAlg1Temp,
                    resultsListAlg2Temp,
                    algorithmInternalData1Temp,
                    algorithmInternalData2Temp
            );

            return failed;
        }


        private TestParamWithResult getSuccessfulTestParamWithResultAlg2() {
            List<Long> execTimesListAlg2Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg2Temp = new ArrayList<>();
            List<TimeConstraintSet> problemsTemp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData2Temp = new ArrayList<>();

            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                // we're going to assume all produced results are matches
                if (resultsListAlg2.get(i) != null) {
                    execTimesListAlg2Temp.add(execTimesListAlg2.get(i));
                    resultsListAlg2Temp.add(resultsListAlg2.get(i));
                    problemsTemp.add(problems.get(i));
                    algorithmInternalData2Temp.add(algorithmInternalData2.get(i));
                }
            }

            int newNumberOfProblemsSuccessful = problemsTemp.size();

            TestParamWithResult successful = new TestParamWithResult(
                    this.testsName + " SUCCESSES ONLY",
                    this.maxSummand,
                    this.fractionEquality,
                    this.fractionZeroSummand,
                    this.variableCount,
                    this.constraintCount,
                    newNumberOfProblemsSuccessful,
                    this.repeatEachSolveAttempt,
                    problemsTemp,
                    new ArrayList<>(),
                    execTimesListAlg2Temp,
                    new ArrayList<>(),
                    resultsListAlg2Temp,
                    new ArrayList<>(),
                    algorithmInternalData2Temp
            );

            return successful;
        }

        private TestParamWithResult getFailedTestParamWithResultAlg2() {
            List<Long> execTimesListAlg2Temp = new ArrayList<>();
            List<Map<String, Long>> resultsListAlg2Temp = new ArrayList<>();
            List<TimeConstraintSet> problemsTemp = new ArrayList<>();
            List<Map<String, Object>> algorithmInternalData2Temp = new ArrayList<>();

            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                // we're going to assume all produced results are matches
                if (resultsListAlg2.get(i) == null) {
                    execTimesListAlg2Temp.add(execTimesListAlg2.get(i));
                    resultsListAlg2Temp.add(resultsListAlg2.get(i));
                    problemsTemp.add(problems.get(i));
                    algorithmInternalData2Temp.add(algorithmInternalData2.get(i));
                }
            }

            int newNumberOfProblemsFailed = problemsTemp.size();

            TestParamWithResult failed = new TestParamWithResult(
                    this.testsName + " FAILURES ONLY",
                    this.maxSummand,
                    this.fractionEquality,
                    this.fractionZeroSummand,
                    this.variableCount,
                    this.constraintCount,
                    newNumberOfProblemsFailed,
                    this.repeatEachSolveAttempt,
                    problemsTemp,
                    new ArrayList<>(),
                    execTimesListAlg2Temp,
                    new ArrayList<>(),
                    resultsListAlg2Temp,
                    new ArrayList<>(),
                    algorithmInternalData2Temp
            );

            return failed;
        }

        /**
         * "VariableCount,ConstraintCount,ResultMeanAlg1,ResultStdDevAlg1,ResultMeanAlg2,ResultStdDevAlg2,MaxAllComponentSizes,MeanAllComponentSizes";
         * @return
         *
         */
        public String getAverageDataString() {
            StringJoiner sj = new StringJoiner(",","","\n");
            sj.add(Integer.toString(variableCount));
            sj.add(Integer.toString(constraintCount));
            sj.add(Long.toString(getMeanOfResults1()));
            sj.add(Long.toString(getStandardDeviationResults1()));
            sj.add(Long.toString(getMeanOfResults2()));
            sj.add(Long.toString(getStandardDeviationResults2()));
            sj.add(Integer.toString(maxAllComponentSizes()));
            sj.add(Long.toString(meanAllComponentSizes()));

            return sj.toString();
        }



        public void runSolvers() {
            solveTimeConstraintsBestAlternating(testsName, repeatEachSolveAttempt, problems, resultsListAlg1, execTimesListAlg1, resultsListAlg2, execTimesListAlg2, algorithmInternalData1, algorithmInternalData2);
        }

        public void runSolverAlg2() {
            solveTimeConstraintsBest2(testsName + " Alg 2", repeatEachSolveAttempt, problems, GraphSolverFactory.REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2, resultsListAlg2, execTimesListAlg2, algorithmInternalData2);
        }

        public void runSolverAlg1() {
            solveTimeConstraintsBest2(testsName + " Alg 1", repeatEachSolveAttempt, problems, GraphSolverFactory.REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2, resultsListAlg2, execTimesListAlg2, algorithmInternalData2);
        }

        public long lastTime1() {
            return execTimesListAlg1.get(execTimesListAlg1.size() - 1);
        }

        public long lastTime2() {
            return execTimesListAlg2.get(execTimesListAlg2.size() - 1);
        }

        public List<Long> getExecTimesListAlg1() {
            return execTimesListAlg1;
        }

        public List<Long> getExecTimesListAlg2() {
            return execTimesListAlg2;
        }

        public List<Map<String, Long>> getResultsListAlg1() {
            return resultsListAlg1;
        }

        public List<Map<String, Long>> getResultsListAlg2() {
            return resultsListAlg2;
        }

        public int getProblemCount() {
            return this.numberOfProblemsWithTheseParameters;
        }

        // used for JIT stuff
        public String getForcedResult() {
            return "Finished " + testsName + ", forcing values: " + lastTime1() + ", " + lastTime2();
        }

        private int getGVertexCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_G_VERTICES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_G_VERTICES);
        }
        private int getGEdgeCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_G_EDGES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_G_EDGES);
        }
        private int getHVertexCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_H_VERTICES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_H_VERTICES);
        }
        private int getHEdgeCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_H_EDGES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_H_EDGES);
        }
        private int getRVertexCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_R_VERTICES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_R_VERTICES);
        }
        private int getREdgeCount(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_R_EDGES)) return -1;
            return (int) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_R_EDGES);
        }
        private long getVertexIncrements1(int i) {
            if (!algorithmInternalData1.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS)) return -1;
            return (long) algorithmInternalData1.get(i).get(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS);
        }
        private long getVertexIncrements2(int i) {
            if (!algorithmInternalData2.get(i).containsKey(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS)) return -1;
            return (long) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS);
        }


        public void writeComplete(FileWriter fw) throws IOException {
            fw.write("VariableCount,ConstraintCount,MaxSummand\n");
            fw.write(variableCount + "," + constraintCount + "," + maxSummand + "\n");

            fw.write("ExecTimeAlg1,ExecTimeAlg2,GVertexCount,GEdgeCount,HVertexCount,HEdgeCount,RVertexCount,REdgeCount,VertexIncrementsAlg1,VertexIncrementsAlg2,MaxComponentSize,MeanComponentSize,AllComponentSizes\n");
            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                long time1 = execTimesListAlg1.get(i);
                long time2 = execTimesListAlg2.get(i);
                int gVertices = getGVertexCount(i);
                int gEdges = getGEdgeCount(i);
                int hVertices = getHVertexCount(i);
                int hEdges = getHEdgeCount(i);
                int rVertices = getRVertexCount(i);
                int rEdges = getREdgeCount(i);
                long vIncAlg1 = getVertexIncrements1(i);
                long vIncAlg2 = getVertexIncrements2(i);
                int maxComponentSize = maxComponentSize(i);
                int meanComponentSize = meanComponentSize(i);
                String componentSizes = componentSizesToString(i);


                StringJoiner sj = new StringJoiner(",","","\n");
                sj.add(Long.toString(time1));
                sj.add(Long.toString(time2));
                sj.add(Integer.toString(gVertices));
                sj.add(Integer.toString(gEdges));
                sj.add(Integer.toString(hVertices));
                sj.add(Integer.toString(hEdges));
                sj.add(Integer.toString(rVertices));
                sj.add(Integer.toString(rEdges));
                sj.add(Long.toString(vIncAlg1));
                sj.add(Long.toString(vIncAlg2));
                sj.add(Integer.toString(maxComponentSize));
                sj.add(Integer.toString(meanComponentSize));
                sj.add(componentSizes);

                fw.write(sj.toString());
            }
        }

        public void writeCompleteAlg2(FileWriter fw) throws IOException {
            fw.write("VariableCount,ConstraintCount,MaxSummand\n");
            fw.write(variableCount + "," + constraintCount + "," + maxSummand + "\n");

            fw.write("ExecTimeAlg2,GVertexCount,GEdgeCount,HVertexCount,HEdgeCount,RVertexCount,REdgeCount,VertexIncrementsAlg2,MaxComponentSize,MeanComponentSize,AllComponentSizes\n");
            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                long time2 = execTimesListAlg2.get(i);
                int gVertices = getGVertexCount(i);
                int gEdges = getGEdgeCount(i);
                int hVertices = getHVertexCount(i);
                int hEdges = getHEdgeCount(i);
                int rVertices = getRVertexCount(i);
                int rEdges = getREdgeCount(i);
                long vIncAlg2 = getVertexIncrements2(i);
                int maxComponentSize = maxComponentSize(i);
                int meanComponentSize = meanComponentSize(i);
                String componentSizes = componentSizesToString(i);


                StringJoiner sj = new StringJoiner(",","","\n");
                sj.add(Long.toString(time2));
                sj.add(Integer.toString(gVertices));
                sj.add(Integer.toString(gEdges));
                sj.add(Integer.toString(hVertices));
                sj.add(Integer.toString(hEdges));
                sj.add(Integer.toString(rVertices));
                sj.add(Integer.toString(rEdges));
                sj.add(Long.toString(vIncAlg2));
                sj.add(Integer.toString(maxComponentSize));
                sj.add(Integer.toString(meanComponentSize));
                sj.add(componentSizes);

                fw.write(sj.toString());
            }
        }

        private List<Integer> getComponentSizes(int i) {
            Map<Integer, List<Integer>> rankToComponentSizes = (Map<Integer, List<Integer>>) algorithmInternalData2.get(i).get(GraphSolvedTimeConstraintSet.KEY_R_COMPONENT_SIZES_BY_RANK);

            if (rankToComponentSizes == null) return List.of(-1); // component sizes -1 when it never got as far as building R because failure in H

            List<Integer> merged = new ArrayList<>();
            for (List<Integer> listRanks : rankToComponentSizes.values()) {
                merged.addAll(listRanks);
            }

            merged.sort(Integer::compareTo);

            return merged;
        }

        private String componentSizesToString(int i) {
            List<Integer> componentSizes = getComponentSizes(i);
            StringJoiner sj = new StringJoiner("/","[","]"); // list is written [size1=count1/size2=count2/.../sizen=countn]
            Map<Integer,Integer> sizeToCount = new HashMap<>();
            for (Integer size : componentSizes) {
                if (!sizeToCount.containsKey(size)) {
                    sizeToCount.put(size, 0);
                }
                sizeToCount.put(size, sizeToCount.get(size) + 1);
            }

            class Pair {
                int size;
                int count;

                public Pair(int size, int count) {
                    this.size = size;
                    this.count = count;
                }
            }

            List<Pair> pairs = new ArrayList<>();
            for (Map.Entry<Integer, Integer> entry : sizeToCount.entrySet()) {
                pairs.add(new Pair(entry.getKey(), entry.getValue()));
            }

            Comparator<Pair> pairComparator = new Comparator<Pair>() {
                @Override
                public int compare(Pair o1, Pair o2) {
                    return Integer.compare(o1.size, o2.size);
                }
            };

            pairs.sort(pairComparator);

            for (Pair entry : pairs) {
                sj.add(entry.size + "=" + entry.count);
            }
            return sj.toString();
        }

        private Integer maxComponentSize(int i) {
            return getComponentSizes(i).stream().max(Integer::compareTo).get();
        }

        private Integer meanComponentSize(int i) {
            // count() is terminating operation...
            // reduce() also terminating operation...
            // so we need 2 streams. could do it without streams, i suppose
            Stream<Integer> is1 = getComponentSizes(i).stream().filter(integer -> integer != -1);
            Stream<Integer> is2 = getComponentSizes(i).stream().filter(integer -> integer != -1);

            int size = (int) is2.count();
            if (size == 0) return -1;

            return is1.reduce(0, (integer, integer2) -> integer + integer2) / size;
        }

        public int maxAllComponentSizes() {
            List<Integer> maxes = new ArrayList<>();
            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                maxes.add(maxComponentSize(i));
            }
            if (maxes.isEmpty()) maxes.add(-1);
            return maxes.stream().max(Integer::compareTo).get();
        }

        public long meanAllComponentSizes() {
            if (numberOfProblemsWithTheseParameters == 0) return -1;
            long sum = 0;
            long validElems = 0;
            for (int i = 0; i < numberOfProblemsWithTheseParameters; i++) {
                int mcs = meanComponentSize(i);
                if (mcs != -1) {
                    sum += mcs;
                    validElems++;
                }
            }
            return validElems == 0 ? -1 : sum / validElems;
        }


        public void writeAverages(FileWriter fw) throws IOException {
            fw.write(getAverageDataString());
        }
    }




/*    public static void main(String[] args) {

        // We're trying to avoid creating cycles in the graph G - that doesn't tell us anything about worst-case performance in handling H
        int maxSummand = 40;
        double fractionEquality = 0.1;
        double fractionZeroSummand = 0.1;


        int variableCount = 100;
        int constraints = 200;
        int repetitionsPerTest = 500;   // lots of repetitions per test, we're not doing 1...maxConstraints, we're doing a fixed constraint count

        List<List<TimeConstraint>> jitWarmupConstraintsList = RandomTester.createRandomTimeConstraintsCustomFixedConstraintCount(10, 20, 20000, 5, 0.1, 0.1);

        List<List<TimeConstraint>> problemsConstraintsList = RandomTester.createRandomTimeConstraintsCustomFixedConstraintCount(variableCount, constraints, repetitionsPerTest, maxSummand, fractionZeroSummand, fractionEquality);

        List<TimeConstraintSet> jitWarmupProblemList = jitWarmupConstraintsList.stream().map(TimeConstraintSet::new).collect(Collectors.toList());

        List<TimeConstraintSet> problemsList = problemsConstraintsList.stream().map(TimeConstraintSet::new).collect(Collectors.toList());



        GraphSolverFactory a1Solver = GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1;
        GraphSolverFactory a2Solver = GraphSolverFactory.REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2;

        int problemCount = problemsConstraintsList.size();

        List<Map<String, Long>> solA1 = new ArrayList<>();
        List<Map<String, Long>> solA2 = new ArrayList<>();

        List<Long> timeDiffA1 = new ArrayList<>(problemCount);
        List<Long> timeDiffA2 = new ArrayList<>(problemCount);



        // a couple of warmup runs??
//        List<Long> warmupForce = new ArrayList<>(problemCount);
//        solveTimeConstraintsBest2(20, jitWarmupProblemList, a1Solver, new ArrayList<>(), warmupForce);
//        System.out.println("Done warmup 1: forcing value" + warmupForce.get(warmupForce.size() - 1));
//
//        warmupForce.clear();
//        solveTimeConstraintsBest2(20, jitWarmupProblemList, a2Solver, new ArrayList<>(), warmupForce);
//        System.out.println("Done warmup 2: forcing value" + warmupForce.get(warmupForce.size() - 1));
//
//
//
//        // and the actual runs
//        solveTimeConstraintsBest2(50, problemsList, a2Solver, solA2, timeDiffA2);
//
//        solveTimeConstraintsBest2(50, problemsList, a1Solver, solA1, timeDiffA1);

        // warmup
        List<Long> warmupForce1 = new ArrayList<>(problemCount);
        List<Long> warmupForce2 = new ArrayList<>(problemCount);
        System.out.println("WARMUP JIT");
        solveTimeConstraintsBestAlternating(20, jitWarmupProblemList, new ArrayList<>(), warmupForce1, new ArrayList<>(), warmupForce2);
        System.out.println("Done warmup, forcing values: " + warmupForce1.get(warmupForce1.size() - 1) + ", " + warmupForce2.get(warmupForce2.size() - 1));

        // actual
        System.out.println("TESTS:");
        solveTimeConstraintsBestAlternating(50, problemsList, solA1, timeDiffA1, solA2, timeDiffA2);

        int alg1Wins = 0;
        int alg2Wins = 0;

        int differences = 0;

        double ratioMin = 999;
        double ratioMax = 0;



        for (int i = 0; i < problemCount; i++) {
            long t1 = timeDiffA1.get(i);
            long t2 = timeDiffA2.get(i);

            double ratio = ((double) t1) / ((double) t2);

            if (t1 < t2) alg1Wins++;
            if (t2 < t1) alg2Wins++;

            if (0 < ratio && ratio < 1000) {
                if (ratio < ratioMin) ratioMin = ratio;
                if (ratio > ratioMax) ratioMax = ratio;
            }
            //System.out.println("Ratio: " + ratio);
            //System.out.println("Diff: " + (t1 - t2));

            if (!checkResultsEqual(solA1.get(i), solA2.get(i))) differences++;

        }

        System.out.println("Alg 1 Wins: " + alg1Wins);
        System.out.println("Alg 2 Wins: " + alg2Wins);
        System.out.println("Best speedup A1/A2 " + (1.0/ratioMin) + ", A2/A1 " + ratioMax);
        System.out.println("DIFFERENT RESULTS: " + differences);
    }*/


    public static void main(String[] args) throws IOException {

        // We're trying to avoid creating cycles in the graph G - that doesn't tell us anything about worst-case performance in handling H

        TestParamWithResult jitWarmup1 = new TestParamWithResult(
                "JIT Warmup 1",
                5,
                0.1,
                0.1,
                8,
                40,
                20000,
                20);

        TestParamWithResult jitWarmup2 = new TestParamWithResult(
                "JIT Warmup 2",
                10,
                0.1,
                0.1,
                20,
                50,
                10000,
                20);

        TestParamWithResult tinyProblems1 = new TestParamWithResult(
                "Tiny problems 1",
                5,
                0.05,
                0.1,
                8,
                4,
                20000,
                20
        );

        TestParamWithResult tinyProblems2 = new TestParamWithResult(
                "Tiny problems 2",
                5,
                0.05,
                0.1,
                8,
                6,
                20000,
                20
        );

        TestParamWithResult tinyProblems3 = new TestParamWithResult(
                "Tiny problems 3",
                5,
                0.05,
                0.1,
                10,
                12,
                20000,
                20
        );

        TestParamWithResult smallProblems1 = new TestParamWithResult(
                "Small problems 1",
                5,
                0.01,
                0.01,
                20,
                10,
                5000,
                20
        );

        TestParamWithResult smallProblems2 = new TestParamWithResult(
                "Small problems 2",
                5,
                0.01,
                0.01,
                20,
                15,
                5000,
                20
        );

        TestParamWithResult smallProblems3 = new TestParamWithResult(
                "Small problems 3",
                5,
                0.01,
                0.01,
                20,
                25,
                5000,
                20
        );


        TestParamWithResult standardProblems1 = new TestParamWithResult(
                "Standard problems 1",
                10,
                0.01,
                0.01,
                100,
                50,
                1000,
                20
        );
        TestParamWithResult standardProblems2 = new TestParamWithResult(
                "Standard problems 2",
                10,
                0.01,
                0.01,
                100,
                70,
                1000,
                20
        );
        TestParamWithResult standardProblems3 = new TestParamWithResult(
                "Standard problems 3",
                10,
                0.01,
                0.01,
                100,
                110,
                1000,
                20
        );

        TestParamWithResult largeProblems1 = new TestParamWithResult(
                "Large problems 1",
                50,
                0.01,
                0.01,
                1000,
                500,
                700,
                10
        );

        TestParamWithResult largeProblems2 = new TestParamWithResult(
                "Large problems 2",
                50,
                0.01,
                0.01,
                1000,
                700,
                700,
                10
        );

        TestParamWithResult largeProblems3 = new TestParamWithResult(
                "Large problems 3",
                50,
                0.1,
                0.1,
                1000,
                1200,
                700,
                10
        );



        System.out.println("Generating `huge problems'");

        TestParamWithResult hugeProblems1 = new TestParamWithResult(
                "Huge problems 1",
                50,
                0.1,
                0.1,
                100000,
                20000,
                50,
                1
        );

        // Assume that the solution time will vastly exceed the time of any
        // garbage collection or JIT weirdness for these two problems
        // and any larger.
//        TestParamWithResult hugeProblems2 = new TestParamWithResult(
//                "Huge problems 2",
//                50,
//                0.1,
//                0.1,
//                75000,
//                25000,
//                50,
//                1
//        );
//
//
//        TestParamWithResult hugeProblems3= new TestParamWithResult(
//                "Huge problems 3",
//                50,
//                0.1,
//                0.1,
//                75000,
//                50000,
//                50,
//                1
//        );
//
//        TestParamWithResult hugeProblems4= new TestParamWithResult(
//                "Huge problems 4",
//                50,
//                0.1,
//                0.1,
//                75000,
//                75000,
//                50,
//                1
//        );
//
//        TestParamWithResult hugeProblems5= new TestParamWithResult(
//                "Huge problems 5",
//                50,
//                0.1,
//                0.1,
//                75000,
//                100000,
//                50,
//                1
//        );



//        TestParamWithResult galacticProblems = new TestParamWithResult(
//                "Galactic problems",
//                50,
//                0.1,
//                0.1,
//                10000000,
//                3000000,
//                20,
//                3
//        );


        jitWarmup1.runSolvers();
        System.out.println(jitWarmup1.getForcedResult());

        jitWarmup2.runSolvers();
        System.out.println(jitWarmup2.getForcedResult());


        // Just for more warmup, now
//        tinyProblems1.runSolvers();
//        smallProblems1.runSolvers();
//        standardProblems1.runSolvers();
        //largeProblems1.runSolvers();


//        System.out.println(tinyProblems1.getForcedResult());
//        System.out.println(smallProblems1.getForcedResult());
//        System.out.println(standardProblems1.getForcedResult());
//        System.out.println(largeProblems1.getForcedResult());

//        writeAllSingleTestParamResult(tinyProblems1, "tiny_problems_1", "Tiny Problems 1");
//        writeAllSingleTestParamResult(smallProblems1, "small_problems_1", "Small Problems 1");
//        writeAllSingleTestParamResult(standardProblems1, "standard_problems_1", "Standard Problems 1");
//        writeAllSingleTestParamResult(largeProblems1, "large_problems_1","Large Problems 1");
//
//
//        tinyProblems2.runSolvers();
//        smallProblems2.runSolvers();
//        standardProblems2.runSolvers();
//        largeProblems2.runSolvers();
//
//
//        writeAllSingleTestParamResult(tinyProblems2, "tiny_problems_2", "Tiny Problems 2");
//        writeAllSingleTestParamResult(smallProblems2, "small_problems_2", "Small Problems 2");
//        writeAllSingleTestParamResult(standardProblems2, "standard_problems_2", "Standard Problems 2");
//        writeAllSingleTestParamResult(largeProblems2, "large_problems_2","Large Problems 2");
//
//
//
//        tinyProblems3.runSolvers();
//        smallProblems3.runSolvers();
//        standardProblems3.runSolvers();
//        largeProblems3.runSolvers();
//
//
//        writeAllSingleTestParamResult(tinyProblems3, "tiny_problems_3", "Tiny Problems 3");
//        writeAllSingleTestParamResult(smallProblems3, "small_problems_3", "Small Problems 3");
//        writeAllSingleTestParamResult(standardProblems3, "standard_problems_3", "Standard Problems 3");
//        writeAllSingleTestParamResult(largeProblems3, "large_problems_3","Large Problems 3");




//        TestParamWithResult largerProblems1 = new TestParamWithResult(
//                "Larger Problems 1",
//                50,
//                0.1,
//                0.1,
//                10000,
//                8000,
//                300,
//                1
//        );
//        largerProblems1.runSolvers();
//        writeAllSingleTestParamResult(largerProblems1, "larger_problems_1", "Larger Problems 1");

        TestParamWithResult largerProblems2 = new TestParamWithResult(
                "Larger Problems 2",
                50,
                0.1,
                0.1,
                10000,
                12000,
                50,
                1
        );
        largerProblems2.runSolvers();
        writeAllSingleTestParamResult(largerProblems2, "larger_problems_2", "Larger Problems 2");

        TestParamWithResult largerProblems3 = new TestParamWithResult(
                "Larger Problems 3",
                50,
                0.1,
                0.1,
                10000,
                20000,
                50,
                1
        );
        largerProblems3.runSolvers();
        writeAllSingleTestParamResult(largerProblems3, "larger_problems_3", "Larger Problems 3");


        TestParamWithResult largerProblems4 = new TestParamWithResult(
                "Larger Problems 4",
                50,
                0.1,
                0.1,
                10000,
                30000,
                40,
                1
        );
        largerProblems4.runSolvers();
        writeAllSingleTestParamResult(largerProblems4, "larger_problems_4", "Larger Problems 4");

        TestParamWithResult largerProblems5 = new TestParamWithResult(
                "Larger Problems 5",
                50,
                0.1,
                0.1,
                25000,
                20000,
                30,
                1
        );
        largerProblems5.runSolvers();
        writeAllSingleTestParamResult(largerProblems5, "larger_problems_5", "Larger Problems 5");

        TestParamWithResult largerProblems6 = new TestParamWithResult(
                "Larger Problems 6",
                50,
                0.1,
                0.1,
                25000,
                30000,
                30,
                1
        );
        largerProblems6.runSolvers();
        writeAllSingleTestParamResult(largerProblems6, "larger_problems_6", "Larger Problems 6");


        TestParamWithResult largerProblems7 = new TestParamWithResult(
                "Larger Problems 7",
                50,
                0.1,
                0.1,
                25000,
                50000,
                30,
                1
        );
        largerProblems7.runSolvers();
        writeAllSingleTestParamResult(largerProblems7, "larger_problems_7", "Larger Problems 7");

        TestParamWithResult largerProblems8 = new TestParamWithResult(
                "Larger Problems 8",
                50,
                0.1,
                0.1,
                40000,
                30000,
                20,
                1
        );
        largerProblems8.runSolvers();
        writeAllSingleTestParamResult(largerProblems8, "larger_problems_8", "Larger Problems 8");

        TestParamWithResult largerProblems9 = new TestParamWithResult(
                "Larger Problems 9",
                50,
                0.1,
                0.1,
                40000,
                80000,
                20,
                1
        );
        largerProblems9.runSolvers();
        writeAllSingleTestParamResult(largerProblems9, "larger_problems_9", "Larger Problems 9");




        // shouldn't need this any more
        //testAll_WinsCounts(List.of(tinyProblems, smallProblems, standardProblems, largeProblems));

        /*List<TestParamWithResult> smallRatio_Half = createTCSListWithIncrementConstraintsVerticesRatioHalf_Small();
        runAllGiven(smallRatio_Half);
        writeGraphVerticesConstraintsAverages(smallRatio_Half, "small_ratio_half", "Small Problem (10-210V), C = 0.5 * V");


        List<TestParamWithResult> smallRatio_ThreeQuarters = createTCSListWithIncrementConstraintsVerticesRatioThreeQuarters_Small();
        runAllGiven(smallRatio_ThreeQuarters);
        writeGraphVerticesConstraintsAverages(smallRatio_ThreeQuarters, "small_ratio_threequarters", "Small Problem (10-210V), C = 0.75 * V");


        List<TestParamWithResult> smallRatio_1 = createTCSListWithIncrementConstraintsVerticesRatio1_Small();
        runAllGiven(smallRatio_1);
        writeGraphVerticesConstraintsAverages(smallRatio_1, "small_ratio_1", "Small Problem, C = V");


        List<TestParamWithResult> smallRatio_2 = createTCSListWithIncrementConstraintsVerticesRatio2_Small();
        runAllGiven(smallRatio_2);
        writeGraphVerticesConstraintsAverages(smallRatio_2, "small_ratio_2", "Small Problem, C = 2V");

        List<TestParamWithResult> smallRatio_3 = createTCSListWithIncrementConstraintsVerticesRatio3_Small();
        runAllGiven(smallRatio_3);
        writeGraphVerticesConstraintsAverages(smallRatio_3, "small_ratio_3", "Small Problem, C = 3V");

        List<TestParamWithResult> smallRatio_4 = createTCSListWithIncrementConstraintsVerticesRatio4_Small();
        runAllGiven(smallRatio_4);
        writeGraphVerticesConstraintsAverages(smallRatio_4, "small_ratio_4", "Small Problem, C = 4V");

        List<TestParamWithResult> smallRatio_5 = createTCSListWithIncrementConstraintsVerticesRatio5_Small();
        runAllGiven(smallRatio_5);
        writeGraphVerticesConstraintsAverages(smallRatio_5, "small_ratio_5", "Small Problem, C = 5V");*/

//        List<TestParamWithResult> smallRatio_10 = createTCSListWithIncrementConstraintsVerticesRatio10_Small();
//        runAllGiven(smallRatio_10);
//        writeGraphVerticesConstraintsAverages(smallRatio_10, "small_ratio_10", "Small Problem, C = 10V");


        /*List<TestParamWithResult> largeRatio_Half = createTCSListWithIncrementConstraintsVerticesRatioHalf_Large();
        runAllGiven(largeRatio_Half);
        writeGraphVerticesConstraintsAverages(largeRatio_Half, "large_ratio_half", "Large Problem (10-210V), C = 0.5 * V");


        List<TestParamWithResult> largeRatio_ThreeQuarters = createTCSListWithIncrementConstraintsVerticesRatioThreeQuarters_Large();
        runAllGiven(largeRatio_ThreeQuarters);
        writeGraphVerticesConstraintsAverages(largeRatio_ThreeQuarters, "large_ratio_threequarters", "Large Problem (10-210V), C = 0.75 * V");

        List<TestParamWithResult> largeRatio_1 = createTCSListWithIncrementConstraintsVerticesRatio1_Large();
        runAllGiven(largeRatio_1);
        writeGraphVerticesConstraintsAverages(largeRatio_1, "large_ratio_1", "Large Problem, C = V");

        List<TestParamWithResult> largeRatio_2 = createTCSListWithIncrementConstraintsVerticesRatio2_Large();
        runAllGiven(largeRatio_2);
        writeGraphVerticesConstraintsAverages(largeRatio_2, "large_ratio_2", "Large Problem, C = 2V");

        List<TestParamWithResult> largeRatio_3 = createTCSListWithIncrementConstraintsVerticesRatio3_Large();
        runAllGiven(largeRatio_3);
        writeGraphVerticesConstraintsAverages(largeRatio_3, "large_ratio_3", "Large Problem, C = 3V");

        List<TestParamWithResult> largeRatio_4 = createTCSListWithIncrementConstraintsVerticesRatio4_Large();
        runAllGiven(largeRatio_4);
        writeGraphVerticesConstraintsAverages(largeRatio_4, "large_ratio_4", "Large Problem, C = 4V");

        List<TestParamWithResult> largeRatio_5 = createTCSListWithIncrementConstraintsVerticesRatio5_Large();
        runAllGiven(largeRatio_5);
        writeGraphVerticesConstraintsAverages(largeRatio_5, "large_ratio_5", "Large Problem, C = 5V");*/

        //List<TestParamWithResult> largeRatio_10 = createTCSListWithIncrementConstraintsVerticesRatio10_Large();
        //writeGraphVerticesConstraintsAverages(largeRatio_10, "large_ratio_10", "Large Problem, C = 10V");


        // Get some big ones on the go?



        //runAllGiven(largeRatio_Half, largeRatio_ThreeQuarters, largeRatio_1, largeRatio_2, largeRatio_3, largeRatio_4, largeRatio_5, largeRatio_10);



//        hugeProblems1.runSolvers();
//        writeAllSingleTestParamResult(hugeProblems1, "huge_problems_1", "Huge Problems 1");
//
//        hugeProblems2.runSolvers();
//        writeAllSingleTestParamResult(hugeProblems2, "huge_problems_2", "Huge Problems 2");
//
//        hugeProblems3.runSolvers();
//        writeAllSingleTestParamResult(hugeProblems3, "huge_problems_3", "Huge Problems 3");


//        // Just algorithm 2, since algorithm 1 is the bottleneck here
//        hugeProblems1.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(hugeProblems1, "huge_problems_1", "Huge Problems 1 (Alg2 only)");
//
//        hugeProblems2.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(hugeProblems2, "huge_problems_2", "Huge Problems 2 (Alg2 only)");
//
//        hugeProblems3.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(hugeProblems3, "huge_problems_3", "Huge Problems 3 (Alg2 only)");
//
//        hugeProblems4.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(hugeProblems4, "huge_problems_4", "Huge Problems 4 (Alg2 only)");
//
//        hugeProblems5.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(hugeProblems5, "huge_problems_5", "Huge Problems 5 (Alg2 only)");


//
//
//        System.out.println("Generating `massive problems 1'");
//        TestParamWithResult massiveProblems1 = new TestParamWithResult(
//                "Massive problems 1",
//                50,
//                0.1,
//                0.1,
//                150000,
//                100000,
//                10,
//                1
//        );
//
//        massiveProblems1.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems1, "massive_problems_1", "Massive Problems 1 (Alg2 only)");
//        massiveProblems1 = null;
//
//        System.out.println("Generating `massive problems 2'");
//        TestParamWithResult massiveProblems2 = new TestParamWithResult(
//                "Massive problems 2",
//                50,
//                0.1,
//                0.1,
//                200000,
//                100000,
//                10,
//                1
//        );
//
//
//        massiveProblems2.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems2, "massive_problems_2", "Massive Problems 2 (Alg2 only)");
//        massiveProblems2 = null;
//
//        System.out.println("Generating `massive problems 3'");
//        TestParamWithResult massiveProblems3 = new TestParamWithResult(
//                "Massive problems 3",
//                50,
//                0.1,
//                0.1,
//                200000,
//                150000,
//                10,
//                1
//        );
//
//        massiveProblems3.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems3, "massive_problems_3", "Massive Problems 3 (Alg2 only)");
//
//        massiveProblems3 = null;

//        System.out.println("Generating `massive problems 4'");
//        TestParamWithResult massiveProblems4 = new TestParamWithResult(
//                "Massive problems 4",
//                50,
//                0.1,
//                0.1,
//                200000,
//                200000,
//                10,
//                1
//        );
//
//        massiveProblems4.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems4, "massive_problems_4", "Massive Problems 4 (Alg2 only)");
//
//
//        System.out.println("Generating `massive problems 5'");
//        TestParamWithResult massiveProblems5 = new TestParamWithResult(
//                "Massive problems 5",
//                50,
//                0.1,
//                0.1,
//                500000,
//                250000,
//                10,
//                1
//        );
//
//        massiveProblems5.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems5, "massive_problems_5", "Massive Problems 5 (Alg2 only)");
//
//
//        System.out.println("Generating `massive problems 6'");
//        TestParamWithResult massiveProblems6 = new TestParamWithResult(
//                "Massive problems 6",
//                50,
//                0.1,
//                0.1,
//                500000,
//                500000,
//                10,
//                1
//        );
//
//        massiveProblems6.runSolverAlg2();
//        writeAllSingleTestParamResultAlg2(massiveProblems5, "massive_problems_6", "Massive Problems 6 (Alg2 only)");

//        massiveProblems1.runSolvers();
//        writeAllSingleTestParamResult(massiveProblems1, "massive_problems_1", "Massive Problems 1");
//        massiveProblems1 = null;
//
//        TestParamWithResult massiveProblems2 = new TestParamWithResult(
//                "Massive problems 2",
//                50,
//                0.1,
//                0.1,
//                1000000,
//                400000,
//                20,
//                3
//        );
//
//        massiveProblems2.runSolvers();
//        writeAllSingleTestParamResult(massiveProblems2, "massive_problems_2", "Massive Problems 2");
//        massiveProblems2 = null;
//
//        TestParamWithResult massiveProblems3 = new TestParamWithResult(
//                "Massive problems 3",
//                50,
//                0.1,
//                0.1,
//                1000000,
//                1000000,
//                20,
//                3
//        );
//
//        massiveProblems3.runSolvers();
//        writeAllSingleTestParamResult(massiveProblems3, "massive_problems_3", "Massive Problems 3");


//
//        galacticProblems.runSolvers();
//        writeAllSingleTestParamResult(hugeProblems3, "galactic_problems", "Galactic Problems");
















    }

    private static void runAllGiven(List<TestParamWithResult>... tests) {
        for (int i = 0; i < tests.length; i++) {
            for (TestParamWithResult tpr : tests[i]) {
                tpr.runSolvers();
            }
        }
    }

    private static void testAll_WinsCounts(List<TestParamWithResult> testParamWithResults) {
        for (TestParamWithResult tpr : testParamWithResults) {
            test_WinsCount(tpr);
        }
    }

    private static void test_WinsCount(TestParamWithResult test) {
        System.out.println("WINS COUNT FOR TEST: " + test.testsName);

        List<Long> timeDiffA1 = test.getExecTimesListAlg1();
        List<Long> timeDiffA2 = test.getExecTimesListAlg2();
        List<Map<String, Long>> solA1 = test.getResultsListAlg1();
        List<Map<String, Long>> solA2 = test.getResultsListAlg2();

        int problemCount = test.getProblemCount();


        int alg1Wins = 0;
        int alg2Wins = 0;

        int differences = 0;

        double ratioMin = 999;
        double ratioMax = 0;


        for (int i = 0; i < problemCount; i++) {
            long t1 = timeDiffA1.get(i);
            long t2 = timeDiffA2.get(i);

            double ratio = ((double) t1) / ((double) t2);

            if (t1 < t2) alg1Wins++;
            if (t2 < t1) alg2Wins++;

            if (0 < ratio && ratio < 1000) {
                if (ratio < ratioMin) ratioMin = ratio;
                if (ratio > ratioMax) ratioMax = ratio;
            }
            //System.out.println("Ratio: " + ratio);
            //System.out.println("Diff: " + (t1 - t2));

            if (!checkResultsEqual(solA1.get(i), solA2.get(i))) differences++;

        }

        System.out.println("Alg 1 Wins: " + alg1Wins);
        System.out.println("Alg 2 Wins: " + alg2Wins);
        System.out.println("Best speedup A1/A2 " + (1.0/ratioMin) + ", A2/A1 " + ratioMax);
        System.out.println(differences != 0? "DIFFERENT RESULTS: " + differences + "\n" : "\n");
    }


    private static void solveTimeConstraintsBest(int repeatSingleTestTimes, List<TimeConstraintSet> problemsList, GraphSolverFactory graphSolverFactory, List<Map<String, Long>> solutionList, List<Long> timeDiffList) {

        int numberOfTests = problemsList.size();


        for (int i = 0; i < numberOfTests; i++) {
            long bestTime = Long.MAX_VALUE;
            Map<String, Long> bestSolution = null;
            TimeConstraintSet tcs = problemsList.get(i);

            for (int j = 0; j < repeatSingleTestTimes; j++) {
                GraphSolvedTimeConstraintSet gstcs = tcs.toGraph();
                long startTime = System.nanoTime();
                Map<String, Long> r = gstcs.solve(graphSolverFactory);
                long endTime = System.nanoTime();
                long timeDiff = endTime - startTime;

                if (timeDiff < bestTime) {
                        bestTime = timeDiff;
                        bestSolution = r;
                }

            }
            solutionList.add(bestSolution);
            timeDiffList.add(bestTime);
        }




    }


    private static void solveTimeConstraintsBest2(String testsName, int repeatSingleTestTimes, List<TimeConstraintSet> problemsList, GraphSolverFactory graphSolverFactory, List<Map<String, Long>> solutionList, List<Long> timeDiffList, List<Map<String, Object>> algorithmInternalData) {
        // Outer loop is repeat, inner loop is test - runs through all tests as batch, then through all tests as batch, then...
        int numberOfTests = problemsList.size();


        // initialise all times to maximum, null solution
        long[] timeDiffs = new long[numberOfTests];
        Map[] sols = new Map[numberOfTests];

        for (int j = 0; j < numberOfTests; j++) {
            timeDiffs[j] = Long.MAX_VALUE;
            sols[j] = null;
        }
        System.out.println("Starting tests: " + testsName);

        for (int i = 0; i < repeatSingleTestTimes; i++) {
            System.out.println("\tStarting repeat " + i + "/" + repeatSingleTestTimes + " of " + testsName);

            for (int j = 0; j < numberOfTests; j++) {
                System.out.println("\t\tStarting repeat " + j + "/" + numberOfTests + " of " + testsName + " iteration " + i);
                TimeConstraintSet tcs = problemsList.get(j);
                GraphSolvedTimeConstraintSet gstcs = tcs.toGraph();

                long startTime = System.nanoTime();
                Map<String, Long> r = gstcs.solve(graphSolverFactory);
                long endTime = System.nanoTime();
                long timeDiff = endTime - startTime;

                long currBest = timeDiffs[j];

                if (timeDiff < currBest) {
                    timeDiffs[j] = timeDiff;
                    sols[j] = r;
                }
                algorithmInternalData.add(gstcs.getAlgorithmInternalData());
            }

        }

        for (int j = 0; j < numberOfTests; j++) {
            solutionList.add(sols[j]);
            timeDiffList.add(timeDiffs[j]);
        }

    }

    private static void solveTimeConstraintsBestAlternating(String testName,
                                                            int repeatSingleTestTimes,
                                                            List<TimeConstraintSet> problemsList,
                                                            List<Map<String, Long>> solutionList1,
                                                            List<Long> timeDiffList1,
                                                            List<Map<String, Long>> solutionList2,
                                                            List<Long> timeDiffList2,
                                                            List<Map<String, Object>> algorithmInternal1,
                                                            List<Map<String, Object>> algorithmInternal2) {
        System.out.println("Starting tests: " + testName);

        int numberOfTests = problemsList.size();

        long[] timeDiffs1 = new long[numberOfTests];
        Map[] sols1 = new Map[numberOfTests];
        long[] timeDiffs2 = new long[numberOfTests];
        Map[] sols2 = new Map[numberOfTests];

        for (int j = 0; j < numberOfTests; j++) {
            timeDiffs1[j] = Long.MAX_VALUE;
            sols1[j] = null;
            timeDiffs2[j] = Long.MAX_VALUE;
            sols2[j] = null;
        }

        GraphSolverFactory a1 = GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1;
        GraphSolverFactory a2 = GraphSolverFactory.REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2;

        boolean m = true;
        for (int i = 0; i < repeatSingleTestTimes; i++) {
            System.out.println("Tests: " + testName + ", repeat " + (i + 1) + "/" + repeatSingleTestTimes);
            for (int j = 0; j < numberOfTests; j++) {
                System.out.println("Tests: " + testName + " Iteration "  + i + "/" + repeatSingleTestTimes + " Test " + j + " of " + numberOfTests);
                TimeConstraintSet tcs = problemsList.get(j);
                GraphSolvedTimeConstraintSet gstcs1 = tcs.toGraph();
                GraphSolvedTimeConstraintSet gstcs2 = tcs.toGraph();

                // if (rnd.nextDouble() < 0.5) {
                if (m) {
                    //System.out.println("- Alg1, Alg2");
                    runTestAlternating(gstcs1, gstcs2, a1, a2, timeDiffs1, timeDiffs2, sols1, sols2, j);
                } else {
                    //System.out.println("- Alg2, Alg1");
                    runTestAlternating(gstcs2, gstcs1, a2, a1, timeDiffs2, timeDiffs1, sols2, sols1, j);
                }
                m = !m;

                algorithmInternal1.add(gstcs1.getAlgorithmInternalData());
                algorithmInternal2.add(gstcs2.getAlgorithmInternalData());

            }
        }

        for (int j = 0; j < numberOfTests; j++) {
            solutionList1.add(sols1[j]);
            timeDiffList1.add(timeDiffs1[j]);
            solutionList2.add(sols2[j]);
            timeDiffList2.add(timeDiffs2[j]);
        }


        System.out.println("Tests " + testName + " completed.");

    }



    private static void runTestAlternating(GraphSolvedTimeConstraintSet gstcs1,
                                           GraphSolvedTimeConstraintSet gstcs2,
                                           GraphSolverFactory gsf1,
                                           GraphSolverFactory gsf2,
                                           long[] timeDiffs1,
                                           long[] timeDiffs2,
                                           Map[] sols1,
                                           Map[] sols2,
                                           int j) {
        long startTime1 = System.nanoTime();
        //System.out.println("-- first starting");
        Map<String, Long> r1 = gstcs1.solve(gsf1);
        long endTime1 = System.nanoTime();

        long startTime2 = System.nanoTime();
        //System.out.println("-- first done, second starting");
        Map<String, Long> r2 = gstcs2.solve(gsf2);
        long endTime2 = System.nanoTime();

        long timeDiff1 = endTime1 - startTime1;
        long timeDiff2 = endTime2 - startTime2;


        long currBest1 = timeDiffs1[j];
        long currBest2 = timeDiffs2[j];

        if (timeDiff1 < currBest1) {
            timeDiffs1[j] = timeDiff1;
            sols1[j] = r1;
        }

        if (timeDiff2 < currBest2) {
            timeDiffs2[j] = timeDiff2;
            sols2[j] = r2;
        }
    }


    // For each: 50 tests of each type, averaged
    // each test of each type is run 5x and take the lowest

    // We really have to split tests by whether or not they had a solution, i think

    private static int incrementTestRepeat = 5;
    private static int incrementTestsPerType = 30;
    private static int graphPoints = 30;


    private static void writeGraphVerticesConstraintsAverages(List<TestParamWithResult> testParamWithResultList, String fileName, String graphTitle) throws IOException {
        File graphDataDir = new File(GRAPH_PATH);
        if (!graphDataDir.isDirectory()) graphDataDir.mkdir();

        List<TestParamWithResult> successes = testParamWithResultList.stream().map(TestParamWithResult::getSuccessfulTestParamWithResult).collect(Collectors.toList());
        List<TestParamWithResult> failures = testParamWithResultList.stream().map(TestParamWithResult::getFailedTestParamWithResult).collect(Collectors.toList());

        FileWriter fwSuccesses = new FileWriter(new File(GRAPH_PATH + fileName + "_HAS_SOLUTION.csv"));
        FileWriter fwFailures = new FileWriter(new File(GRAPH_PATH + fileName + "_NO_SOLUTION.csv"));


        fwSuccesses.write(graphTitle + " (problems with solution only)\n");
        fwSuccesses.write(TestParamWithResult.AVERAGE_HEADERS + "\n");


        for (int i = 0; i < successes.size(); i++) {
            TestParamWithResult tpr = successes.get(i);
            tpr.writeAverages(fwSuccesses);
        }

        fwFailures.write(graphTitle + " (problems without solution only)\n");
        fwFailures.write(TestParamWithResult.AVERAGE_HEADERS + "\n");

        for (int i = 0; i < failures.size(); i++) {
            TestParamWithResult tpr = failures.get(i);
            tpr.writeAverages(fwFailures);
        }

        fwSuccesses.close();
        fwFailures.close();

    }



    private static void writeAllSingleTestParamResult(TestParamWithResult tpr, String fileName, String graphTitle) throws IOException {
        File graphDataDir = new File(GRAPH_PATH);

        TestParamWithResult success = tpr.getSuccessfulTestParamWithResult();
        TestParamWithResult failure = tpr.getFailedTestParamWithResult();

        if (!graphDataDir.isDirectory()) graphDataDir.mkdir();

        FileWriter fwSuccesses = new FileWriter(new File(GRAPH_PATH + fileName + "_HAS_SOLUTION.csv"));
        FileWriter fwFailures = new FileWriter(new File(GRAPH_PATH + fileName + "_NO_SOLUTION.csv"));

        fwSuccesses.write(graphTitle + " (problems with solution only)\n");

        success.writeComplete(fwSuccesses);

        fwFailures.write(graphTitle + " (problems without solution only)\n");
        failure.writeComplete(fwFailures);


        fwSuccesses.close();
        fwFailures.close();

    }

    private static void writeAllSingleTestParamResultAlg2(TestParamWithResult tpr, String fileName, String graphTitle) throws IOException {
        File graphDataDir = new File(GRAPH_PATH);

        TestParamWithResult success = tpr.getSuccessfulTestParamWithResultAlg2();
        TestParamWithResult failure = tpr.getFailedTestParamWithResultAlg2();

        if (!graphDataDir.isDirectory()) graphDataDir.mkdir();

        FileWriter fwSuccesses = new FileWriter(new File(GRAPH_PATH + fileName + "ALG2_ONLY_HAS_SOLUTION.csv"));
        FileWriter fwFailures = new FileWriter(new File(GRAPH_PATH + fileName + "ALG2_ONLY_NO_SOLUTION.csv"));

        fwSuccesses.write(graphTitle + " (problems with solution only, ALG2_ONLY)\n");

        success.writeCompleteAlg2(fwSuccesses);

        fwFailures.write(graphTitle + " (problems without solution only, ALG2_ONLY)\n");
        failure.writeCompleteAlg2(fwFailures);


        fwSuccesses.close();
        fwFailures.close();

    }


    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatioHalf_Small() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(0.5, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatioThreeQuarters_Small() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(0.75, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio1_Small() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(1, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio2_Small() {
        // Tests for 2x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(2, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio3_Small() {
        // Tests for 3x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(3, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio4_Small() {
        // Tests for 4x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(4, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio5_Small() {
        // Tests for 5x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(5, 10, 210);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio10_Small() {
        // Tests for 10x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(10, 10, 210);
    }


    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatioHalf_Large() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(0.5, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatioThreeQuarters_Large() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(0.75, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio1_Large() {
        // Tests for vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(1, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio2_Large() {
        // Tests for 2x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(2, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio3_Large() {
        // Tests for 3x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(3, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio4_Large() {
        // Tests for 4x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(4, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio5_Large() {
        // Tests for 5x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(5, 100, 2000);
    }

    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio10_Large() {
        // Tests for 10x vertex count = constraint count, increase both
        return createTCSListWithIncrementConstraintsVerticesRatio_Create(10, 100, 2000);
    }



    static List<TestParamWithResult> createTCSListWithIncrementConstraintsVerticesRatio_Create(double constraintsPerVertex, int startingVerticesAndConstraints, int endingVerticesAndConstraints) {
        List<TestParamWithResult> ts = new ArrayList<>();
        for (int i = startingVerticesAndConstraints; i <= endingVerticesAndConstraints; i += ((endingVerticesAndConstraints - startingVerticesAndConstraints) / graphPoints)) {
            ts.add(new TestParamWithResult("Increment Vertices and Constraints in ratio 1V:" + constraintsPerVertex + "C, starting " + startingVerticesAndConstraints + "C&V, ending " + endingVerticesAndConstraints + "C&V",
                    20,
                    0.1,
                    0.1,
                    i,
                    (int) (i * constraintsPerVertex),
                    incrementTestsPerType,
                    incrementTestRepeat));
        }

        return ts;
    }

    private static boolean checkResultsEqual(Map<String,Long> sol1, Map<String, Long> sol2) {
        if (sol1 == null) return sol2 == null;
        return sol1.equals(sol2);
    }
}



/*      TEST BATCH 1: tiny, small, standard, large
        TestParamWithResult tinyProblems1 = new TestParamWithResult(
                "Tiny problems 1",
                5,
                0.1,
                0.1,
                8,
                8,
                10000,
                20
        );

        TestParamWithResult tinyProblems2 = new TestParamWithResult(
                "Tiny problems 2",
                5,
                0.1,
                0.1,
                8,
                15,
                10000,
                20
        );

        TestParamWithResult tinyProblems3 = new TestParamWithResult(
                "Tiny problems 3",
                5,
                0.1,
                0.1,
                10,
                20,
                10000,
                20
        );

        TestParamWithResult smallProblems1 = new TestParamWithResult(
                "Small problems 1",
                10,
                0.1,
                0.1,
                20,
                15,
                1000,
                20
        );

        TestParamWithResult smallProblems2 = new TestParamWithResult(
                "Small problems 2",
                10,
                0.1,
                0.1,
                20,
                20,
                1000,
                20
        );

        TestParamWithResult smallProblems3 = new TestParamWithResult(
                "Small problems 3",
                10,
                0.1,
                0.1,
                20,
                30,
                1000,
                20
        );


        TestParamWithResult standardProblems1 = new TestParamWithResult(
                "Standard problems 1",
                40,
                0.1,
                0.1,
                100,
                70,
                500,
                20
        );
        TestParamWithResult standardProblems2 = new TestParamWithResult(
                "Standard problems 2",
                40,
                0.1,
                0.1,
                100,
                100,
                500,
                20
        );
        TestParamWithResult standardProblems3 = new TestParamWithResult(
                "Standard problems 3",
                40,
                0.1,
                0.1,
                100,
                150,
                500,
                20
        );

        TestParamWithResult largeProblems1 = new TestParamWithResult(
                "Large problems 1",
                50,
                0.1,
                0.1,
                1000,
                700,
                500,
                10
        );

        TestParamWithResult largeProblems2 = new TestParamWithResult(
                "Large problems 2",
                50,
                0.1,
                0.1,
                1000,
                1000,
                500,
                10
        );

        TestParamWithResult largeProblems3 = new TestParamWithResult(
                "Large problems 3",
                50,
                0.1,
                0.1,
                1000,
                2000,
                500,
                10
        );
 */