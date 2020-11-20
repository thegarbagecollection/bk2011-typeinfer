import graph.GraphSolverFactory;
import timeconstraints.TimeConstraintExamples;

public class Main {
    public static void main(String[] args) {
        //GraphSolverFactory a1 = GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1;
        //GraphSolverFactory a1 = GraphSolverFactory.BROKEN_TEST_SOLVER_FACTORY;
        //GraphSolverFactory a1 = GraphSolverFactory.DEPENDENT_SOLVER_FACTORY;
        //GraphSolverFactory a1 = GraphSolverFactory.DEPENDENT_SOLVER_FACTORY_CLEANED;
        GraphSolverFactory a2 = GraphSolverFactory.REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2;


//        TimeConstraintExamples.edgeTest.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_4_1.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_4_2.writeProblemGraphs(a1);
//
//
//        TimeConstraintExamples.ex_1_4_3.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_4_4.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_4_5.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_4_6.writeProblemGraphs(a1);
//
//
//        TimeConstraintExamples.ex_1_4_7.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_1.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_2.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_3.writeProblemGraphs(a1);
//        TimeConstraintExamples.ex_1_5_4.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_5.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_6.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_7.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.ex_1_5_8.writeProblemGraphs(a1);
//
//
//        TimeConstraintExamples.failing_1.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_1_mod.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_2.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_3.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_4.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_5.writeProblemGraphs(a1);
//
//        TimeConstraintExamples.failing_6.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_7.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_8.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_9.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_10.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_11.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_12.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_13.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_14.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_15.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_16.writeProblemGraphs(a1);
//        TimeConstraintExamples.failing_17.writeProblemGraphs(a1);




/*
        TimeConstraintExamples.examples.runBatch(a1);
        TimeConstraintExamples.failings.runBatch(a1);
        TimeConstraintExamples.all.runBatch(a1);



        System.out.println(TimeConstraintExamples.failing_1.solveOne(a1));
        System.out.println(TimeConstraintExamples.failing_1_mod.solveOne(a1));


        System.out.println(TimeConstraintExamples.failing_2.solveOne(a1));

        TimeConstraintExamples.failing_2.writeProblemGraphs(a1);


        System.out.println(TimeConstraintExamples.failing_3.solveOne(a1));
        System.out.println(TimeConstraintExamples.failing_4.solveOne(a1));


        System.out.println(TimeConstraintExamples.failing_5.solveOne(a1));

        TimeConstraintExamples.failing_5.writeProblemGraphs(a1);


        System.out.println(TimeConstraintExamples.ex_1_5_3.solveOne(a1));
        System.out.println(TimeConstraintExamples.failing_6.solveOne(a1));






        TimeConstraintExamples.failing_6.writeProblemGraphs(a1);

        TimeConstraintExamples.failing_10.writeProblemGraphs(a1);
        TimeConstraintExamples.failing_11.writeProblemGraphs(a1);

        TimeConstraintExamples.failing_11.writeProblemGraphs(a1);

        System.out.println(TimeConstraintExamples.failing_10.solveOne(a1));
        System.out.println(TimeConstraintExamples.failing_11.solveOne(a1));
        System.out.println(TimeConstraintExamples.failing_13.solveOne(a1));
*/



//        List<List<TimeConstraint>> tcs = RandomTester.createRandomTimeConstraints(50, 1000, 10);
//
//        for (int i = 0; i < tcs.size(); i++) {
//            new ProblemComparison(tcs.get(i), "Random_" + i).writeProblemGraphs(a1);
//        }
//
//        Algorithm_ByDependentVertex.printTemp();


//          RandomTester.runMultiTest(4, 10, 50, a1);
//          RandomTester.runMultiTest(3, 3, 2000, a1);
//          RandomTester.runMultiTest(50, 500, 20, a1);
//         RandomTester.runMultiTest(100, 500, 10, a1);

//        System.out.println(TimeConstraintExamples.failing_16.solveOne(a1));
//        System.out.println(TimeConstraintExamples.failing_18.solveOne(a1));


        //System.out.println(TimeConstraintExamples.failing_12.solveOne(a2));


//        List<ProblemComparison> all = new ArrayList<>(TimeConstraintExamples.allExamples);
//        all.addAll(TimeConstraintExamples.allFailings);
//
//        for (ProblemComparison pc : all) {
//            new AlgorithmComparer(pc, GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1, GraphSolverFactory.DEPENDENT_SOLVER_FACTORY).run();
//        }
//
//        List<List<TimeConstraint>> tcs = RandomTester.createRandomTimeConstraints(5, 20, 100);
//
//        for (int i = 0; i < tcs.size(); i++) {
//            new AlgorithmComparer(new ProblemComparison(tcs.get(i), "Random_" + i), GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1, GraphSolverFactory.DEPENDENT_SOLVER_FACTORY).run();
//        }




        //new AlgorithmComparer(TimeConstraintExamples.ex_1_5_8, GraphSolverFactory.FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1, GraphSolverFactory.BROKEN_TEST_SOLVER_FACTORY).run();


        //RandomTester.runMultiTest(4, 10, 50, a2);
//          RandomTester.runMultiTest(3, 3, 2000, a2);
//          RandomTester.runMultiTest(50, 200, 20, a2);
//         RandomTester.runMultiTest(100, 500, 5, a2);
//        RandomTester.runMultiTest(20, 100, 100, a2);
//        RandomTester.runMultiTest(20, 60, 200, a2);
        RandomTester.runMultiTest(50, 30, 200, a2);
    }
}
