package timeconstraints;

import graph.*;

import java.util.StringJoiner;

public class AlgorithmComparer {
    TimeConstraintSet tcs;

    GraphSolverFactory algorithm1Fact;
    GraphSolverFactory algorithm2Fact;

    String name;

    public AlgorithmComparer(ProblemComparison pc, GraphSolverFactory algorithm1Fact, GraphSolverFactory algorithm2Fact) {
        tcs = pc.tcs;
        this.algorithm1Fact = algorithm1Fact;
        this.algorithm2Fact = algorithm2Fact;
        name = pc.name;
    }

    public boolean run() {
        Graph<Vertex> g1 = TimeConstraint.buildGraph(tcs.constraints);
        Graph<Vertex> g2 = TimeConstraint.buildGraph(tcs.constraints);

        ComponentGraph cg1 = new SCC().runSCCWithReversedEdges(g1);
        ComponentGraph cg2 = new SCC().runSCCWithReversedEdges(g2);

        Algorithm alg1 = algorithm1Fact.create(cg1);
        Algorithm alg2 = algorithm2Fact.create(cg2);

        Algorithm.Result r1 = alg1.runComplete();
        Algorithm.Result r2 = alg2.runComplete();


        System.out.println("Running: " + name);

        boolean success = true;

        if (r1 == Algorithm.Result.SUCCESS && r2 == Algorithm.Result.SUCCESS) {
            if (g1.assignedWeightsMatch(g2)) {
                System.out.println("Correct: results matched.\n");
            }
            else {
                System.out.println("FAILURE: both completed, but answers were not equal.");
                alg1.assignFailedWeightsToGraph();
                alg2.assignFailedWeightsToGraph();

                StringJoiner sj1 = new StringJoiner(", ");
                StringJoiner sj2 = new StringJoiner(", ");
                g1.vertices.forEach(v -> sj1.add(v.toString() + ":" + v.weight));
                g2.vertices.forEach(v -> sj2.add(v.toString() + ":" + v.weight));

                System.out.println("Algorithm 1: " + sj1);
                System.out.println("Algorithm 2: " + sj2);
                System.out.println("\n");
            }
        }
        else if (r1 == Algorithm.Result.SUCCESS && r2 != Algorithm.Result.SUCCESS) {
            System.out.println("FAILURE: 1 return success, 2 returned failure.\n");
            alg1.assignFailedWeightsToGraph();
            alg2.assignFailedWeightsToGraph();

            StringJoiner sj1 = new StringJoiner(", ");
            StringJoiner sj2 = new StringJoiner(", ");
            g1.vertices.forEach(v -> sj1.add(v.toString() + ":" + v.weight));
            g2.vertices.forEach(v -> sj2.add(v.toString() + ":" + v.weight));

            System.out.println("Algorithm 1: " + sj1);
            System.out.println("Algorithm 2: " + sj2);
            System.out.println("\n");
        }
        else if (r1 != Algorithm.Result.SUCCESS && r2 == Algorithm.Result.SUCCESS) {
            System.out.println("FAILURE: 1 return failure, 2 returned success.\n");
            alg1.assignFailedWeightsToGraph();
            alg2.assignFailedWeightsToGraph();

            StringJoiner sj1 = new StringJoiner(", ");
            StringJoiner sj2 = new StringJoiner(", ");
            g1.vertices.forEach(v -> sj1.add(v.toString() + ":" + v.weight));
            g2.vertices.forEach(v -> sj2.add(v.toString() + ":" + v.weight));

            System.out.println("Algorithm 1: " + sj1);
            System.out.println("Algorithm 2: " + sj2);
            System.out.println("\n");
        }
        else if (r1 != Algorithm.Result.SUCCESS && r2 != Algorithm.Result.SUCCESS){
            System.out.println("Correct: both determined no solution.\n");
        }



        return success;
    }
}
