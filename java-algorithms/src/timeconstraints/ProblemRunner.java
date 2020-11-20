package timeconstraints;

import graph.GraphSolverFactory;

import java.util.ArrayList;
import java.util.List;

public class ProblemRunner {

    List<TimeConstraintSet> failingConstraints = new ArrayList<>();

    List<ProblemComparison> failingProblems = new ArrayList<>();

    List<ProblemComparison> pcs;

    public ProblemRunner(List<ProblemComparison> pcs) {
        this.pcs = pcs;
    }

    public List<TimeConstraintSet> getFailingConstraints() {
        return failingConstraints;
    }

    public boolean runBatch(GraphSolverFactory algorithmFactory) {
        pcs.forEach(ProblemComparison::batchPrepare);

        int failureCount = 0;

        for (ProblemComparison pc : pcs) {
            if (pc.batchRun(algorithmFactory)) {
                System.out.println("Success: " + pc.name);
            } else {
                System.out.println("FAILED: " + pc.name);
                failureCount++;
                failingConstraints.add(pc.tcs);
                failingProblems.add(pc);
            }
        }

        if (failureCount == 0) {
            System.out.println("Completed successfully.");
            return true;
        }
        else {
            System.out.println("********* BATCH CONTAINED FAILURES ******");
            System.out.println("Failures: " + failureCount);
            return false;
        }
    }

    public List<ProblemComparison> getFailingProblems() {
        return failingProblems;
    }


    public ProblemRunner combine(ProblemRunner other) {
        List<ProblemComparison> newPCS = new ArrayList<>();
        newPCS.addAll(this.pcs);
        newPCS.addAll(other.pcs);

        return new ProblemRunner(newPCS);
    }
}
