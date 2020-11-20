package timeconstraints;

import net.sf.javailp.*;

import java.util.HashMap;
import java.util.Map;

public class ILPSolvedTimeConstraintSet {
    public static SolverFactory CURRENT_SOLVERFACTORY = new SolverFactoryLpSolve();
    {
        CURRENT_SOLVERFACTORY.setParameter(Solver.VERBOSE, 0);
        CURRENT_SOLVERFACTORY.setParameter(Solver.TIMEOUT, 10); // short timeout for testing!
    }

    Problem problem;
    TimeConstraintSet tcs;
    SolverFactory solverFactory;

    Result result;

    public TimeConstraintSet getTcs() {
        return tcs;
    }

    public ILPSolvedTimeConstraintSet(TimeConstraintSet tcs, SolverFactory solverFactory, Problem problem) {
        this.problem = problem;
        this.tcs = tcs;
        this.solverFactory = solverFactory;
    }

    public void runSolver() {
        Solver solver = solverFactory.get();
        result = solver.solve(problem);
    }

    public Map<String, Long> getSolution() {
        // In javailp, no solution is result=null or result.objective=null... i think???
        if (result == null || result.getObjective() == null) {
            // no solution
            return null;
        }
        else {
            Map<String, Long> copiedResult = new HashMap<>(); // we're still going with longs to avoid having to change all the signatures
            for (String variable : tcs.variableNames) {
                Integer varVal = (Integer) result.get(variable); // hopefully this works!
                copiedResult.put(variable, varVal.longValue());
            }

            return copiedResult;
        }
    }

    public void showResult() {

        System.out.println("\n\n**************************************************************************");

        if (result == null || result.getObjective() == null) {
            System.out.println("\nNO SOLUTION FOUND.");
        }
        else {
            System.out.println("Constraint set:");
            tcs.printConstraints();
            System.out.println("Variable assignments:");
            tcs.printVariableValues(result);
            System.out.println("Solution found.");
        }

        System.out.println("\n\n");
    }


    /*
    LPWizard lpw;

    LPSolution lps;



    public ILPSolvedTimeConstraintSet(TimeConstraintSet tcs, LPWizard lpw) {
        this.tcs = tcs;
        this.lpw = lpw;
    }

    public void runSolver() {
        lps = lpw.solve();
    }

    public Map<String, Long> getSolution() {
        Map<String, Long> results = new HashMap<>();
        try {
            for (String variable : tcs.variableNames) {
                results.put(variable, lps.getInteger(variable));
            }
            return results;
        }
        catch (Exception e) {
            return null;
        }
    }

    public void showResult() {

        System.out.println("\n\n**************************************************************************");

        // They haven't actually provided a way to detect when the constraint set was unsolvable. Hmm.

        try {
            System.out.println("Constraint set:");
            tcs.printConstraints();
            System.out.println("Variable assignments:");
            tcs.printVariableValues(lps);
            System.out.println("Solution found.");
        } catch (Exception e){
            System.out.println("\nNO SOLUTION FOUND.");
        }

        System.out.println("\n\n");
    }
    */


}
