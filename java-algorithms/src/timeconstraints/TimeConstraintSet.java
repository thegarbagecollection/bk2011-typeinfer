package timeconstraints;

import graph.Graph;
import net.sf.javailp.Result;
import net.sf.javailp.SolverFactory;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TimeConstraintSet {
    List<TimeConstraint> constraints;
    Set<String> variableNames;

    public List<TimeConstraint> getConstraints() {
        return constraints;
    }

    public void setConstraints(List<TimeConstraint> constraints) {
        this.constraints = constraints;
    }

    public TimeConstraintSet(List<TimeConstraint> constraints) {
        this.constraints = constraints;
        variableNames = new HashSet<>();
        fillVariableNames();
    }

    private void fillVariableNames() {
        for (TimeConstraint tc : constraints) {
            tc.addVariableNames(variableNames);
        }
    }


    public Set<String> getVariableNames() {
        return variableNames;
    }

    public void printConstraints() {
        constraints.forEach(c -> System.out.println("\t" + c.toString()));
    }

/*
    public void printVariableValues(LPSolution lps) {
        variableNames.forEach(v -> {
            System.out.println("\t" + v + ": " + lps.getInteger(v));
        });
    }
*/

    public void printVariableValues(Result result) {
        variableNames.forEach(v -> {
            System.out.println("\t" + v + ": " + result.get(v));
        });
    }

    public ILPSolvedTimeConstraintSet toILP(SolverFactory solverFactory) {
        return new ILPSolvedTimeConstraintSet(this, solverFactory, TimeConstraint.createILPConstraints(constraints));
    }


    public GraphSolvedTimeConstraintSet toGraph() {
        Graph g = TimeConstraint.buildGraph(constraints);

        return new GraphSolvedTimeConstraintSet(this, g);
    }

}
