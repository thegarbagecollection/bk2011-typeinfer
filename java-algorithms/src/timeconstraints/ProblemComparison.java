package timeconstraints;

import graph.Algorithm_ForwardBackward_RepeatPropagation;
import graph.GraphSolverFactory;
import net.sf.javailp.SolverFactory;

import java.util.List;
import java.util.Map;

public class ProblemComparison {
    TimeConstraintSet tcs;
    ILPSolvedTimeConstraintSet ilpTCS;
    GraphSolvedTimeConstraintSet graphTCS;

    String name;

    Map<String, Long> solutionILP;
    Map<String, Long> solutionGraph;


    Algorithm_ForwardBackward_RepeatPropagation.Result error = null;

    public ProblemComparison(List<TimeConstraint> constraints, String name, SolverFactory solverFactory) {
        this.tcs = new TimeConstraintSet(constraints);
        this.ilpTCS = tcs.toILP(solverFactory);
        this.graphTCS = tcs.toGraph();
        this.name = name;
    }

    public ProblemComparison(List<TimeConstraint> constraints, String name) {
        this.tcs = new TimeConstraintSet(constraints);
        this.ilpTCS = tcs.toILP(ILPSolvedTimeConstraintSet.CURRENT_SOLVERFACTORY);
        this.graphTCS = tcs.toGraph();
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public Algorithm_ForwardBackward_RepeatPropagation.Result getError() {
        return error;
    }

    public boolean solveOne(GraphSolverFactory algorithmFactory) {
        ilpTCS.runSolver();
        solutionILP = ilpTCS.getSolution();
        solutionGraph = graphTCS.solve(algorithmFactory);
        error = graphTCS.getResult();
        System.out.println("\nDONE " + name);
        System.out.println("ILP: " + solutionILP);
        System.out.println("Graph: " + solutionGraph + "\t Result: " + error.message());
        return checkEqual(solutionILP, solutionGraph);
    }


    public void batchPrepare() {
        ilpTCS.runSolver();
    }

    public boolean batchRun(GraphSolverFactory algorithmFactory) {
        solutionILP = ilpTCS.getSolution();
        solutionGraph = graphTCS.solve(algorithmFactory);
        error = graphTCS.getResult();

        return checkEqual(solutionILP, solutionGraph);
    }

    private boolean checkEqual(Map<String,Long> sol1, Map<String, Long> sol2) {
        if (sol1 == null) return sol2 == null;
        return sol1.equals(sol2);
    }
/*
    public void writeProblemGraphs() {
        writeExpandedProblemGraph(name + "_1_exp.dotgraph");
        writeContractedProblemGraph(name + "_2_scc.dotgraph");
        writeFullyCollapsedProblemGraph(name , name);
    }

    public void writeProblemGraphs_v2() {
        writeExpandedProblemGraph(name + "_1_exp.dotgraph");
        writeContractedProblemGraph(name + "_2_scc.dotgraph");
        writeFullyCollapsedProblemGraph_v2(name , name);
    }
*/
    public void writeProblemGraphs(GraphSolverFactory algorithmFactory) {
        //writeExpandedProblemGraph(name + "_1_exp.dotgraph");
        //writeReversedOriginalGraph(name + "_1b_rev.dotgraph");
        //writeContractedProblemGraph(name + "_2_scc.dotgraph");
        writeFullyCollapsedProblemGraph_v3(name , name, algorithmFactory);
    }

    private void writeExpandedProblemGraph(String fileName) {
        graphTCS.writeExpandedProblemGraph(fileName);
    }

    private void writeContractedProblemGraph(String fileName) {
        graphTCS.writeContractedProblemGraph(fileName);
    }

    private void writeReversedOriginalGraph(String fileName) {
        graphTCS.writeReversedOriginalGraph(fileName);
    }

/*
    private void writeFullyCollapsedProblemGraph(String fileName, String name) {
        graphTCS.writeFullyCollapsedProblemGraph(fileName, name);
    }

    private void writeFullyCollapsedProblemGraph_v2(String fileName, String name) {
        graphTCS.writeFullyCollapsedProblemGraph_v2(fileName, name);
    }
*/
    private void writeFullyCollapsedProblemGraph_v3(String fileName, String name, GraphSolverFactory algorithmFactory) {
        graphTCS.writeFullyCollapsedProblemGraph_v3(fileName, name, algorithmFactory);
    }
    public TimeConstraintSet getTcs() {
        return tcs;
    }

    public Map<String, Long> getSolutionILP() {
        return solutionILP;
    }

    public Map<String, Long> getSolutionGraph() {
        return solutionGraph;
    }
}
