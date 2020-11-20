package timeconstraints;

import graph.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GraphSolvedTimeConstraintSet {
    // key-value mappings:
    // "gVertices" -> integer
    // "gEdges" -> integer
    // "hVertices" -> integer
    // "hEdges" -> integer
    // "rVertices" -> integer
    // "rEdges" -> integer
    // "componentSizesByRank" -> map (rank integer -> [size integer])
    public static String KEY_G_VERTICES = "gVertices";
    public static String KEY_G_EDGES = "gEdges";
    public static String KEY_H_VERTICES = "hVertices";
    public static String KEY_H_EDGES = "hEdges";
    public static String KEY_VERTEX_INCREMENTS = "vertexIncrements";
    public static String KEY_R_VERTICES = "rVertices";
    public static String KEY_R_EDGES = "rEdges";
    public static String KEY_R_COMPONENT_SIZES_BY_RANK = "componentSizesByRank";


    TimeConstraintSet tcs;

    Graph<Vertex> g;

    ComponentGraph componentGraph;

    private Algorithm_ForwardBackward_RepeatPropagation.Result result = null;

    public Algorithm_ForwardBackward_RepeatPropagation.Result getResult() {
        return result;
    }


    Map<String, Object> algorithmInternalData = new HashMap<>();

    public GraphSolvedTimeConstraintSet(TimeConstraintSet timeConstraintSet, Graph<Vertex> g) {
        this.g = g;
        this.tcs = timeConstraintSet;
        this.componentGraph = new SCC().runSCCWithReversedEdges(g);
    }

    public Map<String, Long> solve(GraphSolverFactory algorithmFactory) {


        Algorithm a = algorithmFactory.create(componentGraph);
        result = a.runComplete();

        algorithmInternalData = a.getAlgorithmInternalData();
        algorithmInternalData.put(KEY_G_VERTICES, g.getVertexCount());
        algorithmInternalData.put(KEY_G_EDGES, g.getEdgeCount());

        switch (result) {
            case SUCCESS:
                return assignment();
            default:
                return null;
        }
    }

    public Map<String, Object> getAlgorithmInternalData() {
        return algorithmInternalData;
    }

    public int getGVertexCount() {
        return (int) algorithmInternalData.get(KEY_G_VERTICES);
    }
    public int getGEdgesCount() {
        return (int) algorithmInternalData.get(KEY_G_EDGES);
    }

    /**
     * returns -1 if H never got created due to problem having no solution in H-creation phase
     * @return
     */
    public int getHVertexCount() {
        if (!algorithmInternalData.containsKey(KEY_H_VERTICES)) return -1;
        return (int) algorithmInternalData.get(KEY_H_VERTICES);
    }
    /**
     * returns -1 if H never got created due to problem having no solution in H-creation phase
     * @return
     */
    public int getHEdgesCount() {
        if (!algorithmInternalData.containsKey(KEY_H_EDGES)) return -1;
        return (int) algorithmInternalData.get(KEY_H_EDGES);
    }
    /**
     * returns -1 if R never got created due to problem having no solution in H-creation phase
     * @return
     */
    public int getRVertexCount() {
        if (!algorithmInternalData.containsKey(KEY_R_VERTICES)) return -1;
        return (int) algorithmInternalData.get(KEY_R_VERTICES);
    }
    /**
     * returns -1 if R never got created due to problem having no solution in H-creation phase
     * @return
     */
    public int getREdgesCount() {
        if (!algorithmInternalData.containsKey(KEY_R_EDGES)) return -1;
        return (int) algorithmInternalData.get(KEY_R_EDGES);
    }

    /**
     * returns number of vertex increments up to the failure point; will be -1 if H was never successfully created
     * @return
     */
    public long getVertexIncrementCount() {
        if (!algorithmInternalData.containsKey(KEY_VERTEX_INCREMENTS)) return -1;
        return (long) algorithmInternalData.get(KEY_VERTEX_INCREMENTS);
    }

    /**
     * returns null if R never got created due to problem having no solution in H-creation phase
     * @return
     */
    public Map<Integer, List<Integer>> getComponentSizesByRank() {
        return (Map<Integer, List<Integer>>) algorithmInternalData.get(KEY_R_COMPONENT_SIZES_BY_RANK);
    }

    private Map<String, Long> assignment() {
        return g.assignmentsByLabel(tcs);
    }

    public void writeExpandedProblemGraph(String fileName) {
        g.writeGraphVizUnstyled(fileName);
    }

    public void writeContractedProblemGraph(String fileName) {
        componentGraph.writeGraphVizUnstyled(fileName);
    }

/*
    public void writeFullyCollapsedProblemGraph(String fileName, String problemName) {
        Algorithm a = new Algorithm(componentGraph);
        Algorithm.Result intermediateResult = a.firstPartDebug();
        switch (intermediateResult) {
            case SUCCESS:
                componentGraph.writeGraphVizUnstyled(fileName + "_3_coll.dotgraph");
                Algorithm.Result finalResult = a.secondPartDebug(fileName);
                switch (finalResult) {
                    case SUCCESS:
                        System.out.println(problemName + ": has a graph solution.\n\n");
                        componentGraph.writeGraphVizUnstyled(fileName + "_5_final.dotgraph");
                        g.writeGraphVizUnstyled(fileName + "_6_assignment.dotgraph");
                        break;
                    default:
                        System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 2.\n\t" + finalResult.message() + "\n\n");
                        componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
                }
                break;
            default:
                System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 1.\n\t" + intermediateResult.message() + "\n\n");
                componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
        }

        // This will internally write the solution-in-process; need to update this to actually display the solution

    }
    */
/*
    public void writeFullyCollapsedProblemGraph_v2(String fileName, String problemName) {
        Algorithm a = new Algorithm(componentGraph);
        Algorithm.Result intermediateResult = a.firstPartDebug_v2();
        switch (intermediateResult) {
            case SUCCESS:
                componentGraph.writeGraphVizUnstyled(fileName + "_3_coll.dotgraph");
                Algorithm.Result finalResult = a.secondPartDebug_v2(fileName);
                switch (finalResult) {
                    case SUCCESS:
                        System.out.println(problemName + ": has a graph solution.\n\n");
                        componentGraph.writeGraphVizUnstyled(fileName + "_5_final.dotgraph");
                        g.writeGraphVizUnstyled(fileName + "_6_assignment.dotgraph");
                        break;
                    default:
                        System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 2.\n\t" + finalResult.message() + "\n\n");
                        componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
                }
                break;
            default:
                System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 1.\n\t" + intermediateResult.message() + "\n\n");
                componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
        }

        // This will internally write the solution-in-process; need to update this to actually display the solution

    }
*/
    public void writeFullyCollapsedProblemGraph_v3(String fileName, String problemName, GraphSolverFactory algorithmFactory) {
        Algorithm a = algorithmFactory.create(componentGraph);
        Algorithm.Result intermediateResult = a.debug_runToSCC();
        switch (intermediateResult) {
            case SUCCESS:
                componentGraph.writeGraphVizUnstyled(fileName + "_3_coll.dotgraph");
                Algorithm_ForwardBackward_RepeatPropagation.Result finalResult = a.debug_runFromSCCToComplete(fileName);
                switch (finalResult) {
                    case SUCCESS:
                        System.out.println(problemName + ": has a graph solution.\n\n");
                        //componentGraph.writeGraphVizUnstyled(fileName + "_5_final.dotgraph");
                        //g.writeGraphVizUnstyled(fileName + "_6_assignment.dotgraph");
                        break;
                    default:
                        System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 2.\n\t" + finalResult.message() + "\n\n");
                        a.assignFailedWeightsToGraph();
                        componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
                }
                break;
            default:
                System.out.println(problemName + ": could not write fully collapsed problem graph - algorithm detected failure in part 1.\n\t" + intermediateResult.message() + "\n\n");
                //a.assignFailedWeightsToGraph();
                componentGraph.writeGraphVizUnstyled(fileName + "_5_FAILURE_POINT.dotgraph");
        }

        // This will internally write the solution-in-process; need to update this to actually display the solution

    }

    public void writeReversedOriginalGraph(String fileName) {
        Graph gRev = g.reverse();
        gRev.writeGraphVizUnstyled(fileName);
    }
}
