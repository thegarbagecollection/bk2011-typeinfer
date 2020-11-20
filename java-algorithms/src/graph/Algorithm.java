package graph;

import timeconstraints.GraphSolvedTimeConstraintSet;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class Algorithm {
    ComponentGraph g;
    Map<ComponentVertex, Integer> tempVertexWeights = new HashMap<>();
    Map<Variable, Integer> tempEdgeWeights = new HashMap<>();

    public Map<ComponentVertex, Integer> getTempVertexWeights() {
        return tempVertexWeights;
    }

    public Map<Variable, Integer> getTempEdgeWeights() {
        return tempEdgeWeights;
    }

    Map<String, Object> algorithmInternalData = new HashMap<>();


    public Map<String, Object> getAlgorithmInternalData() {
        return algorithmInternalData;
    }

    public Algorithm(ComponentGraph g) {
        this.g = g;
    }

    /*
            public Result firstPartDebug() {
                if (!checkSelfLoops()) return Result.FAILURE_SELF_LOOP;

                if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

                return Result.SUCCESS;
            }

            public Result secondPartDebug(String fileName) {
                return checkPathLengths(fileName, true);
            }

            public Result run() {
                if (!checkSelfLoops()) return Result.FAILURE_SELF_LOOP;

                if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

                return checkPathLengths("", false);
            }
        */
    protected boolean reduceMultiEdges() {
            return g.reduceMultiEdges();
        }

    protected boolean checkSelfLoops() {
        for (Set<Edge<Vertex>> edgeSet : g.selfLoops.values()) {
            for (Edge<Vertex> edge : edgeSet) {
                if (!edge.isValidForSelfLoop()) return false;
                edge.setWeightDirect(0); // all selfloop edges must be of weight 0 in the original graph and its reversed
            }
        }
        return true;
    }

    protected void saveCurrentWeights(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights) {
        tempEdgeWeights = edgeVariableWeights;
        tempVertexWeights = vertexWeights;
    }

    public void assignFailedWeightsToGraph() {
        assignWeightsToGraph(tempVertexWeights, tempEdgeWeights);
    }

    protected void assignWeightsToGraph(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights) {
        vertexWeights.forEach((v, i) -> {
            v.weight = i;
            v.componentContents.forEach(v2 -> v2.weight = i);
        });

        g.edges.forEach(edge -> {
            edge.setWeightFromMap(edgeVariableWeights);
            g.componentEdgeToOriginals.get(edge).forEach(origEdge -> {
                origEdge.setWeightFromOtherEdgeAndAssociatedVariableWeights(edge, edgeVariableWeights);
            });
        });
    }

    protected void printIntermediates2(boolean isDebug, Map<ComponentVertex, Integer> vertexWeights, String fileName, Map<Variable, Integer> edgeVariableWeights, int iterationCount, String intermediateLabel) {
        if (!isDebug) return;

        g.writeGraphVizUnstyledWithWeights(fileName + "_4_iter_" + iterationCount + "_" + intermediateLabel + ".dotgraph", vertexWeights, edgeVariableWeights);
    }

    protected void printIntermediatesGeneral(boolean isDebug,
                                             String fileName,
                                             int iterationCount,
                                             String intermediateLabel,
                                             Map<String, Map<? extends Vertex, ? extends Object>> vertexWeights,
                                             Map<Variable, Integer> primaryEdgeVariableWeights,
                                             Map<String, Map<Variable, ? extends Object>> auxiliaryEdgeVariableWeights,
                                             Map<String, Map<Edge<? extends Vertex>, ? extends Object>> extraEdgeWeights,
                                             Map<? extends Vertex, Map<String, String>> vertexStyling,
                                             Map<Edge<? extends Vertex>, Map<String, String>> edgeStyling) {
        if (!isDebug) return;

        g.writeGraphVizStyledWithWeights(fileName + "_4_iter_" + iterationCount + "_" + intermediateLabel + ".dotgraph",
                vertexWeights,
                primaryEdgeVariableWeights,
                auxiliaryEdgeVariableWeights,
                extraEdgeWeights,
                vertexStyling,
                edgeStyling);
    }


    public enum Result {
        FAILURE_SELF_LOOP("Failure due to self-loop of non-zero and non-variable weight."),
        FAILURE_ADJACENT_DIFFERENT_WEIGHTS("Failure due to two adjacent edges of different fixed weight."),
        FAILURE_UNASSIGNED_VERTEX_OR_EDGE_WEIGHTS("Failure due to vertex or edge weights ending up not assigned on termination. THIS SHOULDN'T HAPPEN!"),
        FAILURE_NO_PROGRESS("Failure due to no progress - algorithm got stuck."),
        FAILURE_DIFFERENT_PATH_LENGTHS("Failure due to different path lengths in reduced SCC graph."),
        FAILURE_IMPOSSIBLE_EDGE_WEIGHT("Failure due to a variable edge needing negative weight"),
        SUCCESS("Operation completed successfully.");

        String _message;

        Result(String message) {
            this._message = message;
        }

        public String message() {
            return _message;
        }
    }

    public Result runComplete() {
        if (!checkSelfLoops()) return Result.FAILURE_SELF_LOOP;

        if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_H_VERTICES, g.getVertexCount());
        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_H_EDGES, g.getEdgeCount());

        Result r = checkPathLengths("", false);


        return r;
    }

    public Result debug_runToSCC() {
        if (!checkSelfLoops()) return Result.FAILURE_SELF_LOOP;

        if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

        return Result.SUCCESS;
    }

    public Result debug_runFromSCCToComplete(String fileName) {
        return checkPathLengths(fileName, true);
    }

    abstract Result checkPathLengths(String fileName, boolean isDebug);

    void vertexIncrementCount() {
        if (!algorithmInternalData.containsKey(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS)) {
            algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS, 1L);
        }
        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS, (long) algorithmInternalData.get(GraphSolvedTimeConstraintSet.KEY_VERTEX_INCREMENTS) + 1);
    }
}
