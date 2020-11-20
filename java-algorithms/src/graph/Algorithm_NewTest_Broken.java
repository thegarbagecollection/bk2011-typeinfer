package graph;

import java.util.*;

public class Algorithm_NewTest_Broken extends Algorithm {
    public Algorithm_NewTest_Broken(ComponentGraph g) {
        super(g);
    }

    @Override
    Result checkPathLengths(String fileName, boolean isDebug) {
        // Here's the plan: set up 'ranks' for each vertex
        // Propagate negative vertex weights along backwards edges through the ranks
        // See what it looks like

        Map<ComponentVertex, Integer> ranks = rankVerticesFromSinks();


        //this.printIntermediates2(isDebug, ranks, fileName, new HashMap<>(), 0, "a_ranks");


        int maxRank = ranks.values().stream().max(Integer::compareTo).get();

        List<List<ComponentVertex>> verticesByRank = new ArrayList<>();
        for (int i = 0; i <= maxRank; i++) {
            verticesByRank.add(new ArrayList<>());
        }

        ranks.forEach((v, rank) -> verticesByRank.get(rank).add(v));

        Map<ComponentVertex, Integer> vertexWeightsBackward = new HashMap<>();


        // First set all rank-0 to 0 weight as an initial
        verticesByRank.get(0).forEach(vertex -> vertexWeightsBackward.put(vertex, 0));

        //this.printIntermediates2(isDebug, vertexWeightsBackward, fileName, new HashMap<>(), 0, "b_initialweight");

        Map<ComponentVertex, PriorityQueue<Edge<ComponentVertex>>> outgoingByDestinationNegativeWeight = new HashMap<>();

        Map<Edge<ComponentVertex>, Integer> edgesBySubtractedWeight = new HashMap<>();

        for (int i = 1; i <= maxRank; i++) {
            List<ComponentVertex> rankedVertices = verticesByRank.get(i);

            for (ComponentVertex v : rankedVertices) {
                /*
                int lowest = 999; // marker - everything will be lower!! and we're not running it on anything with no outgoing edges (i.e. a sink), so should always be set.
                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    int wWeight = vertexWeightsBackward.get(e.to);
                    int newWeight;

                    if (e.isVariableWeight()) newWeight = wWeight;
                    else newWeight = wWeight - ((EdgeWeight.EdgeWeightInteger)e.weight).i;

                    if (newWeight < lowest) lowest = newWeight;
                }
                assert lowest < 999;

                vertexWeightsBackward.put(v, lowest);
                */

                outgoingByDestinationNegativeWeight.put(v, new PriorityQueue<>(Comparator.comparing(edgesBySubtractedWeight::get)));

                int min = 999;

                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    int wWeight = vertexWeightsBackward.get(e.to);
                    int newWeight;

                    if (e.isVariableWeight()) newWeight = wWeight;
                    else newWeight = wWeight - ((EdgeWeight.EdgeWeightInteger) e.weight).i;

                    edgesBySubtractedWeight.put(e, newWeight);

                    outgoingByDestinationNegativeWeight.get(v).offer(e);

                    if (newWeight < min) min = newWeight;

                }
                if (min != 999) {
                    vertexWeightsBackward.put(v, min);
                }

            }

        }


        //this.printIntermediates2(isDebug, vertexWeightsBackward, fileName, new HashMap<>(), 0, "c_minweight");


        // Now let's try propagating forward, in order of ranks from source (should only be a single source, by construction!)


        /*
        Map<ComponentVertex, Integer> ranksForward = rankVerticesFromSources();


        //this.printIntermediates2(isDebug, ranksForward, fileName, new HashMap<>(), 0, "d_ranks_forward");


        maxRank = ranksForward.values().stream().max(Integer::compareTo).get();

        List<List<ComponentVertex>> verticesByRankForward = new ArrayList<>();
        for (int i = 0; i <= maxRank; i++) {
            verticesByRankForward.add(new ArrayList<>());
        }

        ranksForward.forEach((v, rank) -> verticesByRankForward.get(rank).add(v));

        Map<ComponentVertex, Integer> vertexWeightsForward = new HashMap<>();



        // First set all rank-0 to 0 weight as an initial
        verticesByRankForward.get(0).forEach(vertex -> vertexWeightsForward.put(vertex, 0));

        //this.printIntermediates2(isDebug, vertexWeightsForward, fileName, new HashMap<>(), 0, "e_initialweight_forward");

        for (int i = 1; i <= maxRank; i++) {
            List<ComponentVertex> rankedVertices = verticesByRankForward.get(i);

            for (ComponentVertex v : rankedVertices) {
                int highest = -1; // marker - everything will be lower!! and we're not running it on anything with no outgoing edges (i.e. a sink), so should always be set.
                for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                    int wWeight = vertexWeightsForward.get(e.from);
                    int newWeight;

                    if (e.isVariableWeight()) newWeight = wWeight;
                    else newWeight = wWeight + ((EdgeWeight.EdgeWeightInteger)e.weight).i;

                    if (newWeight > highest) highest = newWeight;
                }
                assert highest > -1;

                vertexWeightsForward.put(v, highest);
            }

        }
*/

        Map<String, Map<? extends Vertex, ? extends Object>> allVertexWeights = new HashMap<>();
        allVertexWeights.put("b", vertexWeightsBackward);

        Map<String, Map<Edge<ComponentVertex>, Integer>> tertiaryWeights = new HashMap<>();

        tertiaryWeights.put("o", edgesBySubtractedWeight);



//        printIntermediatesGeneral(isDebug,
//                fileName,
//                0,
//                "f_combined",
//                allVertexWeights,
//                new HashMap<>(),
//                new HashMap<>(),
//                (Map) tertiaryWeights,
//                new HashMap<>(),
//                new HashMap<>()
//                );


        // Let's try following the most negative unassigned path from the root; then after we've set that,
        // set everything that explicitly 'must be' from weighted edges
        // see where we end up

        ComponentVertex root = verticesByRank.get(maxRank).get(0);

/*
        Stack<ComponentVertex> stack = new Stack<>();
        stack.push(root);

        Map<ComponentVertex, Integer> currWeight = new HashMap<>();
        Map<Variable, Integer> currEdgeWeight = new HashMap<>();
        currWeight.put(root, 0);

        Stack<ComponentVertex> toProp = new Stack<>();
        toProp.push(root);

        // Want to propagate using 0 edge variable weights
        while (!stack.isEmpty()) {
            ComponentVertex curr = stack.pop();

            int minValue = 1;
            ComponentVertex next = null;
            Edge<ComponentVertex> nextEdge = null;


            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(curr)) {
                if (vertexWeightsBackward.get(e.to) < minValue) {
                    minValue = vertexWeightsBackward.get(e.to);
                    next = e.to;
                    nextEdge = e;
                }
            }

            if (next != null) {
                stack.push(next);
                toProp.push(next);
                if (nextEdge.isVariableWeight()) {
                    Variable v = ((EdgeWeight.EdgeWeightVariable) nextEdge.weight).v;
                    currEdgeWeight.put(v, 0);
                    currWeight.put(next, currWeight.get(curr));
                } else {
                    int i = ((EdgeWeight.EdgeWeightInteger) nextEdge.weight).i;
                    currWeight.put(next, currWeight.get(curr) + i);
                }
            }
        }


        allVertexWeights.put("curr", currWeight);





        printIntermediatesGeneral(isDebug,
                fileName,
                0,
                "g_combined_2",
                allVertexWeights,
                currEdgeWeight,
                new HashMap<>(),
                (Map) tertiaryWeights,
                new HashMap<>(),
                new HashMap<>()
        );


        // Now let's go backward and forwards from what we just set, setting precisely those vertices attached by a fixed-weight edge
        // we're also starting from the largest such, since it was at the end of a path

        while (!toProp.isEmpty()) {
            ComponentVertex curr = toProp.pop();

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(curr)) {
                if (!e.isVariableWeight()) {
                    int i = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (currWeight.containsKey(e.from) && currWeight.get(e.from) != currWeight.get(curr) - i) {
                        saveCurrentWeights(currWeight, currEdgeWeight);
                        return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                    } else if (!currWeight.containsKey(e.from)) {
                        currWeight.put(e.from, currWeight.get(curr) - i);
                        toProp.push(e.from);
                    }
                }
            }

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(curr)) {
                if (!e.isVariableWeight()) {
                    int i = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if ((currWeight.containsKey(e.to) && currWeight.get(e.to) != currWeight.get(curr) + i)) {
                        saveCurrentWeights(currWeight, currEdgeWeight);
                        return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                    } else if (!currWeight.containsKey(e.to)) {
                        currWeight.put(e.to, currWeight.get(curr) + i);
                        toProp.push(e.to);
                    }
                }
            }
        }

        printIntermediatesGeneral(isDebug,
                fileName,
                0,
                "h_combined_3",
                allVertexWeights,
                currEdgeWeight,
                new HashMap<>(),
                (Map) tertiaryWeights,
                new HashMap<>(),
                new HashMap<>()
        );


*/

        // Ok - got an idea. I think we can get away with the cost of sorting the backwards rank
        // traversal bit for each vertex, so each one knows exactly where the next 'smallest weight path' is
        // coming from in order. Don't think the time cost is too bad - it's only a single sort. Then we repeat
        // cycles of following the 'smallest weight path' until it results in completion

        // Right. Having got all the edges in negative order, what we're going to try is the following:
        // forward DFS, choosing the most negative-listed of the remaining edges each time; as each node finishes,
        // propagate its set weight forward backward along fixed edges. as we DFS forwards, we only visit each node once,
        // and if that's along a variable-weight edge, set that edge variable to weight 0. Fail if a contradiction is reached

        Map<ComponentVertex, Integer> vertexWeights = new HashMap<>();
        vertexWeights.put(root, 0);

        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        Result result = dfsIsh(vertexWeights, edgeVariableWeights, root, outgoingByDestinationNegativeWeight);

        allVertexWeights.put("w", vertexWeights);

        printIntermediatesGeneral(isDebug,
                fileName,
                0,
                "i_combined",
                allVertexWeights,
                edgeVariableWeights,
                new HashMap<>(),
                (Map) tertiaryWeights,
                new HashMap<>(),
                new HashMap<>()
        );


        saveCurrentWeights(vertexWeights, edgeVariableWeights);

        assignFailedWeightsToGraph();

        return result;




        // If we've hit here, we've set all vertices. Now we need to go through the remaining edge variables and ensure they
        // aren't broken.

        /*

        int min = Math.abs(vertexWeights.values().stream().min(Integer::compareTo).get());

        vertexWeights.forEach((v, weight) -> vertexWeights.put(v, weight + min));



        this.printIntermediates2(isDebug, vertexWeights, fileName, new HashMap<>(), 0, "d_setsourcestomin");

        */


        // return Result.SUCCESS;

    }


    private Result dfsIsh(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, ComponentVertex u, Map<ComponentVertex, PriorityQueue<Edge<ComponentVertex>>> orderings) {
        int vWeight = vertexWeights.get(u);
        PriorityQueue<Edge<ComponentVertex>> orderedEdges = orderings.get(u);


        while (orderedEdges != null && !orderedEdges.isEmpty()) {
            Edge<ComponentVertex> e = orderedEdges.poll();
            ComponentVertex to = e.to;

            if (vertexWeights.containsKey(to)) {
                // Check that either:
                // in the case of variable-weight edge, we can set the edge to some non -ve value; if so, do it. Also, we never will have seen it before, so no need to worry about weight changes.
                // in the case of positive-weight edge, check that everything lines up.
                int toWeight = vertexWeights.get(to);
                if (e.isVariableWeight()) {
                    if (toWeight < vWeight) return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;

                    Variable eVar = ((EdgeWeight.EdgeWeightVariable)e.weight).v;

                    edgeVariableWeights.put(eVar, toWeight - vWeight);
                }
                else {
                    int edgeWeight = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                    if (toWeight != vWeight + edgeWeight) return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                }
            }
            else {
                // Assign weight: weight(u) + 0 for variable-weight, weight(u) + w for fixed weight w
                // Then continue from the destination vertex

                if (e.isVariableWeight()) {
                    Variable eVar = ((EdgeWeight.EdgeWeightVariable)e.weight).v;
                    edgeVariableWeights.put(eVar, 0);
                    vertexWeights.put(to, vWeight);
                }
                else {
                    int edgeWeight = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                    vertexWeights.put(to, vWeight + edgeWeight);
                }

                Result temp = dfsIsh(vertexWeights, edgeVariableWeights, to, orderings);
                if (temp != Result.SUCCESS) return temp;
            }

        }

        propagateDefinite(vertexWeights, edgeVariableWeights, u);

        return Result.SUCCESS;
    }


    private Result propagateDefinite(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, ComponentVertex start) {
        Stack<ComponentVertex> toPropagate = new Stack<>();
        toPropagate.push(start);

        while (!toPropagate.isEmpty()) {
            ComponentVertex curr = toPropagate.pop();

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(curr)) {
                if (!e.isVariableWeight()) {
                    int i = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (vertexWeights.containsKey(e.from) && vertexWeights.get(e.from) != vertexWeights.get(curr) - i) {
                        saveCurrentWeights(vertexWeights, edgeVariableWeights);
                        return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                    } else if (!vertexWeights.containsKey(e.from)) {
                        vertexWeights.put(e.from, vertexWeights.get(curr) - i);
                        toPropagate.push(e.from);
                    }
                }
            }

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(curr)) {
                if (!e.isVariableWeight()) {
                    int i = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if ((vertexWeights.containsKey(e.to) && vertexWeights.get(e.to) != vertexWeights.get(curr) + i)) {
                        saveCurrentWeights(vertexWeights, edgeVariableWeights);
                        return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                    } else if (!vertexWeights.containsKey(e.to)) {
                        vertexWeights.put(e.to, vertexWeights.get(curr) + i);
                        toPropagate.push(e.to);
                    }
                }
            }
        }

        return  Result.SUCCESS;
    }


    private Map<ComponentVertex, Integer> rankVerticesFromSinks() {
        // We need to record how many edges from each vertex AREN'T being absorbed into the set; we start iterating in the sink
        Map<ComponentVertex, Integer> ranks = new HashMap<>();
        Map<ComponentVertex, Integer> edgesRemaining = new HashMap<>();
        Map<Integer, List<ComponentVertex>> ranksReverse = new HashMap<>();

        ranksReverse.put(0, new ArrayList<>());

        Set<ComponentVertex> currentAssigned = new HashSet<>();
        g.vertices.forEach(v -> {
            edgesRemaining.put(v, g.edgesOutgoing.get(v).size());
            if (edgesRemaining.get(v) == 0) {
                currentAssigned.add(v);
                ranks.put(v, 0);
                ranksReverse.get(0).add(v);
            }
        });

        int currRank = 0;
        int nextRank = currRank + 1;
        while (ranksReverse.containsKey(currRank)) {
            List<ComponentVertex> vs = ranksReverse.get(currRank);
            for (ComponentVertex v : vs) {
                for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                    edgesRemaining.put(e.from, edgesRemaining.get(e.from) - 1);
                    if (edgesRemaining.get(e.from) == 0) {
                        if (!ranksReverse.containsKey(nextRank)) ranksReverse.put(nextRank, new ArrayList<>());
                        ranksReverse.get(nextRank).add(e.from);
                    }
                }
            }
            currRank = nextRank;
            nextRank++;
        }


        ranksReverse.forEach((k, vs) -> vs.forEach(v -> ranks.put(v, k)));

        return ranks;

    }

    private Map<ComponentVertex, Integer> rankVerticesFromSources() {
        // We need to record how many edges from each vertex AREN'T being absorbed into the set; we start iterating in the sink
        Map<ComponentVertex, Integer> ranks = new HashMap<>();
        Map<ComponentVertex, Integer> edgesRemaining = new HashMap<>();
        Map<Integer, List<ComponentVertex>> ranksReverse = new HashMap<>();

        ranksReverse.put(0, new ArrayList<>());

        Set<ComponentVertex> currentAssigned = new HashSet<>();
        g.vertices.forEach(v -> {
            edgesRemaining.put(v, g.edgesIncoming.get(v).size());
            if (edgesRemaining.get(v) == 0) {
                currentAssigned.add(v);
                ranks.put(v, 0);
                ranksReverse.get(0).add(v);
            }
        });

        int currRank = 0;
        int nextRank = currRank + 1;
        while (ranksReverse.containsKey(currRank)) {
            List<ComponentVertex> vs = ranksReverse.get(currRank);
            for (ComponentVertex v : vs) {
                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    edgesRemaining.put(e.to, edgesRemaining.get(e.to) - 1);
                    if (edgesRemaining.get(e.to) == 0) {
                        if (!ranksReverse.containsKey(nextRank)) ranksReverse.put(nextRank, new ArrayList<>());
                        ranksReverse.get(nextRank).add(e.to);
                    }
                }
            }
            currRank = nextRank;
            nextRank++;
        }


        ranksReverse.forEach((k, vs) -> vs.forEach(v -> ranks.put(v, k)));

        return ranks;

    }
}
