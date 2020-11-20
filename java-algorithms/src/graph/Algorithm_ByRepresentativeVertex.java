package graph;

import timeconstraints.GraphSolvedTimeConstraintSet;

import java.util.*;
import java.util.stream.Collectors;

/**
 * This is a rewrite of Algorithm_ByDependentVertex according to the Algorithm 2 writeup, since it was an absolute
 * clusterfuck trying to work out what was happening.
 *
 * Comments "NEW NOTE:" are added in this revision for clarity
 */
public class Algorithm_ByRepresentativeVertex extends Algorithm {

    private static List<Map<Integer, Integer>> temp = new ArrayList<>();

    public static void printTemp() {
        StringJoiner sj = new StringJoiner("\n");
        temp.forEach(m -> sj.add(m.toString()));
        System.out.println(sj.toString());
    }

    public Algorithm_ByRepresentativeVertex(ComponentGraph g) {
        super(g);
    }



    @Override
    Result checkPathLengths(String fileName, boolean isDebug) {
        // Let's try splitting things up into sets of vertices by dependency

        // NEW NOTE:
        // each member of dependences is a set of vertices in the graph G connected by immutable edges
        // dependence_{x_i} = { v_{x_i + p1}, ..., v_{x_i + pn} } = S_{x_i}
        // so dependence is the vertex subset S_{x_i} represented by the representative vertex r_{x_i}
        // each such set is a "dependence set"
        // dependences = { dependence_{x_0}, ..., dependence_{x_n} }
        Set<Set<ComponentVertex>> dependences = new HashSet<>();

        Set<ComponentVertex> hasDependence = new HashSet<>(); // keeps track of which vertices have been assigned to a set in dependences

        Map<Integer, ComponentVertex> dependenceRoots = new HashMap<>(); // maps a dependence set ID to some minimum vertex in that dependence set

        // NEW NOTE: Group vertices connected by immutable edges into individual sets
        for (ComponentVertex v : g.vertices) {
            if (!hasDependence.contains(v)) {
                Set<ComponentVertex> dependenceSet = new HashSet<>();
                createDependenceSet(v, dependenceSet, hasDependence);
                dependences.add(dependenceSet);
            }
        }


        Map<String, Map<? extends Vertex, ? extends Object>> allVertexWeights = new HashMap<>(); // ??? unknown so far

        Map<ComponentVertex, Integer> vertexToDepSetID = new HashMap<>();

        Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights = new HashMap<>();

        Map<Set<ComponentVertex>, Set<ComponentVertex>> mostDepended = new HashMap<>();

        Map<Integer, Set<ComponentVertex>> dependenceSetIDToDependenceSet = new HashMap<>();

        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        // NEW NOTE:
        // Assign an ID to each "dependence set", which is set of graph H vertices dependent on one another via immutable edges
        // Also assign that "dependence set" ID to each vertex within the "dependence set"
        int depSetID = 0;
        for (Set<ComponentVertex> s : dependences) {

            for (ComponentVertex v : s) {
                vertexToDepSetID.put(v, depSetID);
            }

            // NEW NOTE:
            // computes a "dependence value" (whatever that is) for the vertices in the "dependence set" s?
            // Let's find out
            // changed things in computeDependenceValue():
            // - verticesWithMinimumDependenceWeights: for each vertex, the amount it is greater than the minimum value in its "dependence set"
            //       basically our value p - p_min
            // - mostDepended: a mapping of dependence set -> the vertices (vertex, really) with the minimum value in that dependence set
            // - dependenceRoots: mapping of dependence set ID -> any of the minimum vertices in that dependence set
            Result r = computeDependenceValue(s, verticesWithMinimumDependenceWeights, mostDepended, edgeVariableWeights, dependenceRoots, depSetID);

            if (r != Result.SUCCESS) {
                return r;
            }

            dependenceSetIDToDependenceSet.put(depSetID, s);

            depSetID++;
        }

        // NEW NOTE:
        // A bunch of intermediate output writing, nothing interesting really
        //allVertexWeights.put("Dep", vertexToDepSetID);
        allVertexWeights.put("V", verticesWithMinimumDependenceWeights);


        Map<ComponentVertex, Map<String, String>> styling = new HashMap<>();

        for (ComponentVertex v : g.vertices) {
            if (!styling.containsKey(v)) styling.put(v, new HashMap<>());
            styling.get(v).put("color", Colours.getColour(vertexToDepSetID.get(v)));
        }


        for (Set<ComponentVertex> mins : mostDepended.values()) {
            for (ComponentVertex v : mins) {
                if (!styling.containsKey(v)) styling.put(v, new HashMap<>());
                styling.get(v).put("fillcolor", Colours.getColour(vertexToDepSetID.get(v)));
                styling.get(v).put("style", "filled");
            }
        }


        printIntermediatesGeneral(isDebug,
                fileName,
                0,
                "d_dependentSets",
                allVertexWeights,
                new HashMap<>(),
                new HashMap<>(),
                new HashMap<>(),
                styling,
                new HashMap<>()
        );

        // NEW NOTE:
        // Get the root / source vertices (vertex) of the graph H, i.e. the vertex v_0; will only be the one!
        ComponentVertex root = g.vertices.stream().filter(v -> g.edgesIncoming.get(v).size() == 0).collect(Collectors.toList()).get(0);

        // NEW NOTE:
        // And draw the rest of the f'in owl, as it were
        Result slackForcedRes = createSlackGraph(isDebug,
                fileName,
                root, // root of H
                vertexToDepSetID, // map each vertex v_{x_i + p} -> its dependence set ID
                verticesWithMinimumDependenceWeights, // for each vertex, the amount it is greater than the minimum value in its "dependence set"
                dependenceSetIDToDependenceSet, // map of dependence set ID -> dependence set
                dependenceRoots); // mapping of dependence set ID -> any of the minimum vertices in that dependence set


        return slackForcedRes;

    }

    /**
     * taking variables from how it's called...
     * @param isDebug print debug
     * @param fileName debug file name (i think?)
     * @param zeroVertex the root of the graph H
     * @param gVertexToRepresentativeVertexID map each vertex v_{x_i + p} -> its dependence set ID. note: not just the minimum or a representative, all vertices!
     * @param verticesWithMinimumDependenceWeights  for each vertex, the amount it is greater than the minimum value in its "dependence set"
     * @param dependenceSetIDToDependenceSet map of dependence set ID -> dependence set
     * @param dependenceSetIDToDependenceSetRoot mapping of dependence set ID -> any of the minimum vertices in that dependence set
     * @return
     */
    private Result createSlackGraph(boolean isDebug,
                                    String fileName,
                                    ComponentVertex zeroVertex,
                                    Map<ComponentVertex, Integer> gVertexToRepresentativeVertexID,
                                    Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights,
                                    Map<Integer, Set<ComponentVertex>> dependenceSetIDToDependenceSet,
                                    Map<Integer, ComponentVertex> dependenceSetIDToDependenceSetRoot) {





        // NEW NOTE:
        // this is where we compute our initial w(u->v); or rather, our (0 - w(u->v))
        // effectively a map of r_{x_i} -> (r_{x_j} -> ((q_d - q_min) - (p_d - p_min))) for controlling edge r_{x_i} -> r_{x_j}
        // except done with the dependence set ID for r_{x_i} rather than r_{x_i}, and we're not looking up edges but edges implied by a pair of dependence set ids
        Map<Integer, Map<Integer, Integer>> representativeEdgeWeights = new HashMap<>();
        Map<Integer, Map<Integer, Integer>> representativeEdgeWeightsRelative = new HashMap<>();

        Map<Vertex, Integer> representativeVertexToID = new HashMap<>();
        Graph<Vertex> representativeGraph = new Graph<>(GraphComponentFactory.STANDARD);
        Map<String, Map<Variable, Integer>> variableWeights = new HashMap<>();

        createRepresentativeGraphH(gVertexToRepresentativeVertexID, verticesWithMinimumDependenceWeights, representativeEdgeWeights, representativeVertexToID, representativeGraph, variableWeights);

        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_R_VERTICES, representativeGraph.getVertexCount());
        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_R_EDGES, representativeGraph.getEdgeCount());


        // We're going to copy w' <- w here:
        for (Map.Entry<Integer, Map<Integer, Integer>> entry : representativeEdgeWeights.entrySet()) {
            representativeEdgeWeightsRelative.put(entry.getKey(), new HashMap<>());
            for (Map.Entry<Integer, Integer> entry2 : entry.getValue().entrySet()) {
                representativeEdgeWeightsRelative.get(entry.getKey()).put(entry2.getKey(), entry2.getValue());
            }
        }

        // OLD COMMENT:
        // do the SCC thing here, check for cycles.
        // If we've succeeded, then our forced settings won't cause a conflict.
        // Also, the original forced graph is acyclic (since no cycles).            -- NEW NOTE: think this was referring to graph H having been 'forced'
        // Now create the SCC of this new graph.

        ComponentGraph representativeComponentGraph = new SCC().runSCC(representativeGraph);


        Map<String, Map<Vertex, Integer>> vertexWeights = new HashMap<>();

        Map<Vertex, Map<String, String>> styling = new HashMap<>();

        for (Vertex v : representativeGraph.vertices) {
            styling.put(v, new HashMap<>());
            styling.get(v).put("color", Colours.getColour(representativeVertexToID.get(v)));
            styling.get(v).put("fillcolor",Colours.getColour(representativeVertexToID.get(v)));
        }


        Map<ComponentVertex, Integer> representativeComponentVertexToRank = new HashMap<>();
        Map<Integer, List<ComponentVertex>> rankToRepresentativeComponentVertex = new HashMap<>();

        Map<Vertex, Integer> ranksByDepVertex = calculateRanks(representativeComponentGraph, representativeComponentVertexToRank, rankToRepresentativeComponentVertex);



        Map<Integer, List<Integer>> rankToRepresentativeComponentSizes = new HashMap<>();
        for (Map.Entry<Integer, List<ComponentVertex>> entry : rankToRepresentativeComponentVertex.entrySet()) {
            rankToRepresentativeComponentSizes.put(entry.getKey(), entry.getValue().stream().map(componentVertex -> componentVertex.componentContents.size()).collect(Collectors.toList()));
        }
        algorithmInternalData.put(GraphSolvedTimeConstraintSet.KEY_R_COMPONENT_SIZES_BY_RANK, rankToRepresentativeComponentSizes);

        vertexWeights.put("rank", ranksByDepVertex);

        if (isDebug) {
            representativeGraph.writeGraphVizStyledWithWeights(
                    fileName + "_zz_ranks.dotgraph",
                    (Map) vertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>(),
                    styling,
                    new HashMap<>());
        }





        // For the rewrite, we're going to store weight r_{x_i} with a map
        // depSetID(r_{x_i}) -> w(r_{x_i})
        // Then we'll assign to v_{x_i + p_min} and propagate after this is done - we're following the writeup

        // Start with zero-rank zero-component
        int zeroDepComponent = gVertexToRepresentativeVertexID.get(zeroVertex);

        HashMap<Integer, Integer> representativeVertexWeightsByID = new HashMap<>();
        representativeVertexWeightsByID.put(zeroDepComponent, 0);


        // Now we need to set all the other Dependency SCC components in rank order.

        int nextRank = 1;

        //System.out.println("DEPSCCVERTICESBYRANK: " + reprComponentsByRank);

        while (rankToRepresentativeComponentVertex.containsKey(nextRank)) {
            // We go through the dependency SCC components in rank order.

            for (ComponentVertex reprComponent : rankToRepresentativeComponentVertex.get(nextRank)) {

                // We need to know how many interdependent dependency sets there are
                // in the dependency SCC component.
                if (reprComponent.componentContents.size() == 1) {

                    Vertex rv = null;
                    for (Vertex v : reprComponent.componentContents) {
                        rv = v;
                        break;      // only way to get the single value out of the set...
                    }

                    setSingleRepresentativeVertex(representativeEdgeWeights, representativeVertexToID, representativeGraph, representativeVertexWeightsByID, rv, reprComponent.componentContents);

                }
                else {
                    if (!setRepresentativeVertexCycle(representativeEdgeWeights, representativeEdgeWeightsRelative, representativeVertexToID, representativeGraph, representativeVertexWeightsByID, reprComponent))
                        return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                }

            }
            nextRank++;
        }




        Map<ComponentVertex, Integer> finalVertexWeights = new HashMap<>();
        finalVertexWeights.put(zeroVertex, 0);

        Map<String, Map<ComponentVertex, Integer>> allFinalVertexWeights = new HashMap<>();
        allFinalVertexWeights.put("w", finalVertexWeights);



        // Finally, we need to set the original variable-weight edges to their appropriate values based on the new vertex assignments.

        Map<Variable, Integer> finalEdgeWeights = new HashMap<>();

//      System.out.println(finalVertexWeights);

        for (Map.Entry<Integer, Integer> entry : representativeVertexWeightsByID.entrySet()) {
            int representativeVertexID = entry.getKey();
            int weight = entry.getValue();

            propagateFromDependenceSetRoot(dependenceSetIDToDependenceSetRoot, finalVertexWeights, representativeVertexID, weight);
        }


        if (isDebug) {
            g.writeGraphVizStyledWithWeights(
                    fileName + "_zzz_3_final_assignment.dotgraph",
                    (Map) allFinalVertexWeights,
                    finalEdgeWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>());
        }


        assignWeightsToGraph(finalVertexWeights, finalEdgeWeights);

        return Result.SUCCESS;
    }

    private boolean setRepresentativeVertexCycle(Map<Integer, Map<Integer, Integer>> representativeEdgeWeights, Map<Integer, Map<Integer, Integer>> representativeEdgeWeightsRelative, Map<Vertex, Integer> representativeVertexToID, Graph<Vertex> representativeGraph, HashMap<Integer, Integer> representativeVertexWeightsByID, ComponentVertex reprComponent) {
        //System.out.println("Dependency SCC of size " + reprComponent.componentContents.size() + " : " + showDependencySCC(reprComponent, representativeVertexToID, dependenceSetIDToDependenceSet));

        Set<Vertex> cycleVertices = reprComponent.componentContents;

        //System.out.println("Representative edge weights initially: " + representativeEdgeWeights);
        // for each rv in cycleVertices ; setSingleRepresentativeVertex(rv, cycleVertices)
        for (Vertex rv : cycleVertices) {
            //System.out.println("Initial setting single representative vertex " + rv);
            setSingleRepresentativeVertex(representativeEdgeWeights, representativeVertexToID, representativeGraph, representativeVertexWeightsByID, rv, cycleVertices);
        }
        //System.out.println(representativeVertexWeightsByID);


        // for each rv in cycleVertices, ru->rv in E_r with ru in cycleVertices
        // difAtEq <- w(ru->rv)
        // curDif <- w(ru) - w(rv)
        // w(ru->rv) <- curDif + difAtEq
        for (Vertex rv : cycleVertices) {
            for (Edge<Vertex> ru_rv : representativeGraph.edgesIncoming.get(rv)) {
                Vertex ru = ru_rv.from;
                if (cycleVertices.contains(ru)) {
                    int uID = representativeVertexToID.get(ru);
                    int vID = representativeVertexToID.get(rv);
                    int difAtEq = representativeEdgeWeights.get(uID).get(vID);
                    int curDif = representativeVertexWeightsByID.get(uID) - representativeVertexWeightsByID.get(vID);
                    representativeEdgeWeights.get(uID).put(vID, curDif + difAtEq);
                }
            }
        }


        Set<Edge<Vertex>> allCycleEdges = getEdgesInsideRepresentativeComponent(representativeGraph, cycleVertices);

        //System.out.println("All cycle edges " + allCycleEdges);

        // Iterate for |cycleVertices| + 1
        int iterationCount = 0;
        while (iterationCount <= cycleVertices.size() + 1) {
            boolean changed = false;
            for (Edge<Vertex> rt_ru : allCycleEdges) {
                changed = relaxEdge(representativeEdgeWeights, representativeVertexToID, representativeGraph, representativeVertexWeightsByID, cycleVertices, changed, rt_ru);
            }
            if (!changed) {
                break;
            }
            iterationCount++;
        }

        //System.out.println("DONE: representative vertex weights " + representativeVertexWeightsByID);

        //System.out.println("ITERATIONS " + iterationCount);

        if (iterationCount > cycleVertices.size() + 1) {
            return false;
        }

        // Fix up INCOMING edges of the cycle subgraph so they're now representing actual slack
        // when vertices are set
        // NOTE: OUTGOING (to other representative components) MUST BE LEFT AS RELATIVE SLACK!!

        for (Vertex rv : cycleVertices) {
            int vID = representativeVertexToID.get(rv);
            int rvWeight = representativeVertexWeightsByID.get(vID);

            for (Edge<Vertex> ru_rv : representativeGraph.edgesIncoming.get(rv)) {
                Vertex ru = ru_rv.from;
                if (!cycleVertices.contains(ru)) {
                    int uID = representativeVertexToID.get(ru);
                    int rurvWeightOld = representativeEdgeWeightsRelative.get(uID).get(vID);
                    int ruWeight = representativeVertexWeightsByID.get(uID);
                    representativeEdgeWeights.get(uID).put(vID, ruWeight + rurvWeightOld - rvWeight);
                }
            }
        }
        return true;
    }

    private boolean relaxEdge(Map<Integer, Map<Integer, Integer>> representativeEdgeWeights, Map<Vertex, Integer> representativeVertexToID, Graph<Vertex> representativeGraph, HashMap<Integer, Integer> representativeVertexWeightsByID, Set<Vertex> cycleVertices, boolean changed, Edge<Vertex> rt_ru) {
        Vertex ru = rt_ru.to;
        int tID = representativeVertexToID.get(rt_ru.from);
        int uID = representativeVertexToID.get(rt_ru.to);
        int rtruWeight = representativeEdgeWeights.get(tID).get(uID);

        if (rtruWeight > 0) {
            //System.out.println("HERE");
            changed = true;
            int d = rtruWeight;
            representativeVertexWeightsByID.put(uID, representativeVertexWeightsByID.get(uID) + d);
            vertexIncrementCount();

            //System.out.println("CHANGED WEIGHT of " + ru + " TO " + representativeVertexWeightsByID.get(uID));
            for (Edge<Vertex> ru_rv : representativeGraph.edgesOutgoing.get(ru)) {
                if (cycleVertices.contains(ru_rv.to)) {
                    int vID = representativeVertexToID.get(ru_rv.to);
                    int rurvWeight = representativeEdgeWeights.get(uID).get(vID);
                    representativeEdgeWeights.get(uID).put(vID, rurvWeight + d);
                }
            }

            for (Edge<Vertex> rs_ru : representativeGraph.edgesIncoming.get(ru)) {
                if (cycleVertices.contains(rs_ru.from)) {
                    int sID = representativeVertexToID.get(rs_ru.from);
                    int rsruWeight = representativeEdgeWeights.get(sID).get(uID);
                    representativeEdgeWeights.get(sID).put(uID, rsruWeight - d);
                }
            }
        }
        return changed;
    }

    private Set<Edge<Vertex>> getEdgesInsideRepresentativeComponent(Graph<Vertex> representativeGraph, Set<Vertex> cycleVertices) {
        Set<Edge<Vertex>> allCycleEdges = new HashSet<>();
        for (Vertex ru : cycleVertices) {
            for (Edge<Vertex> ru_rv : representativeGraph.edgesOutgoing.get(ru)) {
                if (cycleVertices.contains(ru_rv.to)) {
                    allCycleEdges.add(ru_rv);
                }
            }
        }
        return allCycleEdges;
    }

    private void setSingleRepresentativeVertex(Map<Integer, Map<Integer, Integer>> representativeEdgeWeights, Map<Vertex, Integer> representativeVertexToID, Graph<Vertex> representativeGraph, HashMap<Integer, Integer> representativeVertexWeights, Vertex rv, Set<Vertex> ignoreSources) {
        int vID = representativeVertexToID.get(rv);

        // m <- max{ w(ru) + w(ru->rv) | ru->rv in E_R, ru not in ignoreSources }
        int m = 0; // max, default value
        //System.out.println("Setting incoming to " + vID);
        for (Edge<Vertex> ru_rv : representativeGraph.edgesIncoming.get(rv)) {
            if (!ignoreSources.contains(ru_rv.from)) {
                int uID = representativeVertexToID.get(ru_rv.from);
                //System.out.println("- Edge from " + uID);

                int ruWeight = representativeVertexWeights.get(uID);
                int rurvWeight = representativeEdgeWeights.get(uID).get(vID);

                if (ruWeight + rurvWeight > m) {
                    m = ruWeight + rurvWeight;
                }
            }
        }


        representativeVertexWeights.put(vID, m);
        vertexIncrementCount();

        // foreach ru->rv in E_R, ru not in ignoreSources
        for (Edge<Vertex> ru_rv : representativeGraph.edgesIncoming.get(rv)) {
            if (!ignoreSources.contains(ru_rv.from)) {
                int uID = representativeVertexToID.get(ru_rv.from);
                int ruWeight = representativeVertexWeights.get(uID);
                int rurvWeight = representativeEdgeWeights.get(uID).get(vID);

                representativeEdgeWeights.get(uID).put(vID, ruWeight + rurvWeight - m);
            }
        }
    }

    private Map<Vertex, Integer> calculateRanks(ComponentGraph representativeComponentGraph, Map<ComponentVertex, Integer> ranksByDepSCCVertex, Map<Integer, List<ComponentVertex>> reprComponentsByRank) {
        rankVerticesFromSources(representativeComponentGraph, ranksByDepSCCVertex, reprComponentsByRank);

        Map<Vertex, Integer> ranksByDepVertex = new HashMap<>();
        ranksByDepSCCVertex.forEach((sccV, rank) -> {
           sccV.componentContents.forEach(v -> {
               ranksByDepVertex.put(v, rank);
           });
        });
        return ranksByDepVertex;
    }

    private void createRepresentativeGraphH(Map<ComponentVertex, Integer> vertexDependences, Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights, Map<Integer, Map<Integer, Integer>> forceds, Map<Vertex, Integer> depSetReductionToDepSetID, Graph<Vertex> representativeGraph, Map<String, Map<Variable, Integer>> variableWeights) {
        for (Map.Entry<ComponentVertex, Integer> entry : vertexDependences.entrySet()) {
            ComponentVertex v = entry.getKey();
            int vDepSetID = entry.getValue();

            // Edges incoming u->v - these are the 'forced'
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                ComponentVertex u = e.from;
                int uDepSetID = vertexDependences.get(u);

                // NEW NOTE: update the weight of the implied controlling edge from r_u -> r_v
                if (vDepSetID != uDepSetID) {
                    // NEW NOTE: (p - p_min) - (q - q_min) for edge e
                    int forced = verticesWithMinimumDependenceWeights.get(u) - verticesWithMinimumDependenceWeights.get(v);


                    if (!forceds.containsKey(uDepSetID)) forceds.put(uDepSetID, new HashMap<>());
                    if (!forceds.get(uDepSetID).containsKey(vDepSetID)) {
                        forceds.get(uDepSetID).put(vDepSetID, forced);
                    } else {
                        int currForced = forceds.get(uDepSetID).get(vDepSetID);
                        // NEW NOTE: ensure minimum value is taken for the controlling edge (since we're using the opposite signs)
                        if (forced > currForced) {
                            forceds.get(uDepSetID).put(vDepSetID, forced);
                        }
                    }
                }
            }
        }

        // NEW NOTE:
        // since we haven't created a graph R yet for vertices r_{x_i}, we do so here. Vertices are identified by their
        // dependence set id, one-one mapping with id(S_{x_i}) <-> r_{x_i} in the writeup.


        // OLD COMMENT: Now we build a graph with these weights, see what happens. Each vertex represents a dependence set.
        // we don't care about edge weight sum here


        variableWeights.put("fcd", new HashMap<>());


        // OLD COMMENT:
        // Forced weights to graph
        // To try this, first we'll add only the negative forced
        // Then we can see if any cycles exist in the resulting SCC of it
        // If so, we've got an impossible graph - forced A->B -ve means we need to increase B.
        // But then if we have forced A->B->...->A, we need to increase B, so then we'll need to (eventually) increase
        // A, so we'll need to increase A... it's unresolvable.
        // Actually, no, add ALL forced. We're looking for non-zero cycles in the result.
        // Consider if A->B->A has a positive non-zero cycle - say, forced(A->B)=1, forced(B->A)=0

        for (Map.Entry<Integer, Map<Integer, Integer>> entry1 : forceds.entrySet()) {
            for (Map.Entry<Integer, Integer> entry2 : entry1.getValue().entrySet()) {
                int depSetV = entry1.getKey();
                int depSetW = entry2.getKey();
                int forced = entry2.getValue();

                Vertex v = representativeGraph.addOrGetCurrentVertex(Integer.toString(depSetV));
                Vertex w = representativeGraph.addOrGetCurrentVertex(Integer.toString(depSetW));

                depSetReductionToDepSetID.put(v, entry1.getKey());
                depSetReductionToDepSetID.put(w, entry2.getKey());

                representativeGraph.addEdgeFixedWeight(v, w, forced);
            }
        }
    }

    private String showDependencySCC(ComponentVertex depSccCV, Map<Vertex, Integer> depSetIDs, Map<Integer, Set<ComponentVertex>> depSetIDToDepSetContents) {
        StringBuilder sb = new StringBuilder();
        sb.append("DepSCC [");
        for (Vertex v : depSccCV.componentContents) {
            int depSetID = depSetIDs.get(v);
            sb.append(Integer.valueOf(depSetID)).append(":");
            sb.append(depSetIDToDepSetContents.get(depSetID).toString());
            sb.append(", ");
        }
        sb.append("]");

        return sb.toString();
    }

    private String showDependencyContents(Vertex depVertex, Map<Vertex, Integer> depSetIDs, Map<Integer, Set<ComponentVertex>> depSetIDToDepSetContents) {
        StringBuilder sb = new StringBuilder();
        sb.append("Dep [");
        int depSetID = depSetIDs.get(depVertex);
        sb.append(Integer.valueOf(depSetID)).append(":");
        sb.append(depSetIDToDepSetContents.get(depSetID).toString());
        sb.append(", ");
        sb.append("]");
        return sb.toString();
    }

    private void propagateFromDependenceSetRoot(Map<Integer, ComponentVertex> dependenceSetIDToDependenceSetRoot, Map<ComponentVertex, Integer> finalVertexWeights, int thisDepSetID, int bestWeight) {
        Stack<ComponentVertex> toPropagateFrom = new Stack<>();

        ComponentVertex thisPropagationRoot = dependenceSetIDToDependenceSetRoot.get(thisDepSetID);

        //System.out.println("Propagating from " + thisPropagationRoot + " with weight " + bestWeight);

        toPropagateFrom.push(thisPropagationRoot);
        finalVertexWeights.put(thisPropagationRoot, bestWeight);

        while (!toPropagateFrom.isEmpty()) {
            ComponentVertex v = toPropagateFrom.pop();
            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                if (!e.isVariableWeight() && !finalVertexWeights.containsKey(e.to)) {
                    int w = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                    finalVertexWeights.put(e.to, finalVertexWeights.get(v) + w);
                    toPropagateFrom.push(e.to);
                }
            }

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (!e.isVariableWeight() && !finalVertexWeights.containsKey(e.from)) {
                    int w = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                    finalVertexWeights.put(e.from, finalVertexWeights.get(v) - w);
                    toPropagateFrom.push(e.from);
                }
            }
        }
    }


    // Taken from the other, failed algorithm attempt
    private void rankVerticesFromSources(Graph<ComponentVertex> toRank, Map<ComponentVertex, Integer> ranksByVertex, Map<Integer, List<ComponentVertex>> verticesByRank) {
        // We need to record how many edges from each vertex AREN'T being absorbed into the set; we start iterating in the source
        Map<ComponentVertex, Integer> edgesRemaining = new HashMap<>();

        verticesByRank.put(0, new ArrayList<>());

        Set<ComponentVertex> currentAssigned = new HashSet<>();
        toRank.vertices.forEach(v -> {
            edgesRemaining.put(v, toRank.edgesIncoming.get(v).size());
            if (edgesRemaining.get(v) == 0) {
                currentAssigned.add(v);
                ranksByVertex.put(v, 0);
                verticesByRank.get(0).add(v);
            }
        });


        int currRank = 0;
        int nextRank = 1;
        while (verticesByRank.containsKey(currRank)) {
            List<ComponentVertex> vs = verticesByRank.get(currRank);
            for (ComponentVertex v : vs) {
                for (Edge<ComponentVertex> e : toRank.edgesOutgoing.get(v)) {
                    edgesRemaining.put(e.to, edgesRemaining.get(e.to) - 1);
                    if (edgesRemaining.get(e.to) == 0) {
                        if (!verticesByRank.containsKey(nextRank)) verticesByRank.put(nextRank, new ArrayList<>());
                        verticesByRank.get(nextRank).add(e.to);
                    }
                }
            }
            currRank = nextRank;
            nextRank++;
        }


        verticesByRank.forEach((k, vs) -> vs.forEach(v -> ranksByVertex.put(v, k)));

    }

    /**
     * Given some set of vertices mutually-interdependent via immutable edges, finds the guaranteed minimum-weight vertex
     * (vertices, really, but that doesn't happen with this construction), and finds the differences in final weight
     * between that minimum-weight vertex and all the others.
     * @param dependenceSet the set of mutually-interdependent vertices
     * @param dependenceValues for each vertex in dependenceSet, the amount by which it is greater than the minimum-weight vertex
     *                         (so weight(minV) + dependenceValues(v) = weight(v))
     * @param mostDepended the mapping of the dependence set to its set of minimal vertices
     * @param edgeVariableWeights input-only, for reference of immutable weights
     * @param dependenceRoots a mapping from dependence set ID number to any arbitrary member of the set of minimal vertices (here, just v_{x_i + p_min})
     * @param dependenceSetID input-only; this dependence set's ID number
     * @return did it complete successfully? not really needed in retrospect, the failure conditions should have been guaranteed by graph H construction
     */
    private Result computeDependenceValue(Set<ComponentVertex> dependenceSet, Map<ComponentVertex, Integer> dependenceValues, Map<Set<ComponentVertex>, Set<ComponentVertex>> mostDepended, Map<Variable, Integer> edgeVariableWeights, Map<Integer, ComponentVertex> dependenceRoots, int dependenceSetID) {
        ComponentVertex arb = null;

        // Get an arbitrary starting vertex out
        for (ComponentVertex v : dependenceSet) {
            arb = v;
            break;
        }

        // Search / traverse starts at arbitrary point, to find the least vertices in the dependence and set up p-p_min

        if (arb == null) throw new RuntimeException("Managed to create a dependence set with no vertices????");

        Stack<ComponentVertex> toDo = new Stack<>();
        Set<ComponentVertex> inProgress = new HashSet<>();
        inProgress.add(arb);
        toDo.push(arb);


        dependenceValues.put(arb, 0);
        Set<ComponentVertex> min = new HashSet<>();
        min.add(arb);
        int minValue = 0;


        while (!toDo.isEmpty()) {
            ComponentVertex v = toDo.pop();

            if (dependenceValues.get(v) < minValue) {
                minValue = dependenceValues.get(v);
                min.clear();
                min.add(v);
            } else if (dependenceValues.get(v) == minValue) {
                min.add(v);
            }

            int vWeight = dependenceValues.get(v);

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                if (!e.isVariableWeight() && dependenceSet.contains(e.to)) {
                    int w = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (!inProgress.contains(e.to)) {
                        inProgress.add(e.to);
                        toDo.push(e.to);
                        dependenceValues.put(e.to, vWeight + w);
                    } else {
                        if (dependenceValues.get(e.to) != vWeight + w) {
                            return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                        }
                    }
                }
            }

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (!e.isVariableWeight() && dependenceSet.contains(e.from)) {
                    int w = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (!inProgress.contains(e.from)) {
                        inProgress.add(e.from);
                        toDo.push(e.from);
                        dependenceValues.put(e.from, vWeight - w);
                    } else {
                        if (dependenceValues.get(e.from) != vWeight - w) {
                            return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                        }
                    }
                    ;
                }
            }


        }



        mostDepended.put(dependenceSet, min);



        // Pick an arbitrary minimum for root, doesn't matter; first one is fine
        assert !min.isEmpty();
        for (ComponentVertex v : min) {
            dependenceRoots.put(dependenceSetID, v);
            break;
        }


        int toAdd = 0 - minValue;

        for (ComponentVertex v : dependenceSet) {
            dependenceValues.put(v, dependenceValues.get(v) + toAdd); // p - p_min
        }

        // We'd better assign edge variable weights within a dependent set too, to make sure they work ok
        // They're all relative anyway. We can just take variable edges incoming within the dependence set.
        for (ComponentVertex v : dependenceSet) {
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (e.isVariableWeight() && dependenceSet.contains(e.from) && dependenceSet.contains(e.to)) {
                    int diff = dependenceValues.get(e.to) - dependenceValues.get(e.from);
                    if (diff < 0) return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;

                    Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;
                    edgeVariableWeights.put(eVar, diff);
                }
            }
        }


        return Result.SUCCESS;
    }


    /**
     * Returns in dependenceSet the set of all vertices connected (transitively) by immutable edges to vertex u,
     * and adds all such vertices to hasDependence.
     * @param u
     * @param dependenceSet
     * @param hasDependence
     */
    private void createDependenceSet(ComponentVertex u, Set<ComponentVertex> dependenceSet, Set<ComponentVertex> hasDependence) {
        Stack<ComponentVertex> toDo = new Stack<>();

        Set<ComponentVertex> inProgress = new HashSet<>();
        toDo.push(u);
        dependenceSet.add(u);
        inProgress.add(u);

        while (!toDo.isEmpty()) {
            ComponentVertex v = toDo.pop();

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                if (!e.isVariableWeight() && !hasDependence.contains(e.to) && !inProgress.contains(e.to)) {
                    dependenceSet.add(e.to);
                    toDo.push(e.to);
                    hasDependence.add(e.to);
                    inProgress.add(e.to);
                }
            }

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (!e.isVariableWeight() && !hasDependence.contains(e.from) && !inProgress.contains(e.from)) {
                    dependenceSet.add(e.from);
                    hasDependence.add(e.from);
                    toDo.push(e.from);
                    inProgress.add(e.from);
                }
            }
        }
    }


}
