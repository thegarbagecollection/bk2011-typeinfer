package graph;

import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

public class Algorithm_ByDependentVertex extends Algorithm {

    private static List<Map<Integer, Integer>> temp = new ArrayList<>();

    public static void printTemp() {
        StringJoiner sj = new StringJoiner("\n");
        temp.forEach(m -> sj.add(m.toString()));
        System.out.println(sj.toString());
    }


    public Algorithm_ByDependentVertex(ComponentGraph g) {
        super(g);
    }

    String[] colours = {"red", "green", "lightblue", "magenta", "cyan", "yellow", "orange", "slategrey"};

    @Override
    Result checkPathLengths(String fileName, boolean isDebug) {
        // Let's try splitting things up into sets of vertices by dependency

        Set<Set<ComponentVertex>> dependences = new HashSet<>();

        Set<ComponentVertex> hasDependence = new HashSet<>();

        Map<Integer, ComponentVertex> dependenceRoots = new HashMap<>();

        for (ComponentVertex v : g.vertices) {
            if (!hasDependence.contains(v)) {
                Set<ComponentVertex> dependenceSet = new HashSet<>();
                createDependenceSet(v, dependenceSet, hasDependence);
                dependences.add(dependenceSet);
            }
        }

        Map<String, Map<? extends Vertex, ? extends Object>> allVertexWeights = new HashMap<>();

        Map<ComponentVertex, Integer> vertexDependences = new HashMap<>();

        Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights = new HashMap<>();

        Map<Set<ComponentVertex>, Set<ComponentVertex>> mostDepended = new HashMap<>();

        Map<Integer, Set<ComponentVertex>> dependenceSetIDToDependenceSet = new HashMap<>();

        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        int depSetID = 0;
        for (Set<ComponentVertex> s : dependences) {
            for (ComponentVertex v : s) {
                vertexDependences.put(v, depSetID);
            }

            Result r = computeDependenceValue(s, verticesWithMinimumDependenceWeights, mostDepended, edgeVariableWeights, dependenceRoots, depSetID);

            if (r != Result.SUCCESS) {
                return r;
            }

            dependenceSetIDToDependenceSet.put(depSetID, s);

            depSetID++;
        }


        //allVertexWeights.put("Dep", vertexDependences);
        allVertexWeights.put("V", verticesWithMinimumDependenceWeights);


        Map<ComponentVertex, Map<String, String>> styling = new HashMap<>();

        for (ComponentVertex v : g.vertices) {
            if (!styling.containsKey(v)) styling.put(v, new HashMap<>());
            styling.get(v).put("color", colours[vertexDependences.get(v)]);
        }


        for (Set<ComponentVertex> mins : mostDepended.values()) {
            for (ComponentVertex v : mins) {
                if (!styling.containsKey(v)) styling.put(v, new HashMap<>());
                styling.get(v).put("fillcolor", colours[vertexDependences.get(v)]);
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

        ComponentVertex root = g.vertices.stream().filter(v -> g.edgesIncoming.get(v).size() == 0).collect(Collectors.toList()).get(0);

        Result slackForcedRes = createSlackGraph(isDebug,
                fileName,
                root,
                vertexDependences,
                verticesWithMinimumDependenceWeights,
                dependenceSetIDToDependenceSet,
                dependenceRoots);


        return slackForcedRes;

        // Now we need to know which sets of components each edge goes between
        // I *think* that within each set the minimum(s) MUST have at least one edge incoming from another
        // set, by construction of the graph - there's only one sink vertex.

        // Start by initialising the 'root' set from the root. We, ah, actually need to find the root first.
        // Actually, by definition, since the root's been set to 0, it's already been initialised. Sorted.


        //Map<Integer, Set<ComponentVertex>> initialisedDependenceSetsByIndex = new HashMap<>();


/*        Set<Set<ComponentVertex>> initialisedDependenceSets = new HashSet<>();
        Set<ComponentVertex> initialisedVertices = new HashSet<>();

        Map<ComponentVertex, Integer> vertexWeights = new HashMap<>();


        //initialisedDependenceSetsByIndex.put(vertexDependences.get(root), dependenceSetIDToDependenceSet.get(vertexDependences.get(root)));
        initialisedDependenceSets.add(dependenceSetIDToDependenceSet.get(vertexDependences.get(root)));

        //System.out.println("ROOT DEPENDENCE SET: " + dependenceSetIDToDependenceSet.get(vertexDependences.get(root)));
        //System.out.println("\n");

        for (ComponentVertex v : dependenceSetIDToDependenceSet.get(vertexDependences.get(root))) {
            initialisedVertices.add(v);
            vertexWeights.put(v, verticesWithMinimumDependenceWeights.get(v));
        }


        // We can make this more efficient later - we check to see if any dependence sets without weights assigned are COMPLETELY able to be determined by
        // an assigned set, and set them appropriately.
        Set<Set<ComponentVertex>> dependenceSetsToInitialise = new HashSet<>(dependences);
        dependenceSetsToInitialise.remove(dependenceSetIDToDependenceSet.get(vertexDependences.get(root)));

        boolean done = false;

        while (!done) {
            List<Set<ComponentVertex>> nextDependenceSetToDo = new ArrayList<>();

            for (Set<ComponentVertex> vs : dependenceSetsToInitialise) {
                // System.out.println("IS DEPENDENCE SET " + vs + " READY TO BE PROCESSED?");
                // System.out.println("INITIALIZED ALREADY: " + initialisedDependenceSets);

                if (!initialisedDependenceSets.contains(vs)) {
                    // System.out.println("CHECKING DEPENDENCE SET " + vs);
                    boolean allFromInitialised = true;

                    for (ComponentVertex cv : vs) {
                        // System.out.println("CHECKING VERTEX " + cv);
                        for (Edge<ComponentVertex> e : g.edgesIncoming.get(cv)) {
                            // System.out.println("CHECKING EDGE " + e);
                            if (!initialisedVertices.contains(e.from) && !vs.contains(e.from))
                                allFromInitialised = false;
                        }
                    }

                    if (allFromInitialised) nextDependenceSetToDo.add(vs);
                }
                //System.out.println("\n");
            }

            if (nextDependenceSetToDo.isEmpty()) done = true;

            // System.out.println("NEXT DEPENDENCE SET: " + nextDependenceSetToDo);

            dependenceSetsToInitialise.removeAll(nextDependenceSetToDo);

            for (Set<ComponentVertex> dependenceSet : nextDependenceSetToDo) {

                Result r = initialiseDependenceSetFromAllKnown(
                        dependenceSet,
                        mostDepended.get(dependenceSet),
                        vertexWeights,
                        edgeVariableWeights,
                        verticesWithMinimumDependenceWeights,
                        vertexDependences);

                initialisedDependenceSets.add(dependenceSet);

                initialisedVertices.addAll(dependenceSet);

                if (r != Result.SUCCESS) {
                    return r;
                }


            }
        }*/

/*
        for (ComponentVertex v : g.vertices) {
            if (vertexWeights.containsKey(v)) {
                styling.get(v).put("fillcolor", colours[vertexDependences.get(v)]);
                styling.get(v).put("style", "filled");
            } else {
                styling.get(v).put("fillcolor", "none");
                styling.get(v).put("style", "filled");
            }
        }


        allVertexWeights.put("W", vertexWeights);


        printIntermediatesGeneral(isDebug,
                fileName,
                0,
                "e_fromknown",
                allVertexWeights,
                edgeVariableWeights,
                new HashMap<>(),
                new HashMap<>(),
                styling,
                new HashMap<>()
        );
*/


    }

    private Result createSlackGraph(boolean isDebug,
                                    String fileName,
                                    ComponentVertex zeroVertex,
                                    Map<ComponentVertex, Integer> vertexDependences,
                                    Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights,
                                    Map<Integer, Set<ComponentVertex>> dependenceSetIDToDependenceSet,
                                    Map<Integer, ComponentVertex> dependenceSetIDToDependenceSetRoot) {
        // 'Slack graph'. For each dependence set, we want to analyse the links between them, starting with their 'minimum dependence weights',
        // to look at their relative differences.
        // For dependence sets A and B:
        // the 'slack' from A to B is, if any, the amount we can increase the weights in A before the weights in B start increasing; it's given by the smallest
        // weight difference between a vertex in B and a vertex in A. <= 0 slack means increasing A immediately increases B.
        // the 'forced' from B to A is the minimum negative weight between a vertex in A and a vertex in B; this indicates that
        // A must be AT LEAST abs(forced(A,B)) larger than B in the solution.


        // A -> B -> slack
        Map<Integer, Map<Integer, Integer>> slacks = new HashMap<>();

        // A -> B -> forced
        Map<Integer, Map<Integer, Integer>> forceds = new HashMap<>();


        for (Map.Entry<ComponentVertex, Integer> entry : vertexDependences.entrySet()) {
            ComponentVertex v = entry.getKey();
            int vDepSetID = entry.getValue();

            // Edges incoming u->v - these are the 'forced'
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                ComponentVertex u = e.from;
                int uDepSetID = vertexDependences.get(u);

                if (vDepSetID != uDepSetID) {
                    int forced = verticesWithMinimumDependenceWeights.get(v) - verticesWithMinimumDependenceWeights.get(u);


                    if (!forceds.containsKey(uDepSetID)) forceds.put(uDepSetID, new HashMap<>());
                    if (!forceds.get(uDepSetID).containsKey(vDepSetID)) {
                        forceds.get(uDepSetID).put(vDepSetID, forced);
                    } else {
                        int currForced = forceds.get(uDepSetID).get(vDepSetID);
                        if (forced < currForced) {
                            forceds.get(uDepSetID).put(vDepSetID, forced);
                        }
                    }
                }
            }

        }

        // Now we build a graph with these weights, see what happens. Each vertex represents a dependence set.
        // we don't care about edge weight sum here

        Map<Vertex, Integer> depSetReductionToDepSetID = new HashMap<>();


        Graph<Vertex> depSetGraph = new Graph<>(GraphComponentFactory.STANDARD);

        Map<String, Map<Variable, Integer>> variableWeights = new HashMap<>();

        variableWeights.put("fcd", new HashMap<>());


//        System.out.println("Forceds: " + forceds);

        Variable.VariableFactory variableFactory = Variable.newFactory();

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

                Vertex v = depSetGraph.addOrGetCurrentVertex(Integer.toString(depSetV));
                Vertex w = depSetGraph.addOrGetCurrentVertex(Integer.toString(depSetW));

                depSetReductionToDepSetID.put(v, entry1.getKey());
                depSetReductionToDepSetID.put(w, entry2.getKey());

                depSetGraph.addEdgeFixedWeight(v, w, forced);


            }
        }

//        for (Vertex v : depSetGraph.vertices) {
//            System.out.println(showDependencyContents(v, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));
//        }

        // do the SCC thing here, check for cycles.


        // If we've succeeded, then our forced settings won't cause a conflict.
        // Also, the original forced graph is acyclic (since no cycles).


        // Now create the SCC of this new graph.
        // What we can probably do is propagate within each component in time O(VE) with Bellman-Ford equivalent.
        // I *think* once we've done that, we can rank components in the new SCC, set them in order, and each
        // component only takes O(V) to set?
        // First, we need to know the size of each component to know if it's worth it.

        ComponentGraph depsSCC = new SCC().runSCC(depSetGraph);



        // Looks like it's worth it - in the resulting graphs, it seems the vast majority of the components are of size 1,
        // so don't mutually influence; occasionally up to 4 or 5 (so really small!). Largest SCC component was size 9 in a graph of ~30 vertices.


        // Now we can look at each component's vertices in turn; ignore those of size 1, they need nothing done.
        // Then run the equivalent of Bellman-Ford within each component, until all edges have size 0 or more
        // or VE iterations are exceeded. We're trying to set each vertex relative to start points of 0.

        // So - initialise each vertex to +0. Initialise each edge to weight(e).
        // Then iterate: if a vertex has an incoming edge of weight w < 0, increase the weight of the vertex by
        // w, increase the weight of each incoming edge by w, and decrease the weight of each outgoing edge by w.
        // Repeat until nothing changes or VE iterations exceeded (think the B-F analysis still kind-of holds;
        // effectively we're trying to find a negative cycle, but the notation is different).
        // Assuming success - we'll have vertices of weights >= 0, and all edges of weights >= 0.
        // We could also try and find an early termination condition similar to in the other algorithm - say,
        // no vertex can end up with a weight more than the sum of abs(negative weights) in the SCC component.
        // Can also terminate (I think) if no vertex has weight 0 - then there isn't a minimum.


        Map<Vertex, Integer> vertexDifferences = new HashMap<>();

        Map<Edge<Vertex>, Integer> edgeDifferences = new HashMap<>();

        for (Map.Entry<ComponentVertex, Set<Vertex>> componentToContent : depsSCC.componentToContents.entrySet()) {
            Set<Vertex> content = componentToContent.getValue();

            content.forEach(v -> vertexDifferences.put(v, 0));

            int zeroVertices = content.size();

            if (content.size() == 1) continue; // we don't bother doing anything if the component has size 1

            Set<Edge<Vertex>> contentEdges = new HashSet<>();

            Map<Vertex, List<Edge<Vertex>>> incomingContentEdges = new HashMap<>();
            Map<Vertex, List<Edge<Vertex>>> outgoingContentEdges = new HashMap<>();

            Map<Edge<Vertex>, Integer> currDiffValue = new HashMap<>();

            for (Vertex v : content) {
                for (Edge<Vertex> e : depSetGraph.edgesOutgoing.get(v)) {
                    if (content.contains(e.to)) {
                        contentEdges.add(e);
                        if (!outgoingContentEdges.containsKey(v)) outgoingContentEdges.put(v, new ArrayList<>());
                        outgoingContentEdges.get(v).add(e);
                        currDiffValue.put(e, ((EdgeWeight.EdgeWeightInteger)e.weight).i);
                    }
                }
            }
            for (Vertex v : content) {
                for (Edge<Vertex> e : depSetGraph.edgesIncoming.get(v)) {
                    if (content.contains(e.from)) {
                        // contentEdges.add(e); only need to do this once!
                        if (!incomingContentEdges.containsKey(v)) incomingContentEdges.put(v, new ArrayList<>());
                        incomingContentEdges.get(v).add(e);
                        // currDiffValue.put(e, ((EdgeWeight.EdgeWeightInteger)e.weight).i); only need to do this once too
                    }
                }
            }

            int ve = content.size() * contentEdges.size();

            int maxNegative = 0;

            for (Edge<Vertex> e : contentEdges) {
                int w = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                if (w < 0) {
                    maxNegative += Math.abs(w);
                }
            }


            boolean change = true;

            boolean maxExceeded = false;

            int iter = 0;

            // We're taking Bellman-Ford as inspiration. Hope it works.
            while (change && iter <= ve + 1 && zeroVertices > 0 && !maxExceeded) {
                change = false;
                for (Edge<Vertex> e : contentEdges) {
                    if (currDiffValue.get(e) < 0) {
                        change = true;
                        Vertex v = e.to;

                        if (vertexDifferences.get(v).equals(0)) {
                            zeroVertices--;
                        }

                        int toAdd = Math.abs(currDiffValue.get(e));

                        // Update edges
                        for (Edge<Vertex> e1 : incomingContentEdges.get(v)) {
                            currDiffValue.put(e1, currDiffValue.get(e1) + toAdd);
                        }
                        for (Edge<Vertex> e1 : outgoingContentEdges.get(v)) {
                            currDiffValue.put(e1, currDiffValue.get(e1) - toAdd);
                        }

                        // Update vertex sum
                        vertexDifferences.put(v, vertexDifferences.get(v) + toAdd);

                        if (vertexDifferences.get(v) > maxNegative) maxExceeded = true;
                    }
                    iter++;
                }
            }

            if (iter > ve + 1 || zeroVertices == 0 || maxExceeded) {
                return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
            }


            for (Map.Entry<Edge<Vertex>, Integer> entry : currDiffValue.entrySet()) {
                edgeDifferences.put(entry.getKey(), entry.getValue());
            }

        }


        // Write this out

        Map<String, Map<Vertex, Integer>> vertexWeights = new HashMap<>();
        vertexWeights.put("diff", vertexDifferences);



        Map<Vertex, Map<String, String>> styling = new HashMap<>();

        for (Vertex v : depSetGraph.vertices) {
            styling.put(v, new HashMap<>());
            styling.get(v).put("color", colours[depSetReductionToDepSetID.get(v)]);
            styling.get(v).put("fillcolor", colours[depSetReductionToDepSetID.get(v)]);
        }


        Map<String, Map<Edge<Vertex>, Integer>> extraEdgeWeights = new HashMap<>();
        extraEdgeWeights.put("diff", edgeDifferences);

        if (isDebug) {
            depSetGraph.writeGraphVizStyledWithWeights(
                    fileName + "_forced_vals.dotgraph",
                    (Map) vertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    (Map) extraEdgeWeights,
                    styling,
                    new HashMap<>());
        }



        // That's done, so we've guaranteed that failure can't happen.
        // Then we can rank all components as previously, starting with 0-component, and
        // iteratively set the components whose values depend on previously-set components.
        // We'll need to think about a good way of setting size > 1 components

        // For each 1-sized component: look at all incoming sizes, pick the largest such.
        // For each >1-sized component: ??? for now.

        Map<ComponentVertex, Integer> ranksByDepSCCVertex = new HashMap<>();
        Map<Integer, List<ComponentVertex>> depSCCVerticesByRank = new HashMap<>();


        rankVerticesFromSources(depsSCC, ranksByDepSCCVertex, depSCCVerticesByRank);


        Map<Vertex, Integer> ranksByDepVertex = new HashMap<>();
        ranksByDepSCCVertex.forEach((sccV, rank) -> {
           sccV.componentContents.forEach(v -> {
               ranksByDepVertex.put(v, rank);
           });
        });

        vertexWeights.put("rank", ranksByDepVertex);


        if (isDebug) {
            depSetGraph.writeGraphVizStyledWithWeights(
                    fileName + "_zz_ranks.dotgraph",
                    (Map) vertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    (Map) extraEdgeWeights,
                    styling,
                    new HashMap<>());
        }


        // Rank 0 is always 0-component in Dependency SCC, containing 1 dependency vertex, which can be initialised with 0.

        // First we need all the vertices inside the 0-component of the standard dependency graph.
        // There should be a 0-component in the dependency SCC containing precisely the 0-component of the standard dependency graph.
        // Then we need to identify the 0-vertex within the 0-component of the standard dependency graph.
        // Finally we propagate from 0-vertex to ONLY those vertices within the 0-component of the standard dependency graph.

        int zeroDepComponent = vertexDependences.get(zeroVertex);
        Set<ComponentVertex> zeroDependencies = dependenceSetIDToDependenceSet.get(zeroDepComponent);

        Stack<ComponentVertex> verticesToSet = new Stack<>();
        verticesToSet.push(zeroVertex);

        Map<ComponentVertex, Integer> finalVertexWeights = new HashMap<>();
        finalVertexWeights.put(zeroVertex, 0);


        while (!verticesToSet.isEmpty()) {
            ComponentVertex u = verticesToSet.pop();

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(u)) {
                if (!finalVertexWeights.containsKey(e.to) && zeroDependencies.contains(e.to)) {
                    int w = ((EdgeWeight.EdgeWeightInteger)e.weight).i;
                    verticesToSet.push(e.to);
                    finalVertexWeights.put(e.to, finalVertexWeights.get(e.from) + w);
                }
            }

        }


        Map<String, Map<ComponentVertex, Integer>> allFinalVertexWeights = new HashMap<>();
        allFinalVertexWeights.put("w", finalVertexWeights);


        if (isDebug) {
            g.writeGraphVizStyledWithWeights(
                    fileName + "_zzz_1_zero_component_final.dotgraph",
                    (Map) allFinalVertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>(),
                    new HashMap<>());
        }


        // Now we need to set all the other Dependency SCC components in rank order.

        int nextRank = 1;

//        System.out.println(depSCCVerticesByRank);

        while (depSCCVerticesByRank.containsKey(nextRank)) {
//            System.out.println("Handling rank " + nextRank);
            // We go through the dependency SCC components in rank order.

            for (ComponentVertex depSccCV : depSCCVerticesByRank.get(nextRank)) {

//                System.out.println("Dealing with depSccCV " + depSccCV);
                // We need to know how many interdependent dependency sets there are
                // in the dependency SCC component.
                if (depSccCV.componentContents.size() == 1) {
                    System.out.println("Single-node Dependency SCC" + showDependencySCC(depSccCV, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));
//                    System.out.println(showDependencySCC(depSccCV, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));
                    // Comparatively easy to handle if only a single dependency set in this component - no interdependencies!
                    // Remember how difference edges on dependency sets A and B are defined:
                    // if the `smallest' vertices in the dependency sets A and B are set to 0, then
                    // the difference edge from A to B is an edge representing the 'smallest' connection from
                    // A to B - the lowest value of weight(b) - weight(a) for vertices b in B and a in A, where weight(v)
                    // is the weight of vertex v when the smallest vertices in v's dependence set have weight 0.
                    // Hence, when weight(smallest(A)) = weight(smallest(B)), the difference edges are correct.
                    // If weight(smallest(B)) = weight(smallest(A)) + 1, then the difference edge A->B of original weight
                    // w has adjusted weight w + 1.

                    // So: for each incoming difference edge e (of, say, weight w), we take the root of the dependency
                    // set that is the source of the edge, and look at its current weight W (this will be set, as done in rank order).
                    // Take the difference W - w; this is the weight the root of the current dependency set must be to have the
                    // original vertices joined by the original source of the difference edge as the same weight.
                    // We need the maximum of these values M to ensure no difference edge ends up as negative.
                    // Set the root of the dependency set to M, and propagate in both directions. Then this dependency set's vertices
                    // have the correct values.
                    // Also, each component (I think) has a difference edge of weight 0 incoming, so the minimum weight at the
                    // root of the dependency set should be 0.

                    // again, thank you Java for not providing any other way to access a set element. yay
                    Vertex dependencySetRepresentative = null;
                    for (Vertex v : depSccCV.componentContents) {
                        dependencySetRepresentative = v;
                    }

//                    System.out.println("Taken representative dependency set " + dependencySetRepresentative);

                    int thisDepSetID = depSetReductionToDepSetID.get(dependencySetRepresentative);

                    int bestWeight = -1; // weight has to be non- -ve, so this is a marker; also, the weight should be at least 0 (I think??) from incoming edges

                    for (Edge<Vertex> e : depSetGraph.edgesIncoming.get(dependencySetRepresentative)) {
                        Vertex from = e.from;

                        int fromDepSetID = depSetReductionToDepSetID.get(from);


                        int depWeightFromAtDepSetRoot =  finalVertexWeights.get(dependenceSetIDToDependenceSetRoot.get(fromDepSetID));

                        int adjustedWeight = depWeightFromAtDepSetRoot - ((EdgeWeight.EdgeWeightInteger)e.weight).i;

                        if (adjustedWeight > bestWeight) {
                            bestWeight = adjustedWeight;
                        }
                    }


                    assert bestWeight != -1;


                    // Now we need to propagate the most suitable weight from this dependency set's root component (any such root is fine
                    // as long as propagation is both forwards and backwards.)

                    propagateFromDependenceSetRoot(dependenceSetIDToDependenceSetRoot, finalVertexWeights, thisDepSetID, bestWeight);

                }
                else {
                    System.out.println("Dependency SCC of size " + depSccCV.componentContents.size() + " : " + showDependencySCC(depSccCV, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));

                    // Need to get this sorted NOW; not going to try being clever. Here's the plan:
                    // Initialise every dependence set to its minimum value based on its known incoming edges from
                    // outside the component - each has at least 1 incoming edge from 0, so this is possible.
                    // Set the difference edge weights appropriately.
                    // Then just do the O(VE) iteration on that; the SCC component stabilises when there aren't
                    // any negative-weight edges left. If changes still occurring after VE iterations, failure - there's a
                    // negative weight cycle, and the component has an impossible-to-satisfy set of constraints.


                    // Initialise vertex weights - same code as for the single-vertex SCC component, except filtering
                    // for only those incoming edges outside the SCC component
                    Map<Vertex, Integer> currRootAssignment = new HashMap<>();
                    for (Vertex dep : depSccCV.componentContents) {
                        int bestWeight = -1; // marker; this'll get set to 0+ by the incoming from 0-component

                        for (Edge<Vertex> e : depSetGraph.edgesIncoming.get(dep)) {
                            if (!depSccCV.componentContents.contains(e.from)) {
                                Vertex from = e.from;

                                int fromDepSetID = depSetReductionToDepSetID.get(from);


                                int depWeightFromAtDepSetRoot =  finalVertexWeights.get(dependenceSetIDToDependenceSetRoot.get(fromDepSetID));

                                int adjustedWeight = depWeightFromAtDepSetRoot - ((EdgeWeight.EdgeWeightInteger)e.weight).i;

                                if (adjustedWeight > bestWeight) {
                                    bestWeight = adjustedWeight;
                                }
                            }
                        }

                        if (bestWeight == -1) throw new RuntimeException("Failure: no suitable weight for vertex");

                        currRootAssignment.put(dep, bestWeight);

                    }

                    // Set up adjusted edge weights within component for new weight assignments
                    Set<Edge<Vertex>> newDiffEdges = new HashSet<>();
                    Map<Edge<Vertex>, Integer> newDiffEdgeWeights = new HashMap<>();


                    for (Vertex dep : depSccCV.componentContents) {
                        for (Edge<Vertex> e : depSetGraph.edgesOutgoing.get(dep)) {
                            if (depSccCV.componentContents.contains(e.to)) {
                                newDiffEdges.add(e);
                                newDiffEdgeWeights.put(e, currRootAssignment.get(e.to) - currRootAssignment.get(e.from));
                            }
                        }
                    }



                    // VE iterations till failure, or stability achieved before then

                    int count = 0; // this will increment V times
                    int vCount = depSccCV.componentContents.size();
                    int ve =  vCount * newDiffEdges.size();

                    boolean changed = true;

                    Set<Vertex> altered = new HashSet<>();


                    while (count <= ve + 1 && changed && altered.size() < vCount) {
                        changed = false;
                        for (Edge<Vertex> e : newDiffEdges) {
                            if (newDiffEdgeWeights.get(e) < 0) {
                                int toAdd = (-newDiffEdgeWeights.get(e));

                                altered.add(e.to);


                            }
                            count++;
                        }
                    }


                    if (count > ve + 1) {
                        // failure

                    }

                    if (altered.size() == vCount) {

                    }

/*
                    // What do we do if we have to simultaneously set several interdependent dependence sets?
                    // Each vertex in this situation has an added weight, and we've set all interrelating difference edges
                    // to >= 0.

                    // First we need to know exactly what the minimum for each dependence set is. This is calculated
                    // based on all edges incoming that are not from within this dependence SCC component - the
                    // external influences determine the minimum for each.
                    // Then we have to adjust this so that the relative vertex weights are satisfied. Not quite sure how yet.

                    // Assume we inject just a single value X (the best choice for the injected dependency set R, based on
                    // its external inputs) into this dependence set group, and the dependence set injected
                    // has relative weight 0. Then the root of the injected set R is set to X, and the roots of all other vertices
                    // are set to X + W, where W is the relative vertex weight of that vertex.

                    // Now assume that R had relative weight r > 0. We would effectively have to set it to weight 0, adjusting
                    // all the other relative vertex weights by W - r, then set all the other roots to their X + (W - r) new value.
                    // NOTE: a check would be needed to ensure that X + (W - r) >= 0 for all vertices, or the assignment is wrong,
                    // and would need to be corrected by adding sufficient to the incoming value that the lowest weight assigned was 0.

                    // Now: what do we do if we have multiple injection points?
                    // Ok, here's what we can do.
                    // 1) First, we find out the correct value for each dependency set based on its incoming edges, as above.
                    // 2) Next, group the dependency sets by their vertex difference value - all +0 together, all +1 together, etc.
                    // Because of how the vertex difference values are changed by incoming edges (effectively setting +n to +0 and
                    // changing other values relative to match), taking any vertex input at +i as correct means that and all its
                    // +i-differenced vertices are at +0. If we took a value of the +i vertices that wasn't the highest, then the
                    // highest wouldn't be set appropriately, it'd be set too low. So we can just take the largest dependency set
                    // value from each set of +i as representative.
                    // 3) Now we go through the +i representatives in order, starting at +0. We track two columns - best +i, current +j.
                    // where j > i.
                    // If we had our current-best as the value, the adjusted-difference-value of j would be j-i. What happens when we
                    // compare max[+i]+(j-i) to max[+j]? If max[+i]+(j-i) >= max[+j], the highest in +j would be satisfied, so we
                    // don't need to change. But if max[+i]+(j-i) < max[+j], then our current setting won't satisfy the highest in +j.
                    // (also, max[+i]+(j-i) >= max[+k] for k < i, so max[+j] > max[+k]). So we take +j as our new best +i.
                    // 4) The terminating value max[+i] gives the assignment we need to take, and we set everything appropriately from there.

                    // Can do this better with a counting sort for densely-packed dependence set differences, but compared to the
                    // O(VE) setup for vertex differences it doesn't really matter.


                    // 1) find the best value for each vertex based on external edges.
                    Map<Vertex, Integer> minimumDependenceVertexWeight = new HashMap<>();
                    Map<Integer, Integer> minimumDependenceWeightByID = new HashMap<>();


                    for (Vertex v : depSccCV.componentContents) {

                        int thisDepSetID = depSetReductionToDepSetID.get(v);

                        int bestWeight = -1; // weight has to be non- -ve, so this is a marker; also, the weight should be at least 0 (I think??) from incoming edges

                        for (Edge<Vertex> e : depSetGraph.edgesIncoming.get(v)) {
                            if (!depSccCV.componentContents.contains(e.from)) {
                                Vertex from = e.from;

                                int fromDepSetID = depSetReductionToDepSetID.get(from);


                                int depWeightFromAtDepSetRoot = finalVertexWeights.get(dependenceSetIDToDependenceSetRoot.get(fromDepSetID));

                                int adjustedWeight = depWeightFromAtDepSetRoot - ((EdgeWeight.EdgeWeightInteger) e.weight).i;

                                if (adjustedWeight > bestWeight) {
                                    bestWeight = adjustedWeight;
                                }
                            }
                        }

                        assert bestWeight != -1;

                        minimumDependenceVertexWeight.put(v, bestWeight);
                        minimumDependenceWeightByID.put(thisDepSetID, bestWeight);
                    }


                    // 2) Find the highest-weight dependence set in each of +0, +1, +2, ..., +n dependence set vertex differences
                    Map<Integer, Integer> highestWeightInDifferenceCategory = new HashMap<>(); // difference category -> weight
                    Map<Integer, Integer> highestWeightInDifferenceCategoryID = new HashMap<>(); // difference category -> dep set id

                    for (Vertex v : depSccCV.componentContents) {

                        int depSetID = depSetReductionToDepSetID.get(v);

                        int diffCat = vertexDifferences.get(v);


                        int bestDepSetValue = minimumDependenceVertexWeight.get(v);

                        if (!highestWeightInDifferenceCategory.containsKey(diffCat)) {
                            highestWeightInDifferenceCategory.put(diffCat, bestDepSetValue);
                            highestWeightInDifferenceCategoryID.put(diffCat, depSetID);
                        }
                        else {
                            int currValue = highestWeightInDifferenceCategory.get(diffCat);
                            if (bestDepSetValue > currValue) {
                                highestWeightInDifferenceCategory.put(diffCat, bestDepSetValue);
                                highestWeightInDifferenceCategoryID.put(diffCat, depSetID);
                            }

                        }

                    }

                    // 3) Find the highest value when we go through in sorted order.
                    // First we sort the difference categories (this is inefficient, just want it implemented for now)
                    List<Integer> diffCatsSorted = new ArrayList<>(highestWeightInDifferenceCategory.keySet());
                    diffCatsSorted.sort(Integer::compareTo);

                    // Now, go through from smallest difference category to largest
                    int currBestDiffCatIndex = 0;
                    int compareDiffCatIndex = 1;

                    while (compareDiffCatIndex < diffCatsSorted.size()) {
                        int diffCatBest = diffCatsSorted.get(currBestDiffCatIndex);
                        int diffCatCurr = diffCatsSorted.get(compareDiffCatIndex);

                        int diffCatDiff = diffCatCurr - diffCatBest;  // (j-i)

                        // Compare max[+i]+(j-i) to max[+j]. If max[+i]+(j-i) >= max[+j], the highest in +j would be satisfied, so we
                        // don't need to change. But if max[+i]+(j-i) < max[+j] then our current setting won't satisfy the highest in +j.
                        // (also, max[+i]+(j-i) >= max[+k] for k < i, so max[+j] > max[+k]). So we take +j as our new best +i.


                        if (highestWeightInDifferenceCategory.get(diffCatBest) + diffCatDiff < highestWeightInDifferenceCategory.get(diffCatCurr)) {
                            currBestDiffCatIndex = compareDiffCatIndex;
                        }

                        compareDiffCatIndex++;
                    }


                    // 4) The terminating value max[+i] gives the assignment we need to take, and we set everything appropriately from there.
                    int bestDiffCat = diffCatsSorted.get(currBestDiffCatIndex);

                    int bestWeight = highestWeightInDifferenceCategory.get(bestDiffCat);

                    for (Vertex v : depSccCV.componentContents) {
                        System.out.println("Assigning weight to " + v);

                        int adjustedDiff = vertexDifferences.get(v) - bestDiffCat;

                        int weightToAssign = bestWeight + adjustedDiff;

                        int depSetID = depSetReductionToDepSetID.get(v);

                        propagateFromDependenceSetRoot(dependenceSetIDToDependenceSetRoot, finalVertexWeights, depSetID, weightToAssign);
                    }
*/



                }


            }






            nextRank++;
        }




        // Finally, we need to set the original variable-weight edges to their appropriate values based on the new vertex assignments.

        Map<Variable, Integer> finalEdgeWeights = new HashMap<>();

//        System.out.println(finalVertexWeights);

        for (Edge<ComponentVertex> e : g.edges) {
            if (e.isVariableWeight()) {
                Variable eVar = ((EdgeWeight.EdgeWeightVariable)e.weight).v;

                Integer toWeight = finalVertexWeights.get(e.to);
                Integer fromWeight = finalVertexWeights.get(e.from);

                int w;

                if (toWeight == null || fromWeight == null) {
                    // something has gone very badly wrong
//                    System.out.println("Algorithm resulted in unassigned weight.");
                    w = -999;
                }
                else {
                    w = toWeight - fromWeight;
                }

                assert w >= 0;

                finalEdgeWeights.put(eVar, w);
            }
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

        System.out.println("Propagating from " + thisPropagationRoot + " with weight " + bestWeight);

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


    private Result initialiseDependenceSetFromAllKnown(Set<ComponentVertex> dependenceSet,
                                                       Set<ComponentVertex> smallestVertices,
                                                       Map<ComponentVertex, Integer> vertexWeights,
                                                       Map<Variable, Integer> edgeVariableWeights,
                                                       Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights,
                                                       Map<ComponentVertex, Integer> vertexDependences) {

        // First we need to look at the smallest vertices in this dependence set.
        // We set them equal to the highest weight of all their incoming vertices (so that specific edge to weight 0 and all
        // others positive for those smallest vertices) and propagate this weight throughout the set.
        // Then we scan all edges coming in, looking for contradictions - is there any point at which this weight setting causes
        // a negative variable-weight edge? If any, we take the largest such contradiction (the largest negative weight for
        // a variable edge) and add abs(that) to all the set's vertices, so all incoming edges are positive.
        // This shouldn't (??) cause any problems, but just in case, we run through again and check that everything is now fine.

        int maxWeightIncomingToSmallest = -1;

        for (ComponentVertex v : smallestVertices) {
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (!dependenceSet.contains(e.from) && vertexWeights.get(e.from) > maxWeightIncomingToSmallest) {
                    maxWeightIncomingToSmallest = vertexWeights.get(e.from);
                }
            }
        }

        if (maxWeightIncomingToSmallest == -1)
            throw new RuntimeException("Managed to find no edges incoming to a smallest vertex in a dependent set");

        for (ComponentVertex v : smallestVertices) {
            vertexWeights.put(v, maxWeightIncomingToSmallest);
        }

        // We can start at any smallest vertex, and do the propagation forwards and backwards.
        Stack<ComponentVertex> toDo = new Stack<>();
        toDo.addAll(smallestVertices);

        Set<ComponentVertex> inProgress = new HashSet<>();

        while (!toDo.isEmpty()) {
            //System.out.println(vertexWeights);
            ComponentVertex v = toDo.pop();
            //System.out.println("NEXT VERTEX " + v);
            int vWeight = vertexWeights.get(v);

            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                if (!e.isVariableWeight() && dependenceSet.contains(e.to) && !inProgress.contains(e.to)) {
                    int w = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (!inProgress.contains(e.to)) {
                        inProgress.add(e.to);
                        toDo.push(e.to);
                        vertexWeights.put(e.to, vWeight + w);
                    }
                    toDo.push(e.to);
                    inProgress.add(e.to);
                }
            }


            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (!e.isVariableWeight() && dependenceSet.contains(e.from) && !inProgress.contains(e.from)) {
                    int w = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                    if (!inProgress.contains(e.from)) {
                        inProgress.add(e.from);
                        toDo.push(e.from);
                        vertexWeights.put(e.from, vWeight - w);
                    }
                    toDo.push(e.from);
                    inProgress.add(e.from);
                }
            }
        }


        // Now we check the perimeter to find the smallest negative edge variable weight
        int smallest = 1;
        for (ComponentVertex v : dependenceSet) {
            int vWeight = vertexWeights.get(v);

            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (e.isVariableWeight()) {
                    int uWeight = vertexWeights.get(e.from);
                    if (vWeight - uWeight < smallest) smallest = vWeight - uWeight;
                }
            }
        }


        // Best case, smallest is >= 0, in which case, great.
        // If one such negative edge variable weight w exists, add abs(w) to all vertices in the set

        if (smallest < 0) {
            for (ComponentVertex v : dependenceSet) {
                vertexWeights.put(v, vertexWeights.get(v) + Math.abs(smallest));
            }
        }


        // Finally, set edge variable weights, along with last check for negatives
        for (ComponentVertex v : dependenceSet) {
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                if (e.isVariableWeight()) {
                    int vWeight = vertexWeights.get(v);
                    int uWeight = vertexWeights.get(e.from);

                    if (vWeight - uWeight < 0) return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;

                    Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;

                    edgeVariableWeights.put(eVar, vWeight - uWeight);

                }
            }
        }


        return Result.SUCCESS;

    }

    private Result computeDependenceValue(Set<ComponentVertex> dependenceSet, Map<ComponentVertex, Integer> dependenceValues, Map<Set<ComponentVertex>, Set<ComponentVertex>> mostDepended, Map<Variable, Integer> edgeVariableWeights, Map<Integer, ComponentVertex> dependenceRoots, int dependenceSetID) {
        ComponentVertex arb = null;

        // Get an arbitrary starting vertex out
        for (ComponentVertex v : dependenceSet) {
            arb = v;
            break;
        }

        // Pass starting at arbitrary point, just to find the least vertices in the dependence

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
        for (ComponentVertex v : min) {
            dependenceRoots.put(dependenceSetID, v);
            break;
        }



        int toAdd = 0 - minValue;

        for (ComponentVertex v : dependenceSet) {
            dependenceValues.put(v, dependenceValues.get(v) + toAdd);
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
