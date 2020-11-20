package graph;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Commenting of Algorithm_ByDependentVertex; also deleting stuff that isn't necessary,
 * because I cannot for the life of me work out what I was doing in ByDependentVertex. The worst coder, as always, is me, a year ago.
 * To me if I view this again later: hello, hope things are going better, sorry about the code.
 *
 * Comments "NEW NOTE:" are added in this revision for clarity
 */
public class Algorithm_ByDependentVertex_Cleaned extends Algorithm {

    private static List<Map<Integer, Integer>> temp = new ArrayList<>();

    public static void printTemp() {
        StringJoiner sj = new StringJoiner("\n");
        temp.forEach(m -> sj.add(m.toString()));
        System.out.println(sj.toString());
    }

    public Algorithm_ByDependentVertex_Cleaned(ComponentGraph g) {
        super(g);
    }

    String[] colours = {"red", "green", "lightblue", "magenta", "cyan", "yellow", "orange", "slategrey", "blue", "lightred"};

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
            styling.get(v).put("color", colours[vertexToDepSetID.get(v)]);
        }


        for (Set<ComponentVertex> mins : mostDepended.values()) {
            for (ComponentVertex v : mins) {
                if (!styling.containsKey(v)) styling.put(v, new HashMap<>());
                styling.get(v).put("fillcolor", colours[vertexToDepSetID.get(v)]);
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
     * @param vertexDependences map each vertex v_{x_i + p} -> its dependence set ID. note: not just the minimum or a representative, all vertices!
     * @param verticesWithMinimumDependenceWeights  for each vertex, the amount it is greater than the minimum value in its "dependence set"
     * @param dependenceSetIDToDependenceSet map of dependence set ID -> dependence set
     * @param dependenceSetIDToDependenceSetRoot mapping of dependence set ID -> any of the minimum vertices in that dependence set
     * @return
     */
    private Result createSlackGraph(boolean isDebug,
                                    String fileName,
                                    ComponentVertex zeroVertex,
                                    Map<ComponentVertex, Integer> vertexDependences,
                                    Map<ComponentVertex, Integer> verticesWithMinimumDependenceWeights,
                                    Map<Integer, Set<ComponentVertex>> dependenceSetIDToDependenceSet,
                                    Map<Integer, ComponentVertex> dependenceSetIDToDependenceSetRoot) {

        /*
        System.out.println("VERTEX DEPENDENCES: " + vertexDependences.toString());
        System.out.println("VERTICES W/ MIN DEP WEIGHTS: " + verticesWithMinimumDependenceWeights.toString());
        System.out.println("DEP SET ID TO DEP SET: " + dependenceSetIDToDependenceSet.toString());
        System.out.println("DEP SET ID TO DEP SET ROOT: " + dependenceSetIDToDependenceSetRoot);
        */

        // NEW NOTE:
        // "Slack graph" is our graph R, give or take -
        // the vertices are labelled by the "dependence set" ID number  associated with S_{x_i} rather than just by r_{x_i}.
        // 'minimum dependence weight' is the value p - p_min for a vertex.
        // "forced" is our (p_d - p_min) - (q_d - q_min), except it's the other way around for different sign: (q_d - q_min) - (p_d - p_min)
        // so "forced" for u->v is (0 - w(u->v)) in the writeup
        // IMPORTANT DIFFERENCE WITH WRITEUP: difference here between algorithm-as-Java and algorithm-as-writeup is that in the writeup we use
        // a positive edge weight to indicate for controlling edge u->v, w(u) > w(v), and a negative weight for w(u) < w(v).
        // In this implementation it's the other way around, so -ve edge weight indicates need to increase v, and +ve edge
        // weight indicates 'slack'.
        // So instead of the max of (p_d - p_min) - (q_d - q_min) for a given r_{x_i} -> r_{x_j}, is the minimum of (q_d - q_min) - (p_d - p_min)



        // OLD COMMENTS:
        // 'Slack graph'. For each dependence set, we want to analyse the links between them, starting with their 'minimum dependence weights',
        // to look at their relative differences.
        // For dependence sets A and B:
        // the 'slack' from A to B is, if any, the amount we can increase the weights in A before the weights in B start increasing; it's given by the smallest
        // weight difference between a vertex in B and a vertex in A. <= 0 slack means increasing A immediately increases B.
        // the 'forced' from B to A is the minimum negative weight between a vertex in A and a vertex in B; this indicates that
        // A must be AT LEAST abs(forced(A,B)) larger than B in the solution.
        // A -> B -> forced

        // NEW NOTE:
        // this is where we compute our initial w(u->v); or rather, our (0 - w(u->v))
        // effectively a map of r_{x_i} -> (r_{x_j} -> ((q_d - q_min) - (p_d - p_min))) for controlling edge r_{x_i} -> r_{x_j}
        // except done with the dependence set ID for r_{x_i} rather than r_{x_i}, and we're not looking up edges but edges implied by a pair of dependence set ids
        Map<Integer, Map<Integer, Integer>> forceds = new HashMap<>();

        for (Map.Entry<ComponentVertex, Integer> entry : vertexDependences.entrySet()) {
            ComponentVertex v = entry.getKey();
            int vDepSetID = entry.getValue();

            // Edges incoming u->v - these are the 'forced'
            for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                ComponentVertex u = e.from;
                int uDepSetID = vertexDependences.get(u);

                // NEW NOTE: update the weight of the implied controlling edge from r_u -> r_v
                if (vDepSetID != uDepSetID) {
                    // NEW NOTE: (q - q_min) - (p - p_min) for edge e
                    int forced = verticesWithMinimumDependenceWeights.get(v) - verticesWithMinimumDependenceWeights.get(u);


                    if (!forceds.containsKey(uDepSetID)) forceds.put(uDepSetID, new HashMap<>());
                    if (!forceds.get(uDepSetID).containsKey(vDepSetID)) {
                        forceds.get(uDepSetID).put(vDepSetID, forced);
                    } else {
                        int currForced = forceds.get(uDepSetID).get(vDepSetID);
                        // NEW NOTE: ensure minimum value is taken for the controlling edge (since we're using the opposite signs)
                        if (forced < currForced) {
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

        Map<Vertex, Integer> depSetReductionToDepSetID = new HashMap<>();

        Graph<Vertex> representativeGraph = new Graph<>(GraphComponentFactory.STANDARD);

        Map<String, Map<Variable, Integer>> variableWeights = new HashMap<>();

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

        // OLD COMMENT:
        // do the SCC thing here, check for cycles.
        // If we've succeeded, then our forced settings won't cause a conflict.
        // Also, the original forced graph is acyclic (since no cycles).            -- NEW NOTE: think this was referring to graph H having been 'forced'
        // Now create the SCC of this new graph.

        ComponentGraph depsSCC = new SCC().runSCC(representativeGraph);


        // NEW NOTE: the below describes it pretty well. Writeup has some cycle r_{x_1} -> r_{x_2} -> ... -> r_{x_1}
        // set the value as it goes; this one instead uses a relative weights idea, and doesn't actually set anything
        // to its final value until it knows whether or not there are infinite cycles in *any* comp(c^R).
        // It does what amounts to taking comp(c^R), and if |comp(c^R)| > 1, relax all negative edges in the cycle
        // (the positive ones in the writeup version)  to increase initial _relative_ vertex weights, and terminates
        // if the _relative_ weights aren't set after O(VE) relaxations. Then if it succeeds, it can go on and set
        // relative to the lowest _relative_ weight vertex in each cycle (which should be of weight 0).
        //
        // OLD COMMENT:
        // What we can probably do is propagate within each component in time O(VE) with Bellman-Ford equivalent.
        // I *think* once we've done that, we can rank components in the new SCC, set them in order, and each
        // component only takes O(V) to set?
        // First, we need to know the size of each component to know if it's worth it.
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

            if (content.size() == 1) continue; // OLD COMMENT: we don't bother doing anything if the component has size 1

            Set<Edge<Vertex>> contentEdges = new HashSet<>();

            Map<Vertex, List<Edge<Vertex>>> incomingContentEdges = new HashMap<>();
            Map<Vertex, List<Edge<Vertex>>> outgoingContentEdges = new HashMap<>();

            Map<Edge<Vertex>, Integer> currDiffValue = new HashMap<>();

            for (Vertex v : content) {
                for (Edge<Vertex> e : representativeGraph.edgesOutgoing.get(v)) {
                    if (content.contains(e.to)) {
                        contentEdges.add(e);
                        if (!outgoingContentEdges.containsKey(v)) outgoingContentEdges.put(v, new ArrayList<>());
                        outgoingContentEdges.get(v).add(e);
                        currDiffValue.put(e, ((EdgeWeight.EdgeWeightInteger)e.weight).i);
                    }
                }
            }
            for (Vertex v : content) {
                for (Edge<Vertex> e : representativeGraph.edgesIncoming.get(v)) {
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

            // OLD COMMENT: We're taking Bellman-Ford as inspiration. Hope it works.
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

                        // OLD COMMENT: Update edges
                        for (Edge<Vertex> e1 : incomingContentEdges.get(v)) {
                            currDiffValue.put(e1, currDiffValue.get(e1) + toAdd);
                        }
                        for (Edge<Vertex> e1 : outgoingContentEdges.get(v)) {
                            currDiffValue.put(e1, currDiffValue.get(e1) - toAdd);
                        }

                        // OLD COMMENT: Update vertex sum
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


        // OLD COMMENT: Write this out

        Map<String, Map<Vertex, Integer>> vertexWeights = new HashMap<>();
        vertexWeights.put("diff", vertexDifferences);



        Map<Vertex, Map<String, String>> styling = new HashMap<>();

        for (Vertex v : representativeGraph.vertices) {
            styling.put(v, new HashMap<>());
            styling.get(v).put("color", colours[depSetReductionToDepSetID.get(v)]);
            styling.get(v).put("fillcolor", colours[depSetReductionToDepSetID.get(v)]);
        }


        Map<String, Map<Edge<Vertex>, Integer>> extraEdgeWeights = new HashMap<>();
        extraEdgeWeights.put("diff", edgeDifferences);

        if (isDebug) {
            representativeGraph.writeGraphVizStyledWithWeights(
                    fileName + "_forced_vals.dotgraph",
                    (Map) vertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    (Map) extraEdgeWeights,
                    styling,
                    new HashMap<>());
        }



        // OLD COMMENT:
        // That's done, so we've guaranteed that failure can't happen.
        // Then we can rank all components as previously, starting with 0-component, and
        // iteratively set the components whose values depend on previously-set components.
        // We'll need to think about a good way of setting size > 1 components
        // For each 1-sized component: look at all incoming sizes, pick the largest such.
        // For each >1-sized component: ??? for now.

        Map<ComponentVertex, Integer> ranksByDepSCCVertex = new HashMap<>();
        Map<Integer, List<ComponentVertex>> reprComponentsByRank = new HashMap<>();


        rankVerticesFromSources(depsSCC, ranksByDepSCCVertex, reprComponentsByRank);


        Map<Vertex, Integer> ranksByDepVertex = new HashMap<>();
        ranksByDepSCCVertex.forEach((sccV, rank) -> {
           sccV.componentContents.forEach(v -> {
               ranksByDepVertex.put(v, rank);
           });
        });

        vertexWeights.put("rank", ranksByDepVertex);


        if (isDebug) {
            representativeGraph.writeGraphVizStyledWithWeights(
                    fileName + "_zz_ranks.dotgraph",
                    (Map) vertexWeights,
                    new HashMap<>(),
                    new HashMap<>(),
                    (Map) extraEdgeWeights,
                    styling,
                    new HashMap<>());
        }


        // OLD COMMENT:
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

        //System.out.println("DEPSCCVERTICESBYRANK: " + reprComponentsByRank);


        while (reprComponentsByRank.containsKey(nextRank)) {
            // We go through the dependency SCC components in rank order.

            for (ComponentVertex reprComponent : reprComponentsByRank.get(nextRank)) {

                // We need to know how many interdependent dependency sets there are
                // in the dependency SCC component.
                if (reprComponent.componentContents.size() == 1) {
                    System.out.println("Single-node Dependency SCC" + showDependencySCC(reprComponent, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));
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
                    for (Vertex v : reprComponent.componentContents) {
                        dependencySetRepresentative = v;
                    }

//                    System.out.println("Taken representative dependency set " + dependencySetRepresentative);

                    int thisDepSetID = depSetReductionToDepSetID.get(dependencySetRepresentative);

                    int bestWeight = -1; // weight has to be non- -ve, so this is a marker; also, the weight should be at least 0 (I think??) from incoming edges

                    for (Edge<Vertex> e : representativeGraph.edgesIncoming.get(dependencySetRepresentative)) {
                        Vertex from = e.from;

                        int fromDepSetID = depSetReductionToDepSetID.get(from);

                        ComponentVertex depSetRoot = dependenceSetIDToDependenceSetRoot.get(fromDepSetID);
                        int depWeightFromAtDepSetRoot =  finalVertexWeights.get(depSetRoot);

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
                    System.out.println("Dependency SCC of size " + reprComponent.componentContents.size() + " : " + showDependencySCC(reprComponent, depSetReductionToDepSetID, dependenceSetIDToDependenceSet));

                    // Need to get this sorted NOW; not going to try being clever. Here's the plan:
                    // Initialise every dependence set to its minimum value based on its known incoming edges from
                    // outside the component - each has at least 1 incoming edge from 0, so this is possible.
                    // Set the difference edge weights appropriately.

                    // Initialise vertex weights - same code as for the single-vertex SCC component, except filtering
                    // for only those incoming edges outside the SCC component


                    Map<Vertex, Integer> currRootAssignment = new HashMap<>(); // this is a TEMPORARY store

                    for (Vertex reprV : reprComponent.componentContents) {
                        //System.out.println("GOT HERE");
                        //System.out.println("VERTEX " + reprV);
                        int bestWeight = -1; // marker; this'll get set to 0+ by the incoming from 0-component
                        Vertex bestTo = null;

                        for (Edge<Vertex> e : representativeGraph.edgesIncoming.get(reprV)) {

                            if (!reprComponent.componentContents.contains(e.from)) {
                                //System.out.println("HERE TOO");
                                Vertex reprVFrom = e.from;

                                int fromDepSetID = depSetReductionToDepSetID.get(reprVFrom);

                                int depWeightFromDepSetRoot = finalVertexWeights.get(dependenceSetIDToDependenceSetRoot.get(fromDepSetID));

                                int adjustedWeight = depWeightFromDepSetRoot - ((EdgeWeight.EdgeWeightInteger)e.weight).i;

                                System.out.println();


                                if (adjustedWeight > bestWeight) {
                                    bestWeight = adjustedWeight;
                                    bestTo = e.to;
                                }
                            }
                        }

                        if (bestWeight == -1) throw new RuntimeException("Failure: no suitable weight for vertex");

                        currRootAssignment.put(reprV, bestWeight);
                        System.out.println("CURR ROOT ASSIGNMENT: " + currRootAssignment);

                        propagateFromDependenceSetRoot(dependenceSetIDToDependenceSetRoot, finalVertexWeights, depSetReductionToDepSetID.get(reprV), bestWeight);

                    }


                    // Set up adjusted edge weights within component for new weight assignments
                    // NEW NOTE: we set the final vertex weights later, from these edge weights, I think
                    // but that means we don't have the final vertex weights in place for
                    Set<Edge<Vertex>> newDiffEdges = new HashSet<>();
                    Map<Edge<Vertex>, Integer> newDiffEdgeWeights = new HashMap<>();


                    for (Vertex dep : reprComponent.componentContents) {
                        for (Edge<Vertex> e : representativeGraph.edgesOutgoing.get(dep)) {
                            if (reprComponent.componentContents.contains(e.to)) {
                                newDiffEdges.add(e);
                                newDiffEdgeWeights.put(e, currRootAssignment.get(e.to) - currRootAssignment.get(e.from));
                            }
                        }
                    }



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
