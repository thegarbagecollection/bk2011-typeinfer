package graph;

import timeconstraints.GraphSolvedTimeConstraintSet;

import java.util.*;
import java.util.stream.Collectors;

public class Algorithm_ForwardBackward_RepeatPropagation extends Algorithm {


    public Algorithm_ForwardBackward_RepeatPropagation(ComponentGraph g) {
        super(g);
    }

    /*
    public Result firstPartDebug_v2() {
        if (!checkSelfLoops()) return Result.FAILURE_SELF_LOOP;

        if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

        return Result.SUCCESS;
    }

    public Result secondPartDebug_v2(String fileName) {
        return checkPathLengths2(fileName, true);
    }

    public Result run_v2() {
        if (!checkSelfLoops()) {
            return Result.FAILURE_SELF_LOOP;
        }

        if (!reduceMultiEdges()) return Result.FAILURE_ADJACENT_DIFFERENT_WEIGHTS;

        Result r = checkPathLengths2("", false);

        return r;
    }
*/

    // Third version. Split up forwards and backwards propagations.
    /*
        Start with an immutably-weighted set of vertices - these count as 'changed vertices' V

        Vertices can only increase in weight.

        Now repeat until V={}:
        1) forward propagate from all vertices in V
           we don't quite want a standard DFS - if we reach a point where the weight is greater, update and continue from updated
           idea here is to push the minimum value of a vertex forwards
           let V' = vertices whose weights change.
        2) backwards propagate from all vertices in V' where possible along fixed-weight edges, updating variable-weight edges
           idea is to 'pull up' those vertices with weights too small.
           let V = vertices whose weights change.


     */
    Result checkPathLengths(String fileName, boolean isDebug) {
        Map<ComponentVertex, Integer> vertexWeights = new HashMap<>();
        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        int finalVariableEdgeCount = g.edges.stream().filter(Edge::isVariableWeight).collect(Collectors.toList()).size();

        // Set up immutable vertices - vertices whose weight MUST equal a specific value
        Set<ComponentVertex> immutableVertices = new HashSet<>();

        setInitialSources(vertexWeights);

        // Note: since these are source vertices by definition, they have no backwards edges, so we don't need to worry about back-propagating from them.
        Set<ComponentVertex> source = getSourceVertices(vertexWeights);

//        for (ComponentVertex u : source) {
//            Result x = propagateKnownWeight(vertexWeights, edgeVariableWeights, u);
//
//            if (x != null) {
//                saveCurrentWeights(vertexWeights, edgeVariableWeights);
//                return x;
//            }
//        }

        immutableVertices.addAll(vertexWeights.keySet());


        printIntermediates2(isDebug, vertexWeights, fileName, edgeVariableWeights, 0, "immutables");


        Set<ComponentVertex> forChangeForward = new HashSet<>(immutableVertices);
        Set<ComponentVertex> forChangeBackward = new HashSet<>();

        int iteration = 0;

        while (forChangeForward.size() > 0) {
            iteration++;
            //System.out.println("Forward traversals from: " + forChangeForward);
            forChangeBackward.clear();

            for (ComponentVertex u : forChangeForward) {
                Result x = forwardPropagate(vertexWeights, edgeVariableWeights, immutableVertices, u, forChangeBackward);
                if (x != null) {
                    saveCurrentWeights(vertexWeights, edgeVariableWeights);
                    return x;
                }
            }

//            System.out.println("Vertex weights: " + vertexWeights);
//            System.out.println("Edge weights: " + edgeVariableWeights);
//            System.out.println("");
            forChangeForward.clear();

            printIntermediates2(isDebug, vertexWeights, fileName, edgeVariableWeights, iteration, "a_fwrd");
            //System.out.println("Backward traversals from: " + forChangeBackward);
            for (ComponentVertex u : forChangeBackward) {
                Result x = backwardPropagate(vertexWeights, edgeVariableWeights, immutableVertices, u, forChangeForward);
                if (x != null) {
                    saveCurrentWeights(vertexWeights, edgeVariableWeights);
                    return x;
                }
            }

            //System.out.println("Vertex weights: " + vertexWeights);
            //System.out.println("Edge weights: " + edgeVariableWeights);

            //System.out.println("");
            printIntermediates2(isDebug, vertexWeights, fileName, edgeVariableWeights, iteration, "b_back");

        }


        // We still have to check we've reached termination with all vertices assigned a weight, though
        // Same as before:

        int finalVertexWeightCount = vertexWeights.keySet().size();
        int finalVertexCount = g.vertices.size();

        int finalEdgeWeightCount = edgeVariableWeights.keySet().size();


        assignWeightsToGraph(vertexWeights, edgeVariableWeights);

        //System.out.println("Terminating check: " + finalVertexWeightCount + "/" + finalVertexCount + " vertices, " + finalEdgeWeightCount + "/" + finalVariableEdgeCount + " variable edges.");
        // We need to check all vertices and edges have weights assigned! This should be guaranteed by termination, but just in case:
        if (finalVertexCount != finalVertexWeightCount || finalEdgeWeightCount != finalVariableEdgeCount) {
            saveCurrentWeights(vertexWeights, edgeVariableWeights);
            return Result.FAILURE_UNASSIGNED_VERTEX_OR_EDGE_WEIGHTS;
        }

        // Set weights both in the SCC graph and in the original problem graph.


        return Result.SUCCESS;


    }


    private Result forwardPropagate(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, Set<ComponentVertex> immutableVertices, ComponentVertex u, Set<ComponentVertex> forChangeBackward) {

        /* Slightly different idea for forward propagation - if there's an edge, take it, assign a weight unless a larger one is already assigned.
         If edge e=v->b is variable-weight:
         - if we can leave b same (i.e. weight(b) > weight(v)), we do, just adjust weight(e) to weight(b)-weight(v)
         - if weight(b) <= weight(v), set weight(e)=0 and weight(b)=weight(v), and add b to forChangeBackward; watch for changes to immutables (although they shouldn't change since they're all sources in the original SCC)
           also continue search from b
         If edge e=v->b is fixed-weight w:
         - if weight(b) < weight(v)+w, set weight(b)=weight(v)+w, add b to forChangeBackward; if weight(b) > weight(v)+w fail. watch for immutables WRONG!!
           not sure about this one - is it possible for weight(b) > weight(v) + w from another route? if it is, we'd need to mark v as to be repaired, but that
           might be ok? yep - consider a diamond (directed downwards)

                0
              y/ \4
              A   B
              1\ /x
                C

            Obviously the minimums are B=C=4, A=3. But if we take 0->B->C first, then B=C=4, and when we take 0->A->C, we set A >= 0, then C=4 > A + 1 >= 1.
            So in this case, we add C to a backwards propagation but don't change it; we'll need to bring A up, along with y.

            So: if weight(b) < weight(v)+w, set weight(b)=weight(v)+w and add b to forChangeBackward; if weight(b) > weight(v)+w, add b to forChangeBackward.
                we can also stop traversing v's edges - v is wrong, and needs to be updated.
                in case of weight(b) >= weight(v)+w, we don't traverse further along b.


            NOTE: modification to make based on ex_1_5_3; if you've got (edges down)
                    V
                  2/ \
                  W  |2
                  1\ /
                    X
            then you need to detect the different-length paths immediately or get stuck in a cycle of continuous incrementing.
            So - when we set a previous vertex, ensure that it hasn't already been set to something different by this iteration -
            if it has, we have two paths of different length, so must fail. WRONG.

            We're going to take an easy way - the maximum variable value must by definition be <= the sum total of all edge weights in the original
            graph. Hence if we ever exceed such a value for a vertex weight, we've failed.
         */
        Stack<ComponentVertex> stack = new Stack<>(); // anything on the stack, or in progress, has a weight assigned
        Set<ComponentVertex> inProgress = new HashSet<>();
        stack.push(u);


        //System.out.println("Starting search from sink vertex " + u);

        while (!stack.empty()) {
            ComponentVertex v = stack.peek();

            //System.out.println("Stack top: " + v);

            if (inProgress.contains(v)) {
                //System.out.println(v + " completed. Removing from stack and in-progress.");
                inProgress.remove(v);
                stack.pop();
            } else {
                //System.out.println(v + " newly-seen.");
                inProgress.add(v);
                // Traverse outgoing edges
                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    ComponentVertex b = e.to;
                    if (e.isVariableWeight()) {
                        /*
                        If edge e=v->b is variable-weight:
                        - if we can leave b same (i.e. weight(b) > weight(v)), we do, just adjust weight(e) to weight(b)-weight(a)
                        - if weight(b) <= weight(v), set weight(e)=0 and weight(b)=weight(v), and add b to forChangeBackward; watch for changes to immutables (although they shouldn't change since they're all sources in the original SCC)
                          also continue search from b
                        - ADDITIONAL: what if weight(b) is undefined? just treat that as -infinity, so set weight(b)=weight(v) and weight(e)=0, add to forChangeBackward. Isn't immutable since not set.
                        */
                        Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;

                        if (vertexWeights.containsKey(b)) {
                            int weightDiff = vertexWeights.get(b) - vertexWeights.get(v);

                            if (weightDiff >= 0) {
                                edgeVariableWeights.put(eVar, weightDiff);
                            } else { // weight(v) > weight(b)
                                if (immutableVertices.contains(b)) {
                                    return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                                } else {
                                        edgeVariableWeights.put(eVar, 0);
                                        vertexWeights.put(b, vertexWeights.get(v));
                                        forChangeBackward.add(b);
                                        stack.push(b);
                                        vertexIncrementCount();
                                }
                            }
                        } else {

                                edgeVariableWeights.put(eVar, 0);
                                vertexWeights.put(b, vertexWeights.get(v));
                                vertexIncrementCount();
                                forChangeBackward.add(b);
                                stack.push(b);
                        }
                    } else {
                        /*
                            Edge e=v->b is fixed-weight w:
                            if weight(b) < weight(v)+w, set weight(b)=weight(v)+w and add b to forChangeBackward; if weight(b) > weight(v)+w, add b to forChangeBackward; if equal, do nothing
                            we can also stop traversing v's edges - v is wrong, and needs to be updated;
                            in case of weight(b) >= weight(v)+w, we don't traverse further along b.
                            ADDED: if weight(b) not defined, weight(b) = weight(v)+w, add b to forChangeBackward, traverse from b
                            Could make this faster if we stored the vertices in order weight(b)-weight(v->b); early seeing of maximum outgoing leads to earliest termination,
                            not going to bother, it's small.
                         */
                        int edgeWeight = ((EdgeWeight.EdgeWeightInteger) (e.weight)).i;
                        int newWeight = edgeWeight + vertexWeights.get(v);

                        if (newWeight > g.totalEdgeWeightSum) return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                        if (vertexWeights.containsKey(b)) {
                            int currDestWeight = vertexWeights.get(b);

                            if (immutableVertices.contains(b)) {
                                if (newWeight != vertexWeights.get(b)) {
                                    return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                                }
                            } else {
                                if (newWeight < currDestWeight) {
                                    forChangeBackward.add(b); // weight(b) > weight(v)+w, so we need to backwards-traverse from b to set this vertex properly
                                } else if (newWeight > currDestWeight) {
                                    vertexWeights.put(b, newWeight);
                                    vertexIncrementCount();
                                    forChangeBackward.add(b);
                                    stack.push(b);
                                } // else newWeight = currDestWeight, so don't do anything, we're not changing anything
                            }
                        } else {

                            vertexWeights.put(b, newWeight);
                            vertexIncrementCount();
                            forChangeBackward.add(b);
                            stack.push(b);
                        }
                    }

                }

            }
        }

        return null;
    }


    private Result backwardPropagate(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, Set<ComponentVertex> immutableVertices, ComponentVertex u, Set<ComponentVertex> forChangeForward) {

        /* And the equivalent backward propagation rules:
           If e=a->v is variable-weight
           - if weight(a) < weight(v): set weight(e)=weight(v)-weight(a)
                weight(a) = weight(v): set weight(e)=0
                weight(a) > weight(v): we'll need to restart forward propagation from a, so add a to forChangeForward
                weight(a) is undefined: can this even happen? don't think so. anyway, if so, leave it alone, we can't guarantee anything
                                        about it other than that weight(a) <= weight(v), and it's not assigned, so can't do anything.



           If e=a->v is fixed-weight w
           - if weight(a) < weight(v)-w:  set weight(a) = weight(v)-w, add a to forChangeForward, continue backwards traversal from a; fail if a is immutable-weight
                                          what if weight(v)-w < 0? can't be: weight(v) >= weight(a)+w by forward traversal, weight(a) >= 0 in forward traversal, so...
                weight(a) = weight(v)-w:  do nothing
                weight(a) > weight(v)-w:  shouldn't ever occur - a has a weight, v has a weight, both assigned during forward search; neither can decrease,
                                          hence forward search gives weight(v) >= weight(a)+w; so we can never get this
                weight(a) is undefined:   this shouldn't ever occur - a is either forwards-reachable or an original source, so it has a weight by this point.
                                          on the offchance, though, set weight(a) = weight(v)-w, add a to forChangeForward, continue backwards traversal from a


            NOTE: modification to make based on ex_1_5_3; if you've got (edges down)
                    V
                  2/ \
                  W  |2
                  1\ /
                    X
            then you need to detect the different-length paths immediately or get stuck in a cycle of continuous incrementing.
            So - when we set a previous vertex, ensure that it hasn't already been set to something different by this iteration -
            if it has, we have two paths of different length, so must fail. WRONG.

            We're going to take an easy way - the maximum variable value must by definition be <= the sum total of all edge weights in the original
            graph. Hence if we ever exceed such a value for a vertex weight, we've failed.

         */
        Stack<ComponentVertex> stack = new Stack<>(); // anything on the stack, or in progress, has a weight assigned
        Set<ComponentVertex> inProgress = new HashSet<>();
        stack.push(u);


        //System.out.println("Starting search from sink vertex " + u);

        while (!stack.empty()) {
            ComponentVertex v = stack.peek();

            if (inProgress.contains(v)) {
                inProgress.remove(v);
                stack.pop();
            } else {

                inProgress.add(v);
                for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                    ComponentVertex a = e.from;
                    if (e.isVariableWeight()) {
                        /*
                            If e=a->v is variable-weight
                            - if weight(a) < weight(v): set weight(e)=weight(v)-weight(a)
                                 weight(a) = weight(v): set weight(e)=0
                                 weight(a) > weight(v): we'll need to restart forward propagation from a, so add a to forChangeForward
                                 weight(a) is undefined: can this even happen? don't think so. anyway, if so, leave it alone, we can't guarantee anything
                                                         about it other than that weight(a) <= weight(v), and it's not assigned, so can't do anything.
                         */
                        Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;

                        if (vertexWeights.containsKey(a)) {
                            int weightA = vertexWeights.get(a);
                            int weightV = vertexWeights.get(v);

                            if (weightA <= weightV) {
                                edgeVariableWeights.put(eVar, weightV - weightA);
                            } else { // weight(a) > weight(v)

                                    forChangeForward.add(a);

                            }
                        } else {
                            throw new RuntimeException("THIS SHOULDN'T HAPPEN 3");
                        }
                    } else { // e is fixed-weight
                        /*
                        If e=a->v is fixed-weight w
                        - if weight(a) < weight(v)-w:  set weight(a) = weight(v)-w, add a to forChangeForward, continue backwards traversal from a; fail if a is immutable-weight
                                                       what if weight(v)-w < 0? can't be: weight(v) >= weight(a)+w by forward traversal, weight(a) >= 0 in forward traversal, so...
                             weight(a) = weight(v)-w:  do nothing
                             weight(a) > weight(v)-w:  shouldn't ever occur - a has a weight, v has a weight, both assigned during forward search; neither can decrease,
                                                       hence forward search gives weight(v) >= weight(a)+w; so we can never get this.
                                                       having said that, it's just happened. so let's say that when it happens, we need to run forward from a.
                             weight(a) is undefined:   this shouldn't ever occur - a is either forwards-reachable or an original source, so it has a weight by this point.
                                                       on the offchance, though, set weight(a) = weight(v)-w, add a to forChangeForward, continue backwards traversal from a
                         */
                        int w = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                        int weightDiff = vertexWeights.get(v) - w;

                        if (weightDiff < 0) throw new RuntimeException("THIS SHOULDN'T HAPPEN");

                        if (weightDiff > g.totalEdgeWeightSum) return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                        if (vertexWeights.containsKey(a)) {
                            int weightA = vertexWeights.get(a);

                            if (weightA < weightDiff) {
                                if (immutableVertices.contains(a)) return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                                    vertexWeights.put(a, weightDiff);
                                    vertexIncrementCount();
                                    forChangeForward.add(a);
                                    stack.push(a);


                            } else if (weightA > weightDiff) {
                                // should never occur
                                // throw new RuntimeException("THIS SHOULDN'T HAPPEN EITHER");

                                    forChangeForward.add(a);


                            } // else do nothing

                        } else {
                            vertexWeights.put(a, weightDiff);
                            vertexIncrementCount();
                            forChangeForward.add(a);
                            stack.push(a);
                            throw new RuntimeException("NOR SHOULD THIS");
                        }
                    }

                }

            }
        }

        return null;
    }


    // Second version.
    /*
        Hold a set of vertices of guaranteed weight V whose weight is guaranteed by some path of fixed-weight edges
        from the initial source vertices, and a set of all other vertices which will be of variable weight.

        Run the previous algorithm starting at V, but we let variable-weight vertices increase in weight.

        We need to distinguish edges of fixed weight from edges of variable weight with a weight assigned.
     */
    /*
    private Result checkPathLengths2(String fileName, boolean isDebug) {
        Map<ComponentVertex, Integer> vertexWeights = new HashMap<>();
        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        int finalVariableEdgeCount = g.edges.stream().filter(Edge::isVariableWeight).collect(Collectors.toList()).size();

        // Set up immutable vertices - vertices whose weight MUST equal a specific value
        Set<ComponentVertex> immutableVertices = new HashSet<>();

        setInitialSources(vertexWeights);

        Set<ComponentVertex> source = getSourceVertices(vertexWeights);

        for (ComponentVertex u : source) {
            propagateKnownWeight(vertexWeights, edgeVariableWeights, u);
        }

        immutableVertices.addAll(vertexWeights.keySet());


        printIntermediates2(isDebug, vertexWeights, fileName, edgeVariableWeights, 0, "immutables");


        // Mutable vertices are vertices whose weight is _at least_ the specific value, which can only _increase_.

        // Same idea-ish for termination, but we have to look at changes made - terminate on no more changes
        // rely on the condition of only permitting increases to cause termination
        // (should terminate when all vertices are assigned the sum of all fixed-weight edges, since no path is longer)


        // Taking a copy is a really inefficient way of showing termination, but it's easy(ish?) to fix later.
        Map<ComponentVertex, Integer> initialWeights = null;
        Map<ComponentVertex, Integer> endingWeights = new HashMap<>(vertexWeights);


        while (!endingWeights.equals(initialWeights)) {
            initialWeights = endingWeights;

            source = getSourceVertices(vertexWeights);

            for (ComponentVertex u : source) {
                Result x = propagatePossibleWeight(vertexWeights, edgeVariableWeights, immutableVertices, u);
                if (x != null) {
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
                    return x;
                }
            }

            // we don't actually need to do the other stuff now! (propagating to unweighted along incoming variable edges etc.)
            // if a vertex v has incoming variable edges u->v, as long as u has a weight,
            // it's been propagated to v and the biggest chosen.
            // So basically we just do this iteration until nothing further changes.


            endingWeights = new HashMap<>(vertexWeights);
        }

        // We still have to check we've reached termination with all vertices assigned a weight, though
        // Same as before:

        int finalVertexWeightCount = vertexWeights.keySet().size();
        int finalVertexCount = g.vertices.size();

        int finalEdgeWeightCount = edgeVariableWeights.keySet().size();


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


        //System.out.println("Terminating check: " + finalVertexWeightCount + "/" + finalVertexCount + " vertices, " + finalEdgeWeightCount + "/" + finalVariableEdgeCount + " variable edges.");
        // We need to check all vertices and edges have weights assigned! This should be guaranteed by termination, but just in case:
        if (finalVertexCount != finalVertexWeightCount || finalEdgeWeightCount != finalVariableEdgeCount) {
            return Result.FAILURE_UNASSIGNED_VERTEX_OR_EDGE_WEIGHTS;
        }

        // Set weights both in the SCC graph and in the original problem graph.


        return Result.SUCCESS;


    }

*/

    /*
    private Result propagatePossibleWeight(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, Set<ComponentVertex> immutableVertices, ComponentVertex u) {
        // Propagating a possible weight is slightly different - we can change previous vertices upwards to match as well, where possible.
        // We also have to adjust edge weights appropriately

        // Assign weights when adding vertices to the stack - so on first seeing the vertex
        Stack<ComponentVertex> stack = new Stack<>(); // anything on the stack, or in progress, has a weight assigned

        Set<ComponentVertex> inProgress = new HashSet<>();
        Set<ComponentVertex> onStack = new HashSet<>(); // constant-time lookup of stack contents

        stack.push(u);
        onStack.add(u);

        //System.out.println("Starting search from sink vertex " + u);

        while (!stack.empty()) {
            ComponentVertex v = stack.peek();

            //System.out.println("Stack top: " + v);

            if (inProgress.contains(v)) {
                //System.out.println(v + " completed. Removing from stack and in-progress.");
                inProgress.remove(v);
                stack.pop();
                onStack.remove(v);
            } else {
                //System.out.println(v + " newly-seen.");
                inProgress.add(v);
                // Traverse outgoing edges
                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    ComponentVertex to = e.to;
                    if (e.isVariableWeight()) {
                        // A forward edge of variable weight.
                        // Key question: is the destination vertex assigned a weight?
                        // If it is, we ensure that this edge has correct weight.
                        // If it isn't, THINK we can propagate, since the invariant is now that the vertex weight <= its final weight,
                        // so we SHOULD be able to propagate forward the current weight
                        Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;

                        if (vertexWeights.containsKey(to)) {
                            int weightDiff = vertexWeights.get(to) - vertexWeights.get(v);

                            if (weightDiff >= 0) {
                                edgeVariableWeights.put(eVar, weightDiff);
                            } else {
                                if (immutableVertices.contains(to)) {
                                    return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                                } else {
                                    edgeVariableWeights.put(eVar, 0);
                                    vertexWeights.put(to, vertexWeights.get(v));
                                    inProgress.remove(to); // we need to redo this variable because it is AT LEAST as big as this current one
                                    if (!onStack.contains(to)) {
                                        stack.push(to);
                                        onStack.add(to);
                                    }
                                }
                            }
                        } else {
                            // NOT SURE ABOUT THIS
                            edgeVariableWeights.put(eVar, 0);
                            vertexWeights.put(to, vertexWeights.get(v));
                            stack.push(to);
                            onStack.add(to);
                        }
                    } else { // is a fixed-weight forward edge, just propagate forwards, check for immutables and weight reduction
                        int edgeWeight = ((EdgeWeight.EdgeWeightInteger) (e.weight)).i;
                        int newWeight = edgeWeight + vertexWeights.get(v);


                        if (vertexWeights.containsKey(to)) {
                            int currDestWeight = vertexWeights.get(to);
                            // If the destination already has a weight
                            // Either: it's immutable, in which case check that it won't change
                            if (immutableVertices.contains(to)) {
                                if (newWeight != vertexWeights.get(to)) {
                                    return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                                }
                            } else {
                                // Or it's not immutable, in which case check that its new weight is non-decreasing
                                // and add it to reprocessing if its new weight is greater
                                if (newWeight < currDestWeight) {
                                    return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                                } else if (newWeight > currDestWeight) {
                                    vertexWeights.put(to, newWeight);
                                    inProgress.remove(to);
                                    if (!onStack.contains(to)) {
                                        onStack.add(to);
                                        stack.push(to);
                                    }
                                } // else newWeight = currDestWeight, so don't do anything, we're not changing anything
                            }
                        } else {
                            // if it doesn't have a weight, it's not immutable, and it can just be assigned a weight directly
                            // also, it's not in progress or on the stack, or it'd have some weight
                            vertexWeights.put(to, newWeight);
                            stack.push(to);
                            onStack.add(to);
                        }
                    }

                }


                // Traverse backwards along incoming edges
                for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                    //System.out.println("Incoming: edge " + e);
                    ComponentVertex from = e.from;

                    if (e.isVariableWeight()) {
                        // Variable-weight edge - we only do something here if the destination already has a weight;
                        // current being known >= from doesn't imply anything about from, other than that it can't be > current.
                        // And if it was, it would have been detected when handling that vertex. (I think.)
                        if (vertexWeights.containsKey(from)) {
                            Variable eVar = ((EdgeWeight.EdgeWeightVariable) e.weight).v;

                            if (vertexWeights.get(from) > vertexWeights.get(v)) {
                                // we can't alter this at all - from can't reduce, and if v could have increased it would have from from;
                                // I'm not even sure this can happen, tbh.
                                return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                            } else {
                                // otherwise, simply ensure that this edge is of correct weight.
                                // this is more of an 'update just in case' thing.
                                edgeVariableWeights.put(eVar, vertexWeights.get(v) - vertexWeights.get(from));
                            }

                        }
                    } else {
                        int edgeWeight = ((EdgeWeight.EdgeWeightInteger) e.weight).i;
                        int weightV = vertexWeights.get(v);
                        int newWeight = weightV - edgeWeight;
                        if (newWeight < 0) { // This is an impossible solution - it requires a time of < 0.
                            return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                        }

                        // Fixed-weight edge
                        // Fairly straightforward - propagate backwards, fail if we would reduce the previous vertex's weight
                        if (vertexWeights.containsKey(from)) { // if it has a weight, increase it if needed, fail if would decrease
                            int weightFrom = vertexWeights.get(from);
                            if (weightFrom > newWeight) {
                                return Result.FAILURE_DIFFERENT_PATH_LENGTHS;
                            } else if (newWeight > weightFrom) {
                                // we can increase the weight of the previous vertex to match the new weight
                                vertexWeights.put(from, newWeight);

                                // and also add it to the vertices to process
                                inProgress.remove(from);
                                if (!onStack.contains(from)) {
                                    stack.push(from);
                                    onStack.add(from);
                                }

                            } // else equal, so do nothing

                        } else {
                            // no weight, so just assign one from here (as long as >= 0), add vertex to process
                            vertexWeights.put(from, newWeight);
                            stack.push(from);
                            onStack.add(from);
                        }
                    }

                }
            }
        }
        return null;
    }

*/
    /*
    private Result checkPathLengths(String fileName, boolean isDebug) {
        /*
        Map<ComponentVertex, Integer> vertexWeights = new HashMap<>();
        Map<Variable, Integer> edgeVariableWeights = new HashMap<>();

        // We want to iterate until either
        // a) weights are assigned to every vertex
        // b) no changes made by an iteration - algorithm stuck, terminate

        // We need all SCC source vertices initialised to a weight 0
        setInitialSources(vertexWeights);

        int iterationCount = 0; // for generation of intermediate .dotgraph files
        int vertexCount = g.vertices.size();
        int previouslyAssignedWeights = -1; // -1 to prevent immediate termination!

        // we need to assign this here instead of at the end - all the edges end up not as variables!
        int finalVariableEdgeCount = g.edges.stream().filter(Edge::isVariableWeight).collect(Collectors.toList()).size();

        while (previouslyAssignedWeights < vertexCount) {
            iterationCount++;


            // Get all "source" vertices, defined here as vertices with a weight assigned and
            // at least one outgoing edge to a vertex with unknown weight
            Set<ComponentVertex> sources = getSourceVertices(vertexWeights);

            // Now we have to set all weights definitely settable -
            // the vertices that *have* to be of a certain weight
            // So those vertices adjacent from a weighted vertex via a weighted edge (both forwards and backwards)
            // and those vertices with a SINGLE incoming variable edge from a weighted vertex
            // (might be able to do the latter in reverse too? not sure if there's any point)
            // We fail if we reach a contradiction - a vertex that must, by two different traversals, be two different weights.
            // We can also set edge weights if needed here - if we have A[5] and B[6], and A -x-> B, we know x=1.
            // BUT we also have to fail if a negative weight is required - this is implying that both A > B and A <= B.


            // We start from each sink vertex, DFS-with-reversed-edges, to find the vertex weights REQUIRED
            for (ComponentVertex u : sources) {
                Result x = propagateKnownWeight(vertexWeights, edgeVariableWeights, u);
                if (x != null) return x;
            }

            // For now, we'll print out here, assign the given weights to the vertices in the component graph
            // and print out the graph


            printIntermediates(isDebug, vertexWeights, fileName, edgeVariableWeights, iterationCount, 1);


            // Now we iterate over writing weights to non-weighted vertices with precisely one incoming variable edge from a weighted vertex until none such remain

            // Set up initial set of writable vertices
            Stack<ComponentVertex> singleIncomingVariableEdgeValid = new Stack<>(); // 'valid' vertices are those which we can write to immediately
            Set<ComponentVertex> singleIncomingVariableEdgeInvalid = new HashSet<>(); // 'invalid' vertices are those which we cannot write to yet, but only have a single incoming edge

            for (Map.Entry<ComponentVertex, List<Edge<ComponentVertex>>> entry : g.edgesIncoming.entrySet()) {
                ComponentVertex v = entry.getKey();
                List<Edge<ComponentVertex>> incomings = entry.getValue();

                if (!vertexWeights.containsKey(v) && incomings.size() == 1 && incomings.get(0).isVariableWeight()) {
                    // must be assignable - has no weight, and only one incoming variable edge (or it would have been assigned)
                    // but we need to ensure the edge's source has a weight
                    if (vertexWeights.containsKey(incomings.get(0).from)) {
                        singleIncomingVariableEdgeValid.push(v);
                    } else {
                        singleIncomingVariableEdgeInvalid.add(v);
                    }
                }
            }

            //System.out.println("Initial set of singleIncomingEdgeValid: " + singleIncomingVariableEdgeValid);
            //System.out.println("Initial set of singleIncomingEdgeInvalid: " + singleIncomingVariableEdgeInvalid);

            // Now iterate over these vertices, adding new such vertices as conditions change
            while (!singleIncomingVariableEdgeValid.empty()) {
                ComponentVertex v = singleIncomingVariableEdgeValid.pop();

                //System.out.println("Handling vertex " + v);

                Edge<ComponentVertex> incoming = g.edgesIncoming.get(v).get(0); // must be single!
                ComponentVertex u = incoming.from;

                //System.out.println("Assign a weight of " + vertexWeights.get(u) + " to " + v + " from " + u);
                vertexWeights.put(v, vertexWeights.get(u));

                //System.out.println("Propagating weight");
                Result x = propagateKnownWeight(vertexWeights, edgeVariableWeights, v);
                if (x != null) return x;

                // Once we've done propagating, we need to check if any vertices which had only a single edge from an
                // unassigned vertex are now valid for propagating
                // This is a bit inefficient - we could probably reduce this
                Set<ComponentVertex> readyToUpdate = new HashSet<>();
                for (ComponentVertex toCheck : singleIncomingVariableEdgeInvalid) {
                    Edge<ComponentVertex> e = g.edgesIncoming.get(toCheck).get(0); // again, only 1 in there
                    if (vertexWeights.containsKey(e.from)) {
                        readyToUpdate.add(toCheck);
                    }
                }

                singleIncomingVariableEdgeInvalid.removeAll(readyToUpdate);
                singleIncomingVariableEdgeValid.addAll(readyToUpdate);

                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    if (!vertexWeights.containsKey(e.to) && g.edgesIncoming.get(e.to).size() == 1) {
                        //System.out.println("Updating singleIncomingEdge with " + e.to);
                        singleIncomingVariableEdgeValid.push(e.to);
                    }
                }
            }


            // Write again
            printIntermediates(isDebug, vertexWeights, fileName, edgeVariableWeights, iterationCount, 2);



            // Next step is to add in those vertices that have multiple incoming variable edges from weighted vertices.
            // NOTE: no fixed edges!! anyway, if those edges were fixed and the variable was weighted, it'd already
            // have been propagated by the earlier stuff.
            // Same idea as before - a set of vertices which can immediately be set (i.e. all variable edges to that vertex, all from weighted vertices),
            // and a set of vertices which might be settable once the first are done (i.e. all variable edges to that vertex, but some from non-weighted vertices)
            Stack<ComponentVertex> multiVariableEdgeVerticesToSet = new Stack<>();
            Set<ComponentVertex> multiVariableEdgeVerticesAwaiting = new HashSet<>();
            for (Map.Entry<ComponentVertex, List<Edge<ComponentVertex>>> entry : g.edgesIncoming.entrySet()) {
                if (!vertexWeights.containsKey(entry.getKey()) && entry.getValue().size() > 1 && entry.getValue().stream().allMatch(Edge::isVariableWeight)) {
                    if (entry.getValue().stream().map(edge -> edge.from).allMatch(vertexWeights::containsKey)) {
                        multiVariableEdgeVerticesToSet.add(entry.getKey());
                    } else {
                        multiVariableEdgeVerticesAwaiting.add(entry.getKey());
                    }
                }
            }

            while (!multiVariableEdgeVerticesToSet.isEmpty()) {
                ComponentVertex v = multiVariableEdgeVerticesToSet.pop();

                int maxWeight = g.edgesIncoming.get(v).stream().map(edge -> vertexWeights.get(edge.from)).max(Integer::compareTo).get(); // we know it has at least one incoming edge from a weighted vertex, so fine

                vertexWeights.put(v, maxWeight);

                g.edgesIncoming.get(v).forEach(edge -> {
                    Variable variable = ((EdgeWeight.EdgeWeightVariable) edge.weight).v;

                    int diff = maxWeight - vertexWeights.get(edge.from);

                    edgeVariableWeights.put(variable, diff);
                });

                Result x = propagateKnownWeight(vertexWeights, edgeVariableWeights, v);
                if (x != null) return x;


                Set<ComponentVertex> readyToDo = new HashSet<>();
                for (ComponentVertex v2 : multiVariableEdgeVerticesAwaiting) {
                    if (g.edgesIncoming.get(v2).stream().map(e -> e.from).allMatch(vertexWeights::containsKey)) {
                        readyToDo.add(v2);
                    }
                }

                multiVariableEdgeVerticesAwaiting.removeAll(readyToDo);
                multiVariableEdgeVerticesToSet.addAll(readyToDo);

            }


            // Write again
            printIntermediates(isDebug, vertexWeights, fileName, edgeVariableWeights, iterationCount, 3);



            // Ensure some progress has been made, and fail if not; continue loop if so.
            int currentlyAssignedWeights = vertexWeights.keySet().size();
            if (currentlyAssignedWeights == previouslyAssignedWeights) {
                return Result.FAILURE_NO_PROGRESS;
            }
            previouslyAssignedWeights = currentlyAssignedWeights;


        }

        int finalVertexWeightCount = vertexWeights.keySet().size();
        int finalVertexCount = g.vertices.size();

        int finalEdgeWeightCount = edgeVariableWeights.keySet().size();


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


        //System.out.println("Terminating check: " + finalVertexWeightCount + "/" + finalVertexCount + " vertices, " + finalEdgeWeightCount + "/" + finalVariableEdgeCount + " variable edges.");
        // We need to check all vertices and edges have weights assigned! This should be guaranteed by termination, but just in case:
        if (finalVertexCount != finalVertexWeightCount || finalEdgeWeightCount != finalVariableEdgeCount) {
            return Result.FAILURE_UNASSIGNED_VERTEX_OR_EDGE_WEIGHTS;
        }

        // Set weights both in the SCC graph and in the original problem graph.


        return Result.SUCCESS;
        throw new RuntimeException("SHOULDN'T BE CALLING THIS");
    }

*/


/*
    private void printIntermediates(boolean isDebug, Map<ComponentVertex, Integer> vertexWeights, String fileName, Map<Variable, Integer> edgeVariableWeights, int iterationCount, int intermediateCount) {
        if (!isDebug) return;

        vertexWeights.forEach((v, i) -> v.weight = i);

        g.edges.forEach(edge -> edge.setWeightFromMap(edgeVariableWeights));


        g.writeGraphVizUnstyled(fileName + "_4_iter_" + iterationCount + "_intermediate_weights_" + intermediateCount + ".dotgraph");
    }
*/

    private Result propagateKnownWeight(Map<ComponentVertex, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights, ComponentVertex u) {
        // Assign weights when adding vertices to the stack - so on first seeing the vertex
        Stack<ComponentVertex> stack = new Stack<>(); // anything on the stack, or in progress, has a weight assigned
        Set<ComponentVertex> inProgress = new HashSet<>();

        stack.push(u);

        //System.out.println("Starting search from sink vertex " + u);

        while (!stack.empty()) {
            ComponentVertex v = stack.peek();
            //System.out.println("Stack top: " + v);

            if (inProgress.contains(v)) {
                //System.out.println(v + " completed. Removing from stack and in-progress.");
                inProgress.remove(v);
                stack.pop();
            } else {
                //System.out.println(v + " newly-seen.");
                inProgress.add(v);
                // Traverse outgoing edges
                for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                    //System.out.println("Outoing: edge " + e);
                    /*
                    if (e.isVariableWeight() && vertexWeights.containsKey(e.to) && !edgeVariableWeights.containsKey(((EdgeWeight.EdgeWeightVariable) e.weight).v)) {
                        // e is a variable, this vertex has a weight, the destination has a weight, so we can set
                        // the variable weight, and check for contradictions
                        int newWeight = vertexWeights.get(e.to) - vertexWeights.get(v);
                        if (newWeight < 0) {
                            //System.out.println("WHOOPS 3: " + newWeight);
                            return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                        } else {
                            edgeVariableWeights.put(((EdgeWeight.EdgeWeightVariable) e.weight).v, newWeight);
                        }
                    } else
                    */
                    if (!e.isVariableWeight()) {
                        // e is fixed, and this vertex has a weight, so we can set the destination directly
                        // and check for contradictions
                        int newDestWeight = vertexWeights.get(v) + ((EdgeWeight.EdgeWeightInteger) e.weight).i;

                        if (vertexWeights.containsKey(e.to) && newDestWeight != vertexWeights.get(e.to))
                            return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                        if (!vertexWeights.containsKey(e.to)) {
                            vertexWeights.put(e.to, newDestWeight);
                            stack.push(e.to);
                        }
                    }

                }

                /*// Traverse backwards along incoming edges
                for (Edge<ComponentVertex> e : g.edgesIncoming.get(v)) {
                    //System.out.println("Incoming: edge " + e);
                    if (e.isVariableWeight() && vertexWeights.containsKey(e.from) && !edgeVariableWeights.containsKey(((EdgeWeight.EdgeWeightVariable) e.weight).v)) {
                        // e is a variable, this destination vertex has a weight, the source has a weight, so we can set
                        // the variable weight
                        // Note: I don't think this will ever get used!
                        int newWeight = vertexWeights.get(v) - vertexWeights.get(e.from);
                        if (newWeight < 0) {
                            //System.out.println("WHOOPS 2: " + newWeight);
                            return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                        } else {
                            edgeVariableWeights.put(((EdgeWeight.EdgeWeightVariable) e.weight).v, newWeight);
                        }
                    } else if (!e.isVariableWeight()) {
                        // e is fixed, and this vertex has a weight, so we can set the destination directly
                        int newDestWeight = vertexWeights.get(v) - ((EdgeWeight.EdgeWeightInteger) e.weight).i;

                        if (newDestWeight < 0) {
                            //System.out.println("WHOOPS 1: " + newDestWeight);
                            return Result.FAILURE_IMPOSSIBLE_EDGE_WEIGHT;
                        }
                        if (vertexWeights.containsKey(e.from) && newDestWeight != vertexWeights.get(e.from))
                            return Result.FAILURE_DIFFERENT_PATH_LENGTHS;

                        if (!vertexWeights.containsKey(e.from)) {
                            vertexWeights.put(e.from, newDestWeight);
                            stack.push(e.from);
                        }
                    }
                }*/
            }

        }

        return null;
    }

    private Set<ComponentVertex> getSourceVertices(Map<ComponentVertex, Integer> vertexWeights) {
        // A source vertex here is a vertex assigned a weight with at least one outgoing edge to a vertex of unknown weight
        // serving as a starting point for propagation
        Set<ComponentVertex> sourceVertices = new HashSet<>();
        for (ComponentVertex v : vertexWeights.keySet()) {
            for (Edge<ComponentVertex> e : g.edgesOutgoing.get(v)) {
                if (!vertexWeights.containsKey(e.to)) sourceVertices.add(v);
            }
        }
        return sourceVertices;
    }

    private void setInitialSources(Map<ComponentVertex, Integer> vertexWeights) {
        for (Map.Entry<ComponentVertex, List<Edge<ComponentVertex>>> entry : g.edgesIncoming.entrySet()) {
            if (entry.getValue().size() == 0) {
                vertexWeights.put(entry.getKey(), 0);
            }
        }
    }

}
