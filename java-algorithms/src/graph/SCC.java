package graph;

import java.util.*;

public class SCC {
    // Taken from CLRS 3rd ed., p. 617
    public ComponentGraph runSCCWithReversedEdges(Graph<Vertex> g) {
        //Map<Vertex, Integer> finishingTimes = computeFinishingTimes(g);
        Map<Vertex, Integer> finishingTimes = computeFinishingTimes_recursive(g);
        Graph<Vertex> gRev = g.reverse();

//        System.out.println("Finishing times:");
//        finishingTimes.forEach((k,v) -> System.out.println(k + ": " + v));
//        System.out.println();

        List<Vertex> inFinishingTimeOrder = new ArrayList<>(g.vertices);

        inFinishingTimeOrder.sort(Comparator.comparingInt(finishingTimes::get).reversed());

//        System.out.println("Finishing time order: " + inFinishingTimeOrder);

        Map<Vertex, Integer> componentIndices = computeComponents(inFinishingTimeOrder, gRev);

//        System.out.println("Component indices:");
//        componentIndices.forEach((k,v) -> System.out.println(k + ":" + v));

        ComponentGraph cg = new ComponentGraph(g.totalEdgeWeightSum);


        // We use gRev because we want the edges in the component graph to be backwards
        cg.initialise(componentIndices, gRev.edges);

        return cg;
    }

    public ComponentGraph runSCC(Graph<Vertex> g) {
        Map<Vertex, Integer> finishingTimes = computeFinishingTimes_recursive(g);
        Graph<Vertex> gRev = g.reverse();


        List<Vertex> inFinishingTimeOrder = new ArrayList<>(g.vertices);

        inFinishingTimeOrder.sort(Comparator.comparingInt(finishingTimes::get).reversed());


        Map<Vertex, Integer> componentIndices = computeComponents(inFinishingTimeOrder, gRev);

        ComponentGraph cg = new ComponentGraph(g.totalEdgeWeightSum);

        cg.initialise(componentIndices, g.edges);

        return cg;
    }

    private Map<Vertex,Integer> computeFinishingTimes_recursive(Graph<Vertex> g) {
        Map<Vertex, Integer> finishingTimes = new HashMap<>();
        Set<Vertex> inProgress = new HashSet<>();

        int finishingTime = 0;

        for (Vertex u : g.vertices) {
            if (!finishingTimes.containsKey(u)) {
                finishingTime = finishingTimeForVertex(g, u, finishingTime, inProgress, finishingTimes);
            }
        }

        return finishingTimes;
    }

    private int finishingTimeForVertex(Graph<Vertex> g, Vertex v, int oldFinishingTime, Set<Vertex> inProgress, Map<Vertex, Integer> finishingTimes) {
        int updatedFinishingTime = oldFinishingTime;
        inProgress.add(v);
        for (Edge<Vertex> e : g.edgesOutgoing.get(v)) {
            if (!finishingTimes.containsKey(e.to) && !inProgress.contains(e.to)) {
                updatedFinishingTime = finishingTimeForVertex(g, e.to, updatedFinishingTime, inProgress, finishingTimes);
            }
        }

        inProgress.remove(v);
        finishingTimes.put(v, updatedFinishingTime + 1);
        return updatedFinishingTime + 1;
    }

    private Map<Vertex, Integer> computeFinishingTimes(Graph<Vertex> g) {
        Map<Vertex, Integer> finishingTimes = new HashMap<>(); // once a vertex is done, it has a finishing time

        Stack<Vertex> stack = new Stack<>();

        Set<Vertex> inProgress = new HashSet<>(); // constant-time check to see if the vertex is in the stack

        Set<Vertex> onStack = new HashSet<>();

        List<Vertex> allVertices = g.vertices;

        int finishingTime = 0;

        for (Vertex u : allVertices) {
            if (!finishingTimes.containsKey(u)) {
                //System.out.println("Starting iteration from " + u);
                stack.push(u);
                onStack.add(u);

                while (!stack.empty()) {
                    Vertex v = stack.peek();
                    //System.out.println("Processing " + v);

                    if (inProgress.contains(v)) {
                        //System.out.println("\t" + v + " in progress");
                        inProgress.remove(v);

                        if (!finishingTimes.containsKey(v)) {
                            finishingTimes.put(v, finishingTime);
                            //System.out.println("\tAssigning finishing time " + finishingTime + " to " + v);
                        }

                        finishingTime++;
                        stack.pop();
                    }
                    else {
                        //System.out.println("\t" + v + " not in progress");
                        inProgress.add(v);
                        for (Edge e : g.edgesOutgoing.get(v)) {
                            if (!finishingTimes.containsKey(e.to)) {// && !inProgress.contains(e.to) && !onStack.contains(e.to)) {
                                //System.out.println("\tAdding " + e.to + " to the stack");
                                stack.push(e.to);
                                onStack.add(e.to);
                            }
                        }
                    }
                }
            }
        }

        return finishingTimes;
    }

    private Map<Vertex, Integer> computeComponents(List<Vertex> inFinishingTimeOrder, Graph<Vertex> gRev) {
        Map<Vertex, Integer> componentIndices = new HashMap<>();

        Stack<Vertex> stack = new Stack<>();

        Set<Vertex> inProgress = new HashSet<>(); // constant-time check to see if the vertex is in the stack

        int component = 0;

        for (Vertex u : inFinishingTimeOrder) {
            if (!componentIndices.containsKey(u)) {
                stack.add(u);

                while (!stack.empty()) {
                    Vertex v = stack.peek();

                    if (inProgress.contains(v)) {
                        inProgress.remove(v);

                        //System.out.println("\tSetting component index " + v + ":" + component);
                        componentIndices.put(v, component);

                        stack.pop();
                    }
                    else {
                        inProgress.add(v);
//                        System.out.println("\tSetting component index " + v + ":" + component);
//                        componentIndices.put(v, component);*/
                        for (Edge e : gRev.edgesOutgoing.get(v)) {
                            if (!componentIndices.containsKey(e.to) && !inProgress.contains(e.to)) {
                                stack.push(e.to);
                            }
                        }
                    }
                }

                component++;
            }


        }

        return componentIndices;
    }


}
