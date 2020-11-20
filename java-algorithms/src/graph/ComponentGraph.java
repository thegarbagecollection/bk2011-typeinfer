package graph;

import java.util.*;
import java.util.stream.Collectors;

public class ComponentGraph extends Graph<ComponentVertex> {

    Map<ComponentVertex, Set<Vertex>> componentToContents;

    Map<Vertex, ComponentVertex> contentToComponent;

    Map<ComponentVertex, Set<Edge<Vertex>>> selfLoops;

    Map<Edge<ComponentVertex>, Set<Edge<Vertex>>> componentEdgeToOriginals;


    Variable.VariableFactory variableFactory;

    public ComponentGraph(int totalEdgeWeightSumFromOriginal) {
        super(GraphComponentFactory.COMPONENT);
        componentToContents = new HashMap<>();
        contentToComponent = new HashMap<>();
        selfLoops = new HashMap<>();
        componentEdgeToOriginals = new HashMap<>();
        variableFactory = Variable.newFactory();
        totalEdgeWeightSum = totalEdgeWeightSumFromOriginal;
    }

    public void initialise(Map<Vertex, Integer> componentMapping, List<Edge<Vertex>> originalEdges) {
        Map<Integer, Set<Vertex>> componentIndicesWithContents = new HashMap<>();

        componentMapping.forEach((vertex, index) -> {
            if (!componentIndicesWithContents.containsKey(index)) componentIndicesWithContents.put(index, new HashSet<>());
            componentIndicesWithContents.get(index).add(vertex);
        });

        componentIndicesWithContents.forEach((index, vertexSet) -> {
            addOrGetCurrentComponentVertex(vertexSet);
        });

        addOriginalEdges(originalEdges);
    }

    private ComponentVertex addOrGetCurrentComponentVertex(Set<Vertex> componentContents) {
        String label = ComponentVertex.label(componentContents);
        ComponentVertex v = super.addOrGetCurrentVertex(label);
        if (v.componentContents == null) {
            v.setComponentContents(componentContents);
            componentToContents.put(v, componentContents);
            componentContents.forEach(u -> contentToComponent.put(u, v));
        }
        return v;
    }

    Edge<ComponentVertex> addEdgeWithEdgeWeight(ComponentVertex from, ComponentVertex to, EdgeWeight weight) {
        Edge<ComponentVertex> e = new Edge<>(from, to, weight);
        addEdge(e);
        return e;
    }

    private void addOriginalEdges(List<Edge<Vertex>> originalEdges) {
        for (Edge<Vertex> e : originalEdges) {
            ComponentVertex from = contentToComponent.get(e.from);
            ComponentVertex to = contentToComponent.get(e.to);

            if (from.equals(to)) {
                if (!selfLoops.containsKey(from)) selfLoops.put(from, new HashSet<>());
                selfLoops.get(from).add(e);
            }
            else {
                Edge<ComponentVertex> e2 = addEdgeWithEdgeWeight(from, to, e.copyWeight(variableFactory));

                if (!componentEdgeToOriginals.containsKey(e2)) componentEdgeToOriginals.put(e2, new HashSet<>());
                componentEdgeToOriginals.get(e2).add(e);
            }
        }
    }



    boolean reduceMultiEdges() {
        for (ComponentVertex v : vertices) {

            Map<ComponentVertex, Set<Edge<ComponentVertex>>> groupEdgesByDestination = new HashMap<>();

            for (Edge<ComponentVertex> e : edgesOutgoing.get(v)) {
                if (!groupEdgesByDestination.containsKey(e.to)) groupEdgesByDestination.put(e.to,new HashSet<>());
                groupEdgesByDestination.get(e.to).add(e);
            }

            for (Map.Entry<ComponentVertex, Set<Edge<ComponentVertex>>> entries : groupEdgesByDestination.entrySet()) {
                ComponentVertex dest = entries.getKey();
                Set<Edge<ComponentVertex>> destEdges = entries.getValue();

                if (destEdges.size() == 1) continue;

                Set<Edge<ComponentVertex>> variableEdges = new HashSet<>();
                Set<Edge<ComponentVertex>> fixedEdges = new HashSet<>();
                for (Edge<ComponentVertex> e : destEdges) {
                    e.assignToSet(fixedEdges, variableEdges);
                }

                // bit ugly, but should be ok
                if (!checkFixedEdges(fixedEdges.stream().map(o -> (EdgeWeight.EdgeWeightInteger) o.weight).collect(Collectors.toSet())))
                    return false;

                // here, we know that either we have no fixed-weight edges, or that all fixed-weight edges have equal weight
                // 1) No fixed-weight edges at all: if any variable-weight edges, replace with a single variable-weight edge, ensuring original edge variables are matched up correctly
                // 2) Fixed-weight edges and no variable-weight edges: replace with a single fixed-weight edge of same weight
                // 3) Fixed-weight edges and variable-weight edges: replace all with single fixed-weight edge of same weight

                int variableEdgesSize = variableEdges.size(); // save this before mutating!! shouldn't cause any problems, but just in case...
                int fixedEdgesSize = fixedEdges.size();

                //System.out.println("Variable edges from vertex " + v + " to vertex " + dest + ": " + variableEdges);
                //System.out.println("Fixed edges from vertex " + v + " to vertex " + dest + ": " + fixedEdges);

                if (fixedEdgesSize == 0 && variableEdgesSize > 0) {

                    List<Edge<Vertex>> originals = removeEdgesAndGetOriginalAssociatedEdges(variableEdges);

                    EdgeWeight ew = originals.get(0).weight.copy(variableFactory);
                    Edge<ComponentVertex> e = addEdgeWithEdgeWeight(v, dest, ew);
                    componentEdgeToOriginals.put(e, new HashSet<>(originals));


                } else if (fixedEdgesSize > 0 && variableEdgesSize == 0) {
                    List<Edge<Vertex>> originals = removeEdgesAndGetOriginalAssociatedEdges(fixedEdges);
                    EdgeWeight ew = originals.get(0).weight.copy(variableFactory);
                    Edge<ComponentVertex> e = addEdgeWithEdgeWeight(v, dest, ew);
                    componentEdgeToOriginals.put(e, new HashSet<>(originals));

                } else if (fixedEdgesSize > 0 && variableEdgesSize > 0) {

                    List<Edge<Vertex>> originals = removeEdgesAndGetOriginalAssociatedEdges(fixedEdges);
                    EdgeWeight ew = originals.get(0).weight.copy(variableFactory);

                    originals.addAll(removeEdgesAndGetOriginalAssociatedEdges(variableEdges));

                    Edge<ComponentVertex> e = addEdgeWithEdgeWeight(v, dest, ew);
                    componentEdgeToOriginals.put(e, new HashSet<>(originals));

                }
                // if there aren't any edges outgoing from this vertex, we don't need to do anything anyway
            }
        }

        //System.out.println(componentEdgeToOriginals);
        //System.out.println(componentEdgeToOriginals.keySet().size());
        return true;
    }

    List<Edge<Vertex>> removeEdgesAndGetOriginalAssociatedEdges(Set<Edge<ComponentVertex>> variableEdges) {
        List<Edge<Vertex>> allOriginals = new ArrayList<>(); // we want to sample 1 to get the copy of the variable
        variableEdges.forEach(edge -> allOriginals.addAll(componentEdgeToOriginals.get(edge)));
        removeEdges(variableEdges);
        return allOriginals;
    }



    void removeEdges(Set<Edge<ComponentVertex>> edgesToRemove) {
        edges.removeAll(edgesToRemove);
        for (Edge<ComponentVertex> e : edgesToRemove) {
            edgesOutgoing.get(e.from).remove(e);
            edgesIncoming.get(e.to).remove(e);
            componentEdgeToOriginals.put(e, new HashSet<>());
        }
    }

    private boolean checkFixedEdges(Set<EdgeWeight.EdgeWeightInteger> fixedEdges) {
        Integer currWeight = null;
        for (EdgeWeight.EdgeWeightInteger weight : fixedEdges) {
            if (currWeight == null) {
                currWeight = weight.i;
            }
            else if (currWeight != weight.i) {
                return false;
            }
        }
        return true;
    }
}
