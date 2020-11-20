package graph;

import timeconstraints.TimeConstraintSet;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class Graph<V extends Vertex> {
    List<Edge<V>> edges;

    public List<V> vertices;

    Map<V, List<Edge<V>>> edgesOutgoing;

    Map<V, List<Edge<V>>> edgesIncoming;

    Map<String, V> vertexByLabel;

    GraphComponentFactory<V> componentFactory;


    int totalEdgeWeightSum; // for termination detection



    public Graph(GraphComponentFactory<V> factory) {
        edges = new ArrayList<>();
        vertices = new ArrayList<>();
        edgesOutgoing = new HashMap<>();
        edgesIncoming = new HashMap<>();
        vertexByLabel = new HashMap<>();
        componentFactory = factory;
        this.totalEdgeWeightSum = -1;
    }


    private Graph(GraphComponentFactory<V> factory, List<Edge<V>> edges, List<V> vertices, Map<V, List<Edge<V>>> edgesOutgoing, Map<V, List<Edge<V>>> edgesIncoming, Map<String, V> vertexByLabel, int totalEdgeWeightSum) {
        this.edges = edges;
        this.vertices = vertices;
        this.edgesOutgoing = edgesOutgoing;
        this.edgesIncoming = edgesIncoming;
        this.vertexByLabel = vertexByLabel;
        this.componentFactory = factory;
        this.totalEdgeWeightSum = totalEdgeWeightSum;
    }

    public Edge<V> addEdgeVariableWeight(V from, V to, Variable.VariableFactory variableFactory) {
        // We label an edge using the name of its "from" vertex
        String label = "e_" + from.label + "_";
        Variable v = variableFactory.newVariable(label);
        Edge<V> e = componentFactory.createEdge(from, to, EdgeWeight.createVariable(v));
        addEdge(e);
        return e;
    }

    public Edge<V> addEdgeFixedWeight(V from, V to, int weight) {
        Edge<V> e = componentFactory.createEdge(from, to, EdgeWeight.createFixed(weight));
        addEdge(e);
        return e;
    }


    void addEdge(Edge<V> e) {
        assert vertices.contains(e.from) && vertices.contains(e.to);
        edges.add(e);

        if (!edgesIncoming.containsKey(e.to)) edgesIncoming.put(e.to, new ArrayList<>());
        if (!edgesOutgoing.containsKey(e.from)) edgesOutgoing.put(e.from, new ArrayList<>());

        edgesIncoming.get(e.to).add(e);
        edgesOutgoing.get(e.from).add(e);
    }


    public V addOrGetCurrentVertex(String label) {
        if (vertexByLabel.containsKey(label)) return vertexByLabel.get(label);
        return addVertex(label);
    }

    public V getVertexIfExists(String label) {
        return vertexByLabel.get(label);
    }

    private V addVertex(String label) {
        V v = componentFactory.createVertex(label);
        vertexByLabel.put(label, v);
        vertices.add(v);
        if (!edgesOutgoing.containsKey(v)) edgesOutgoing.put(v, new ArrayList<>());
        if (!edgesIncoming.containsKey(v)) edgesIncoming.put(v, new ArrayList<>());
        return v;
    }

    public Graph<V> reverse() {
        List<Edge<V>> revEdges = new ArrayList<>();

        // need this to have the reversed-edges still line up with the originals, weight-wise
        edges.forEach(edge -> revEdges.add(componentFactory.createEdgeWithPreviousReference(edge.to, edge.from, edge.weight, edge)));


        List<V> revVs = new ArrayList<>(vertices);
        Map<V, List<Edge<V>>> revEdgesOutgoing = new HashMap<>();
        Map<V, List<Edge<V>>> revEdgesIncoming = new HashMap<>();

        vertices.forEach(vertex -> {
            revEdgesOutgoing.put(vertex, new ArrayList<>());
            revEdgesIncoming.put(vertex, new ArrayList<>());
        });

        revEdges.forEach(edge -> {
            revEdgesOutgoing.get(edge.from).add(edge);
            revEdgesIncoming.get(edge.to).add(edge);
        });


        return new Graph<>(componentFactory, revEdges, revVs, revEdgesOutgoing, revEdgesIncoming, vertexByLabel, totalEdgeWeightSum);
    }




    public void writeGraphVizUnstyled(String fileName) {
        List<String> lines = new ArrayList<>();

        lines.add("digraph {");

        vertices.forEach(vertex -> {
            lines.add(vertex.labelToGraphViz() + ";");
        });


        edges.forEach(edge -> {
            lines.add(edge.toGraphViz() + ";");
        });

        lines.add("}");


        try {

            PrintWriter pw = new PrintWriter(new FileWriter(fileName));
            lines.forEach(pw::println);
            pw.flush();


        } catch (IOException e) {
            System.out.println("FAILED: exception.");
            System.out.println(e);
        }

    }


    public void writeGraphVizUnstyledWithWeights(String fileName, Map<V, Integer> vertexWeights, Map<Variable, Integer> edgeVariableWeights) {
        List<String> lines = new ArrayList<>();

        lines.add("digraph {");

        vertices.forEach(vertex -> {
            lines.add(vertex.labelToGraphVizWithTempWeight(vertexWeights) + ";");
        });


        edges.forEach(edge -> {
            lines.add(edge.toGraphVizWithTempWeight(vertexWeights, edgeVariableWeights) + ";");
        });

        lines.add("}");


        try {
            PrintWriter pw = new PrintWriter(new FileWriter(fileName));
            lines.forEach(pw::println);
            pw.flush();
        } catch (IOException e) {
            System.out.println("FAILED: exception.");
            System.out.println(e);
        }
    }


    public void writeGraphVizStyledWithWeights(String fileName,
                                               Map<String, Map<? extends Vertex, ? extends Object>> vertexWeights,
                                               Map<Variable, Integer> primaryEdgeVariableWeights,
                                               Map<String, Map<Variable, ? extends Object>> auxiliaryEdgeVariableWeights,
                                               Map<String, Map<Edge<? extends Vertex>, ? extends Object>> otherEdgeWeights,
                                               Map<? extends Vertex, Map<String, String>> vertexStyling,
                                               Map<Edge<? extends Vertex>, Map<String, String>> edgeStyling) {
        List<String> lines = new ArrayList<>();

        lines.add("digraph {");

        vertices.forEach(vertex -> {
            lines.add(vertex.labelToGraphVizWithTempWeightStyled(vertexWeights, vertexStyling) + ";");
        });


        edges.forEach(edge -> {
            lines.add(edge.toGraphVizWithTempWeightStyled(vertexWeights, primaryEdgeVariableWeights, auxiliaryEdgeVariableWeights, otherEdgeWeights, edgeStyling) + ";");
        });

        lines.add("}");


        try {
            PrintWriter pw = new PrintWriter(new FileWriter(fileName));
            lines.forEach(pw::println);
            pw.flush();
        } catch (IOException e) {
            System.out.println("FAILED: exception.");
            System.out.println(e);
            throw new RuntimeException();
        }
    }

    public Map<String, Long> assignmentsByLabel(TimeConstraintSet tcs) {
        Map<String, Long> variableAssignments = new HashMap<>();
        for (String s : tcs.getVariableNames()) {
            Vertex v = addOrGetCurrentVertex(s);
            try {
                variableAssignments.put(s, v.weight.longValue());
            } catch (Exception e) {
                System.out.println("EXCEPTION: tried to assign from vertex label " + s);
                //System.out.println("Vertices available: ");
                //vertices.forEach(v2 -> System.out.println(v2.label + ": " + (v2.weight != null ? Integer.toString(v2.weight) : "NO WEIGHT ASSIGNED")));
            }
        }

        return variableAssignments;
    }

    public void selfSetMaxEdgeWeightSum() {
        Map<Variable, Integer> emptyVariable = new HashMap<>();
        totalEdgeWeightSum = 0;
        for (Edge<V> e : edges) {
            if (!e.isVariableWeight()) {
                totalEdgeWeightSum += ((EdgeWeight.EdgeWeightInteger)e.weight).i;
            }
        }
    }

    public boolean assignedWeightsMatch(Graph<V> g2) {
        Set<String> vertexLabels = this.vertices.stream().map(v -> v.label).collect(Collectors.toSet());

        Set<String> vertexLabels2 = g2.vertices.stream().map(v -> v.label).collect(Collectors.toSet());

        vertexLabels.addAll(vertexLabels2);

        boolean allMatch = true;

        for (String vL : vertexLabels) {
            V v1 = this.getVertexIfExists(vL);
            V v2 = g2.getVertexIfExists(vL);

            String s1 = "";
            String s2 = "";

            if (v1 != null) {
                if (v1.weight != null) {
                    s1 = v1.weight.toString();
                }
                else {
                    s1 = "-";
                }
            }
            if (v2 != null) {
                if (v2.weight != null) {
                    s2 = v2.weight.toString();
                }
                else {
                    s2 = "-";
                }
            }

            if (!s1.equals(s2)){
                allMatch = false;
                System.out.println("Match failed: "+  vL + ": " + s1 + ", " + s2);
            }
        }


        return allMatch;
    }

    public int getVertexCount() {
        return vertices.size();
    }

    public int getEdgeCount() {
        return edges.size();
    }
}
