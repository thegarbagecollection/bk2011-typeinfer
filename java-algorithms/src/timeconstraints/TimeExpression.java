package timeconstraints;

import graph.Graph;
import graph.Variable;
import graph.Vertex;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class TimeExpression {
    String varName;
    int summand;


    public TimeExpression(String varName, int summand) {
        this.varName = varName;
        this.summand = summand;
    }

    void updateVarSummand(Map<String, Integer> smallestVarSummand) {
        if ((!smallestVarSummand.containsKey(varName)) || smallestVarSummand.get(varName) > summand) {
            smallestVarSummand.put(varName, Math.min(summand, 0));
        }
    }

    String asLabel() {
        if (summand > 0) {
            return varName + "+" + summand;
        }
        else if (summand < 0) {
            return varName + "-" + (-summand);
        }
        else {
            return varName;
        }
    }

    public String asHaskellRepresentation() {
        return varName + "," + summand;
    }

    public static TimeExpression fromString(String s) {
        if (s.contains("+")) {
            String[] ss = s.split("\\+");

            String varName = ss[0].trim();
            int summand = Integer.parseInt(ss[1].trim());

            return new TimeExpression(varName, summand);

        } else if (s.contains("-")) {
            String[] ss = s.split("-");

            String varName = ss[0].trim();
            int summand = 0 - Integer.parseInt(ss[1].trim());

            return new TimeExpression(varName, summand);

        } else { // presumably it's a single variable?
            return new TimeExpression(s.trim(), 0);
        }
    }

    @Override
    public String toString() {
        if (summand > 0) {
            return varName + " + " + summand;
        }
        else if (summand < 0) {
            return varName + " - " + (-summand);
        }
        else {
            return varName;
        }
    }

    void addVariable(Set<String> variables) {
        variables.add(varName);
    }

    public Vertex addVertex(Graph g) {
        String label = asLabel();
        return g.addOrGetCurrentVertex(label);
    }

    public void collectVariableExpression(Graph g, Map<String, Set<TimeConstraint.VertexWithSummand>> variablesToTheirVertices) {
        String label = asLabel();
        if (!variablesToTheirVertices.containsKey(varName)) variablesToTheirVertices.put(varName, new HashSet<>());

        Vertex v = g.addOrGetCurrentVertex(label);

        variablesToTheirVertices.get(varName).add(new TimeConstraint.VertexWithSummand(v, summand));
    }

    public void addNegativeGraphConstraint(Graph<Vertex> g, Variable.VariableFactory variableFactory, Vertex zeroVertex) {
        if (summand < 0) {
            Vertex v = g.addOrGetCurrentVertex(asLabel());
            g.addEdgeVariableWeight(v, zeroVertex, variableFactory);
        }
    }
}
