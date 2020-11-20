package graph;

import java.util.Map;
import java.util.Set;

public abstract class EdgeWeight {

    public abstract EdgeWeight copy(Variable.VariableFactory variableFactory);

    public abstract boolean isValidForSelfLoop();

    public abstract <V extends Vertex> void assignEdgeToSet(Edge<V> vEdge, Set<Edge<V>> fixedEdges, Set<Edge<V>> variableEdges);

    public abstract EdgeWeight returnModified(Map<Variable, Integer> edgeWeightMap);

    public abstract boolean isVariable();

    public EdgeWeight returnModifiedFromEdge(Edge<? extends Vertex> edge, Map<Variable, Integer> newEdgeVariableWeights) {
        return edge.weight.returnModified(newEdgeVariableWeights);
    }

    public abstract Integer getWeightFromTemp(Map<Variable, Integer> weights);

    static class EdgeWeightVariable extends EdgeWeight {
        Variable v;

        EdgeWeightVariable(Variable v) {
            this.v = v;
        }

        @Override
        public String toString() {
            return v.name();
        }

        @Override
        public EdgeWeight copy(Variable.VariableFactory variableFactory) {
            return new EdgeWeightVariable(variableFactory.newVariable("x"));
        }

        @Override
        public boolean isValidForSelfLoop() {
            return true;
        }

        @Override
        public <V extends Vertex> void assignEdgeToSet(Edge<V> vEdge, Set<Edge<V>> fixedEdges, Set<Edge<V>> variableEdges) {
            variableEdges.add(vEdge);
        }

        @Override
        public EdgeWeight returnModified(Map<Variable, Integer> edgeWeightMap) {
            if (edgeWeightMap.containsKey(v))
                return new EdgeWeightInteger(edgeWeightMap.get(v));
            else
                return this;
        }

        @Override
        public boolean isVariable() {
            return true;
        }


        @Override
        public Integer getWeightFromTemp(Map<Variable, Integer> weights) {
            return weights.get(v);
        }
    }

    static class EdgeWeightInteger extends EdgeWeight {
        int i;

        EdgeWeightInteger(int i) {
            this.i = i;
        }

        @Override
        public String toString() {
            return Integer.toString(i);
        }

        @Override
        public EdgeWeight copy(Variable.VariableFactory variableFactory) {
            return new EdgeWeightInteger(i);
        }

        @Override
        public boolean isValidForSelfLoop() {
            return i == 0;
        }

        @Override
        public <V extends Vertex> void assignEdgeToSet(Edge<V> vEdge, Set<Edge<V>> fixedEdges, Set<Edge<V>> variableEdges) {
            fixedEdges.add(vEdge);
        }

        @Override
        public EdgeWeight returnModified(Map<Variable, Integer> edgeWeightMap) {
            return this;
        }

        @Override
        public boolean isVariable() {
            return false;
        }

        @Override
        public Integer getWeightFromTemp(Map<Variable, Integer> weights) {
            return i;
        }
    }

    public static EdgeWeight createVariable(Variable v) {
        return new EdgeWeightVariable(v);
    }

    public static EdgeWeight createFixed(int weight) {
        return new EdgeWeightInteger(weight);
    }
}
