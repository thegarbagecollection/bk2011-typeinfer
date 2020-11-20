package graph;

import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;

public class Edge<V extends Vertex> {
    V from;
    V to;
    EdgeWeight weight;

    Edge<?> previousReference;

    public Edge(V from, V to, EdgeWeight weight) {
        this.from = from;
        this.to = to;
        this.weight = weight;
        previousReference = null;
    }

    public Edge(V from, V to, EdgeWeight weight, Edge<?> previousReference) {
        this.from = from;
        this.to = to;
        this.weight = weight;
        this.previousReference = previousReference;
    }

    public String toGraphViz() {
        return from.labelToGraphViz() + "->" + to.labelToGraphViz() + "[label=\"" + weight.toString() + "\"]" ;
    }

    public String toGraphVizWithTempWeight(Map<V, Integer> vertexWeights, Map<Variable, Integer> edgeWeights) {
        if (edgeWeights == null || weight.getWeightFromTemp(edgeWeights) == null) {
            if (!isVariableWeight())
                return from.labelToGraphVizWithTempWeight(vertexWeights) + "->" + to.labelToGraphVizWithTempWeight(vertexWeights) + "[label=\"?\"]";
            else
                return from.labelToGraphVizWithTempWeight(vertexWeights) + "->" + to.labelToGraphVizWithTempWeight(vertexWeights) + "[label=\"" + (((EdgeWeight.EdgeWeightVariable)weight).v.name()) + "\"]";
        }

        Integer w = weight.getWeightFromTemp(edgeWeights);

        return from.labelToGraphVizWithTempWeight(vertexWeights) + "->" + to.labelToGraphVizWithTempWeight(vertexWeights) + "[label=\"" + w.toString() + "\"]";

    }

    @Override
    public String toString() {
        return from.toString() + "-[" + weight.toString() +"]->" + to.toString();
    }

    public EdgeWeight copyWeight(Variable.VariableFactory variableFactory) {
        return weight.copy(variableFactory);
    }

    public boolean isValidForSelfLoop() {
        return weight.isValidForSelfLoop();
    }

    public void assignToSet(Set<Edge<V>> fixedEdges, Set<Edge<V>> variableEdges) {
        weight.assignEdgeToSet(this, fixedEdges, variableEdges);
    }

    public void setWeightFromMap(Map<Variable, Integer> edgeVariableWeights) {
        EdgeWeight newEW = weight.returnModified(edgeVariableWeights);
        weight = newEW;
        if (previousReference != null)  previousReference.weight = weight;
    }

    public void setWeightFromOtherEdgeAndAssociatedVariableWeights(Edge<? extends Vertex> edge, Map<Variable, Integer> newEdgeVariableWeights) {
        EdgeWeight newEW = weight.returnModifiedFromEdge(edge, newEdgeVariableWeights);
        weight = newEW;
        if (previousReference != null)  previousReference.weight = weight;
    }

    public boolean isVariableWeight() {
        return weight.isVariable();
    }

    public void setWeightDirect(int i) {
        EdgeWeight newEW = new EdgeWeight.EdgeWeightInteger(i);
        weight = newEW;
        if (previousReference != null)  previousReference.weight = weight;
    }


    public String toGraphVizWithTempWeightStyled(Map<String, Map<? extends Vertex, ? extends Object>> vertexWeights,
                                                 Map<Variable, Integer> primaryEdgeWeightByVariable,
                                                 Map<String, Map<Variable, ? extends Object>> auxiliaryEdgeWeightsByVariable,
                                                 Map<String, Map<Edge<? extends Vertex>, ? extends Object>> edgeWeightsExtra,
                                                 Map<Edge<? extends Vertex>, Map<String, String>> edgeStyling) {




        String styling = "";
        if (edgeStyling.containsKey(this)) {
            StringJoiner stylingJoiner = new StringJoiner(";");
            edgeStyling.get(this).forEach((k,v) -> stylingJoiner.add(k + "=" + v));
            styling = stylingJoiner.toString();
        }

        String fromLabel = from.labelToGraphVizWithTempWeightGeneral(vertexWeights);
        String toLabel = to.labelToGraphVizWithTempWeightGeneral(vertexWeights);

        String primaryWeightByVariable;

        if (weight.getWeightFromTemp(primaryEdgeWeightByVariable) == null) {
            if (!isVariableWeight())
                primaryWeightByVariable = "?";
            else
                primaryWeightByVariable = (((EdgeWeight.EdgeWeightVariable)weight).v.name());
        }
        else {
            primaryWeightByVariable = weight.getWeightFromTemp(primaryEdgeWeightByVariable).toString();
        }

        StringJoiner weightLabel = new StringJoiner("\\n").add(primaryWeightByVariable);

        if (isVariableWeight()) {
            auxiliaryEdgeWeightsByVariable.forEach((tag, varMap) -> {
                Object aux = varMap.get(((EdgeWeight.EdgeWeightVariable)weight).v);
                if (aux != null) weightLabel.add("[" + tag + " = " + aux.toString() + "]");
            });
        }

        edgeWeightsExtra.forEach((tag, edgeMap) -> {
            if (edgeMap.containsKey(this)) {
                Object aux = edgeMap.get(this);
                weightLabel.add("[" + tag + " = " + aux.toString() + "]");
            }
        });


        return fromLabel + "->" + toLabel + "[label=\"" + weightLabel.toString() + "\";" + styling + "]";

    }
}
