package graph;

import java.util.Map;
import java.util.StringJoiner;

public class Vertex {
    public Integer weight;

    String label;

    public Vertex(String label) {
        this.label = label;
        this.weight = null;
    }

    public Vertex(String label, int weight) {
        this.label = label;
        this.weight = weight;
    }

    public String labelToGraphViz() {
        if (weight == null)
            return "\"" + label + "\"";
        else
            return "\"" + label + "\\n[w = " + weight.toString() + "]\"";
    }

    public String labelToGraphVizWithTempWeight(Map<? extends Vertex, Integer> weights) {
        if (weights == null || weights.get(this) == null)
            return "\"" + label + "\"";
        else
            return "\"" + label + "\\n[w = " + weights.get(this) + "]\"";
    }

    public String labelToGraphVizWithTempWeightGeneral(Map<String, Map<? extends Vertex, ? extends Object>> weights) {

        StringJoiner sj = new StringJoiner("\\n");
        weights.forEach((tag, valueMap) -> {
            if (valueMap.containsKey(this)) {
                sj.add("[" +  tag + " = "  + valueMap.get(this).toString() + "]");
            }
        });

        String extraLabel = !sj.toString().equals("") ? "\\n" + sj.toString() : "";

        return "\"" + label + extraLabel + "\"";
    }

    @Override
    public String toString() {
        if (weight == null)
            return "(" + label + ")";
        else
            return "(" + label + "[" + weight.toString() + "])";
    }


    public String labelToGraphVizWithTempWeightStyled(Map<String, Map<? extends Vertex, ? extends Object>> weights, Map<? extends Vertex, Map<String,String>> styling) {
        String pureLabel = labelToGraphVizWithTempWeightGeneral(weights);

        StringJoiner sj = new StringJoiner(";","[","]");

        if (styling.containsKey(this)) {
            styling.get(this).forEach((k, v) -> sj.add(k + "=\"" + v + "\""));

            return pureLabel + sj.toString();
        }
        else {
            return pureLabel;
        }

    }
}
