package graph;

import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;

public class ComponentVertex extends Vertex {
    Set<Vertex> componentContents;


    public static String label(Set<Vertex> componentContents) {
        // Default label is just all the labels of all vertices involved, concatenated with ';'
        StringJoiner sj = new StringJoiner(";");
        componentContents.forEach(vertex -> sj.add(vertex.label));
        return sj.toString();
    }

    public ComponentVertex(String label) {
        super(label);
    }

    public void setComponentContents(Set<Vertex> componentContents) {
        this.componentContents = componentContents;
    }

    @Override
    public String labelToGraphViz() {
        // For Graphviz - line-separated, surrounded by "
        StringJoiner sj = new StringJoiner("\\n","\"","\"");
        componentContents.forEach(vertex -> sj.add(vertex.label));
        if (weight != null) sj.add("[w = " + weight.toString() + "]");
        return sj.toString();
    }

    @Override
    public String labelToGraphVizWithTempWeightGeneral(Map<String, Map<? extends Vertex, ? extends Object>> weights) {

        StringJoiner sj = new StringJoiner("\\n");
        weights.forEach((tag, valueMap) -> {
            if (valueMap.containsKey(this)) {
                sj.add("[" +  tag + " = "  + valueMap.get(this).toString() + "]");
            }
        });

        String extraLabel = !sj.toString().equals("") ? "\\n" + sj.toString() : "";

        String tempLabel = labelToGraphViz().replaceAll("\\\"", "");

        return "\"" + tempLabel + extraLabel + "\"";
    }

    @Override
    public String toString() {
        // For in-program and debug display - same as label, but with () surrounding
        StringJoiner sj = new StringJoiner(";", "(", ")");
        componentContents.forEach(vertex -> sj.add(vertex.label));
        if (weight != null) sj.add("[w = " + weight.toString() + "]");
        return sj.toString();
    }

}
