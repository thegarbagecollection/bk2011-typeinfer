package graph;

public abstract  class GraphComponentFactory<V extends Vertex> {
    public abstract V createVertex(String label);

    public abstract Edge<V> createEdge(V from, V to, EdgeWeight edgeWeight);

    public abstract Edge<V> createEdgeWithPreviousReference(V from, V to, EdgeWeight edgeWeight, Edge<?> previousReference);

    public static final GraphComponentFactory<Vertex> STANDARD = new GraphComponentFactory<Vertex>() {
        @Override
        public Vertex createVertex(String label) {
            return new Vertex(label);
        }

        @Override
        public Edge<Vertex> createEdge(Vertex from, Vertex to, EdgeWeight edgeWeight) {
            return new Edge<>(from, to, edgeWeight);
        }

        @Override
        public Edge<Vertex> createEdgeWithPreviousReference(Vertex from, Vertex to, EdgeWeight edgeWeight, Edge<?> previousReference) {
            return new Edge<>(from, to, edgeWeight, previousReference);
        }
    };


    public static final GraphComponentFactory<ComponentVertex> COMPONENT = new GraphComponentFactory<ComponentVertex>() {
        @Override
        public ComponentVertex createVertex(String label) {
            return new ComponentVertex(label);
        }

        @Override
        public Edge<ComponentVertex> createEdge(ComponentVertex from, ComponentVertex to, EdgeWeight edgeWeight) {
            return new Edge<>(from, to, edgeWeight);
        }

        @Override
        public Edge<ComponentVertex> createEdgeWithPreviousReference(ComponentVertex from, ComponentVertex to, EdgeWeight edgeWeight, Edge<?> previousReference) {
            return new Edge<>(from, to, edgeWeight, previousReference);
        }
    };

}
