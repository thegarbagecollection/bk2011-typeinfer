package graph;

public abstract class GraphSolverFactory {
    public static final GraphSolverFactory BROKEN_TEST_SOLVER_FACTORY = new NewTestBroken_Factory();
    public static final GraphSolverFactory DEPENDENT_SOLVER_FACTORY_CLEANED = new Cleaned_ByDependentVertex_Factory();
    public static final GraphSolverFactory DEPENDENT_SOLVER_FACTORY = new DependentVertexTestFactory();

    public abstract Algorithm create(ComponentGraph g);

    public static final GraphSolverFactory FORWARD_BACKWARD_REPEAT_SOLVER_FACTORY_ALGORITHM_1 = new ForwardBackwardRepeatPropagationFactory_Algorithm1();
    public static final GraphSolverFactory REPRESENTATIVE_VERTEX_SOLVER_FACTORY_ALGORITHM_2 =  new RepresentativeVertexFactory_Algorithm2();

    private static class ForwardBackwardRepeatPropagationFactory_Algorithm1 extends GraphSolverFactory{
        @Override
        public Algorithm create(ComponentGraph g) {
            return new Algorithm_ForwardBackward_RepeatPropagation(g);
        }
    }

    private static class NewTestBroken_Factory extends GraphSolverFactory {
        @Override
        public Algorithm create(ComponentGraph g) {
            return new Algorithm_NewTest_Broken(g);
        }
    }

    private static class DependentVertexTestFactory extends GraphSolverFactory {
        @Override
        public Algorithm create(ComponentGraph g) {
            return new Algorithm_ByDependentVertex(g);
        }
    }

    private static class Cleaned_ByDependentVertex_Factory extends GraphSolverFactory {
        @Override
        public Algorithm create(ComponentGraph g) {
            return new Algorithm_ByDependentVertex_Cleaned(g);
        }
    }

    private static class RepresentativeVertexFactory_Algorithm2 extends GraphSolverFactory {
        @Override
        public Algorithm create(ComponentGraph g) {
            return new Algorithm_ByRepresentativeVertex(g);
        }
    }
}
