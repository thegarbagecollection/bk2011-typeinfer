package timeconstraints;

import graph.Graph;
import graph.GraphComponentFactory;
import graph.Variable;
import graph.Vertex;
import net.sf.javailp.Linear;
import net.sf.javailp.Operator;
import net.sf.javailp.OptType;
import net.sf.javailp.Problem;

import java.util.*;

public abstract class TimeConstraint {
    TimeExpression lhs;
    TimeExpression rhs;

    public abstract String getHaskellRepresentation();

    void addVariableNames(Set<String> variables) {
        lhs.addVariable(variables);
        rhs.addVariable(variables);
    }

    void updateVarSummands(Map<String, Integer> smallestVarSummand) {
        lhs.updateVarSummand(smallestVarSummand);
        rhs.updateVarSummand(smallestVarSummand);
    }

    private void addILPConstraint(Problem problem) {
        // our constraints are
        // x + a = y + b     <=> x - y = b - a
        // x + a <= y + b    <=> x - y <= b - a

        Linear constraint = new Linear();

        constraint.add(1, lhs.varName);
        constraint.add(-1,rhs.varName);

        problem.add(constraint, getILPOperator(), rhs.summand - lhs.summand);

    }

    abstract Operator getILPOperator();

    private static void addVariablesToObjectiveFunction(Set<String> variables, Problem problem) {
        Linear objFn = new Linear();
        variables.forEach(variable -> objFn.add(1, variable));
        problem.setObjective(objFn, OptType.MIN);
    }


    public static Problem createILPConstraints(List<TimeConstraint> constraints) {
        Map<String, Integer> smallestSummandForVar = new HashMap<>();

        Set<String> variables = new HashSet<>();

        Problem problem = new Problem();

        constraints.forEach(tc -> {
            tc.addILPConstraint(problem);
            tc.updateVarSummands(smallestSummandForVar);
            tc.addVariableNames(variables);
        });

        addVariablesToObjectiveFunction(variables, problem);

        // 'smallest summand' n for a variable x will be either 0 or < 0.
        // For n = 0, we create a constraint 0 <= x
        // For n < 0, we create a constraint -n <= x

        smallestSummandForVar.forEach((x, n) -> {
            Linear minVal = new Linear();
            minVal.add(1,x);
            problem.add(minVal, Operator.GE, -n); // this works fine as long as 0 = -0!
        });

        variables.forEach(variable -> {
            problem.setVarType(variable, Integer.class);
        });


        return problem;

    }

    /*
    public abstract void addILPConstraint(LPWizard lpw, Variable.VariableFactory constraintNameGenerator, Map<String, Integer> smallestVarSummand);

    // We try and minimize ALL variables, so it doesn't matter which is root. Close enough.
    void addVariablesToObjectiveFunction(LPWizard lpw) {
        lpw.plus(lhs.varName).plus(rhs.varName);
    }

    public static LPWizard createILPConstraints(List<TimeConstraint> constraints) {
        LPWizard lpw = new LPWizard();

        Map<String, Integer> smallestSummandForVar = new HashMap<>();

        Variable.VariableFactory cng = Variable.newFactory();

        for (TimeConstraint timeConstraint : constraints) {
            //System.out.println("Handling constraint " + timeConstraint);
            timeConstraint.addILPConstraint(lpw, cng, smallestSummandForVar);
        }

        // 'smallest summand' n for a variable x will be either 0 or < 0.
        // For n = 0, we create a constraint 0 <= x
        // For n < 0, we create a constraint -n <= x

        smallestSummandForVar.forEach((x, n) -> {
            System.out.println("Smallest summand for " + x + ": " + n);
            String cvar1 = cng.newVariable("c").name();
            String cvar2 = cng.newVariable("c").name();

            //System.out.println("Constraint variables " + cvar1 + "," + cvar2);
            if (n.equals(0)) {
                //System.out.println("Adding constraint 0 <= " + x);
                lpw.addConstraint(cvar1, 0, "<=").plus(x).setAllVariablesInteger();
            }
            else if (n < 0) {
                System.out.println("Adding constraint " + (-n) + " <= " + x);
                lpw.addConstraint(cvar2, -n, "<=").plus(x).setAllVariablesInteger();
            }
        });

        lpw.setAllVariablesInteger();

        lpw.setMinProblem(true);

        return lpw;
    }

*/


    public static TimeConstraint fromString(String constraint) {
        if (constraint.contains(">=")) {
            String[] splits = constraint.split(">=");
            String lhss = splits[0].trim();
            String rhss = splits[1].trim();

            TimeExpression lhs = TimeExpression.fromString(lhss);
            TimeExpression rhs = TimeExpression.fromString(rhss);

            return new TimeInequalityConstraint(rhs, lhs);
        }
        else if (constraint.contains("=")) {
            String[] splits = constraint.split("=");
            String lhss = splits[0].trim();
            String rhss = splits[1].trim();

            TimeExpression lhs = TimeExpression.fromString(lhss);
            TimeExpression rhs = TimeExpression.fromString(rhss);

            return new TimeEqualityConstraint(lhs, rhs);
        } else {
            throw new RuntimeException("Could not create constraint from " + constraint);
        }

    }



    public static Graph buildGraph(List<TimeConstraint> tcList) {
        // BUG found: we CAN'T set ALL "source" SCC vertices to 0 - see failing_6_batch_3_Random_240:
        // we might get, say, X2-2 = 0 as a source, but elsewhere it turns out that X2-2 > 0 (say, X2 = X1 + 4 or similar)
        // so we have to add variable-weight edges from every negative-summand expression vertex to 0-vertex. Positive-summand
        // is fine, since for X + a, X is always added as a base.

        Graph<Vertex> g = new Graph<>(GraphComponentFactory.STANDARD);

        Map<String, Integer> smallestSummandForVar = new HashMap<>();

        Map<String, Set<VertexWithSummand>> variablesToTheirVertices = new HashMap<>();

        Variable.VariableFactory variableFactory = Variable.newFactory();

        tcList.forEach(timeConstraint -> {
            timeConstraint.updateVarSummands(smallestSummandForVar);
            timeConstraint.addGraphConstraint(g, variableFactory);
            timeConstraint.collectVariableExpressions(g, variablesToTheirVertices);
        });

        // Next need all inequalities of form X >= 0, X >= 1
        // 'smallest summand' n for a variable x will be either 0 or < 0.
        // For n = 0, we create a constraint 0 <= x, so a variable-weight edge to the zero vertex
        // For n < 0, we create a constraint -n <= x, so an extra vertex x', a variable weight edge x->x', and a weight abs(n) edge x'->0

        Vertex zeroVertex = g.addOrGetCurrentVertex("0");

        tcList.forEach(timeConstraint -> {
            timeConstraint.addNegativeExpressionGraphConstraint(g, variableFactory, zeroVertex);
        });

        smallestSummandForVar.forEach((xS, n) -> {
            Vertex x = g.addOrGetCurrentVertex(xS); // in case we only have X + a expressions for X, and no expression of just X, we need to add X

            // We also need to make sure variablesToTheirVertices contains X + 0 for every X, in case we just added it.
            // otherwise we won't have a relation to X from anything X + 1, X + 2, etc. Or, from X to X - 1, X - 2 etc.
            variablesToTheirVertices.get(xS).add(new VertexWithSummand(x, 0));

            if (n == 0) {
                g.addEdgeVariableWeight(x, zeroVertex, variableFactory);
            }
            else if (n < 0) {
                Vertex xPrime = g.addOrGetCurrentVertex(xS + "'");
                g.addEdgeVariableWeight(x, xPrime, variableFactory);
                g.addEdgeFixedWeight(xPrime, zeroVertex, -n);
            }
        });

        // Finally need all equation differences (K + 1 >= K, edge weight (K+1)->(K) of 1 etc.)
        for (Map.Entry<String, Set<TimeConstraint.VertexWithSummand>> entry : variablesToTheirVertices.entrySet()) {

            Set<TimeConstraint.VertexWithSummand> verticesWithSummands = entry.getValue();

            List<VertexWithSummand> vwsList = new ArrayList<>(verticesWithSummands);

            // Reversed sort, so largest vertices first
            vwsList.sort(Comparator.comparingInt(o -> ((VertexWithSummand) o).summand).reversed());



            // We want pairwise with no duplicates
            for (int i = 0; i < vwsList.size(); i++) {
                for (int j = i + 1; j < vwsList.size(); j++) {
                    VertexWithSummand larger = vwsList.get(i);
                    VertexWithSummand smaller = vwsList.get(j);

                    assert larger.summand > smaller.summand;

                    int weightDiff = larger.summand - smaller.summand;

                    g.addEdgeFixedWeight(larger.v, smaller.v, weightDiff);

                }
            }
        };

        g.selfSetMaxEdgeWeightSum();

        return g;
    }

    private void addNegativeExpressionGraphConstraint(Graph<Vertex> g, Variable.VariableFactory variableFactory, Vertex zeroVertex) {
        lhs.addNegativeGraphConstraint(g, variableFactory, zeroVertex);
        rhs.addNegativeGraphConstraint(g, variableFactory, zeroVertex);
    }

    private void collectVariableExpressions(Graph g, Map<String, Set<VertexWithSummand>> variablesToTheirVertices) {
        lhs.collectVariableExpression(g, variablesToTheirVertices);
        rhs.collectVariableExpression(g, variablesToTheirVertices);
    }

    static class VertexWithSummand {
        Vertex v;
        int summand;

        public VertexWithSummand(Vertex v, int summand) {
            this.v = v;
            this.summand = summand;
        }

        @Override
        public String toString() {
            return v.toString() + "+[" + summand + "]";
        }

        // Need these to not find duplicates where the same vertex has the same summand in two separate equations
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            VertexWithSummand that = (VertexWithSummand) o;
            return summand == that.summand &&
                    Objects.equals(v, that.v);
        }

        @Override
        public int hashCode() {

            return Objects.hash(v, summand);
        }
    }

    protected abstract void addGraphConstraint(Graph g, Variable.VariableFactory variableFactory);
}
