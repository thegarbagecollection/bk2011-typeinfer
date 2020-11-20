package timeconstraints;

import graph.Graph;
import graph.Variable;
import graph.Vertex;
import net.sf.javailp.Operator;

import java.util.Map;

public class TimeInequalityConstraint extends TimeConstraint {

    public TimeInequalityConstraint(TimeExpression lesser, TimeExpression greater) {
        this.lhs = lesser;
        this.rhs = greater;
    }
/*
    @Override
    public void addILPConstraint(LPWizard lpw, Variable.VariableFactory constraintNameGenerator, Map<String, Integer> smallestVarSummand) {
        String constraintName = constraintNameGenerator.newVariable("c").name();

        // if lv + lx <= rv + rx, then lx - rx <= rv - lv
        // Probably also library bugged
        // lpw.addConstraint(constraintName, lhs.summand - rhs.summand, "<=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();

        if (lhs.summand >= rhs.summand) {
            lpw.addConstraint(constraintName, lhs.summand - rhs.summand, "<=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();
        }
        else {
            lpw.addConstraint(constraintName, rhs.summand - lhs.summand, ">=").plus(lhs.varName).plus(rhs.varName, -1).setAllVariablesInteger();
        }

        updateVarSummands(smallestVarSummand);
        addVariablesToObjectiveFunction(lpw);
    }
*/

    @Override
    Operator getILPOperator() {
        return Operator.LE;
    }

    @Override
    public String toString() {
        return rhs.toString() + " >= " + lhs.toString();
    }

    @Override
    protected void addGraphConstraint(Graph g, Variable.VariableFactory variableFactory) {
        Vertex lessV = lhs.addVertex(g);
        Vertex moreV = rhs.addVertex(g);

        g.addEdgeVariableWeight(moreV, lessV, variableFactory);

    }

    @Override
    public String getHaskellRepresentation() {
        return lhs.asHaskellRepresentation() + "<" + rhs.asHaskellRepresentation();
    }
}
