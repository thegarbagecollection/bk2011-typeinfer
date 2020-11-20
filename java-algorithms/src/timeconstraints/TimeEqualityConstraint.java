package timeconstraints;

import graph.Graph;
import graph.Variable;
import graph.Vertex;
import net.sf.javailp.Linear;
import net.sf.javailp.Operator;
import net.sf.javailp.Problem;

import java.util.Map;

public class TimeEqualityConstraint extends TimeConstraint {


    public TimeEqualityConstraint(TimeExpression lhs, TimeExpression rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }
/*
    @Override
    public void addILPConstraint(LPWizard lpw, Variable.VariableFactory constraintNameGenerator, Map<String, Integer> smallestVarSummand) {
        String constraintName = constraintNameGenerator.newVariable("c").name();

        // if lv + lx = rv + rx, then lx - rx = rv - lv
        //lpw.addConstraint(constraintName, lhs.summand - rhs.summand, "=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();
        // This seems bugged in the library - if the LHS is < 0 it breaks. Garbage. Absolute shit. Love libraries with no documentation.
        // I wonder if it has any other delights in store???
        // Going to try modifying it to ensure +ve LHS

        if (lhs.summand >= rhs.summand) {
            //lpw.addConstraint(constraintName, lhs.summand - rhs.summand, "=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();
            lpw.addConstraint(constraintName, lhs.summand - rhs.summand, ">=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();
            lpw.addConstraint(constraintName, lhs.summand - rhs.summand, "<=").plus(rhs.varName).plus(lhs.varName, -1).setAllVariablesInteger();
        }
        else {
            //lpw.addConstraint(constraintName, rhs.summand - lhs.summand, "=").plus(lhs.varName).plus(rhs.varName, -1).setAllVariablesInteger();
            lpw.addConstraint(constraintName, rhs.summand - lhs.summand, ">=").plus(lhs.varName).plus(rhs.varName, -1).setAllVariablesInteger();
            lpw.addConstraint(constraintName, rhs.summand - lhs.summand, "<=").plus(lhs.varName).plus(rhs.varName, -1).setAllVariablesInteger();
        }


        updateVarSummands(smallestVarSummand);
        addVariablesToObjectiveFunction(lpw);
    }
*/

    @Override
    Operator getILPOperator() {
        return Operator.EQ;
    }

    @Override
    public String toString() {
        return lhs.toString() + " = " + rhs.toString();
    }

    @Override
    protected void addGraphConstraint(Graph g, Variable.VariableFactory variableFactory) {
        Vertex lessV = lhs.addVertex(g);
        Vertex moreV = rhs.addVertex(g);

        g.addEdgeFixedWeight(moreV, lessV, 0);
        g.addEdgeFixedWeight(lessV, moreV, 0);
    }

    @Override
    public String getHaskellRepresentation() {
        return lhs.asHaskellRepresentation() + "=" + rhs.asHaskellRepresentation();
    }
}
