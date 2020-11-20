package timeconstraints;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomConstraintGenerator {
    Random r = new Random();

    private int variableCount;
    private int maxSummand;

    private double fractionZeroSummand; // what fraction of the summands generated should be 0? others will be equally divided

    private double fractionEquality; // bias towards equality constraints

    private int constraintCount;

    private List<String> variables;

    public RandomConstraintGenerator(int variableCount, int maxSummand, double fractionZeroSummand, int constraintCount, double fractionEquality) {
        this.variableCount = variableCount;
        this.maxSummand = maxSummand;
        this.fractionZeroSummand = fractionZeroSummand;
        this.constraintCount = constraintCount;
        this.fractionEquality = fractionEquality;

        initializeVariables();
    }

    private void initializeVariables() {
        variables = new ArrayList<>();
        for (int i = 0; i < variableCount; i++) {
            variables.add("X" + i);
        }
    }

    private String getRandomVariable() {
        int next = r.nextInt(variableCount);
        return variables.get(next);
    }

    private int getRandomSummand() {
        double frac = r.nextDouble();

        if (frac < fractionZeroSummand) {
            return 0;
        }

        int multiplier;

        if (r.nextDouble() < 0.5) {
            multiplier = 1;
        }
        else {
            multiplier = -1;
        }

        int summand = r.nextInt(maxSummand) + 1; // even distribution over possible non-zero summands

        return multiplier * summand;
    }

    private TimeExpression generateExpression() {
        String variable = getRandomVariable();
        int summand = getRandomSummand();
        return new TimeExpression(variable, summand);
    }

    private TimeConstraint generateConstraint() {
        if (r.nextDouble() < fractionEquality) {
            return new TimeEqualityConstraint(generateExpression(), generateExpression());
        }
        else {
            return new TimeInequalityConstraint(generateExpression(), generateExpression());
        }
    }

    public List<TimeConstraint> generateConstraintList() {
        List<TimeConstraint> timeConstraints = new ArrayList<>();
        for (int i = 0; i < constraintCount; i++) {
            timeConstraints.add(generateConstraint());
        }
        return timeConstraints;
    }
}
