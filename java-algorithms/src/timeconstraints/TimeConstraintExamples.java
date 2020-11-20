package timeconstraints;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TimeConstraintExamples {

    // These examples all from the recent document p12_direct_v2.pdf

    public static List<TimeConstraint> fromStrings(String... constraints) {
        List<TimeConstraint> tcs = new ArrayList<>();
        for (String s : constraints) {
            tcs.add(TimeConstraint.fromString(s.trim()));
        }
        return tcs;
    }

    public static List<TimeConstraint> fromStringsCSep(String constraintsAsOne) {
        String[] splitConstraints = constraintsAsOne.split(",");
        return fromStrings(splitConstraints);
    }


    // Note: we don't need X >= 0, X >= 1, since those are generated automatically

    private static List<TimeConstraint> ex_1_4_1tc = fromStrings("A = B + 1", "B >= A");

    private static List<TimeConstraint> ex_1_4_2tc = fromStrings("A = B - 1", "B >= A");

    private static List<TimeConstraint> ex_1_4_3tc = fromStrings("A = B - 2", "B >= A");

    private static List<TimeConstraint> ex_1_4_4tc = fromStrings("A = D", "B >= A", "C = B - 1", "D = C + 1");

    private static List<TimeConstraint> ex_1_4_5tc = fromStrings("E = B - 1", "C = E", "C = D", "A = C");

    private static List<TimeConstraint> ex_1_4_6tc = fromStrings("A = F", "F = B", "B = C", "D = G + 1", "G >= B", "E = H + 1", "H >= C", "I = J", "I >= A", "J >= C", "K = L", "K >= D", "L >= E", "N = K - 1", "M = I", "M = N", "C = M");

    private static List<TimeConstraint> ex_1_4_7tc = fromStrings("D = A", "A = B", "B = C", "G = F", "F >= B", "L = H + 1", "H >= A", "I >= C", "J = K", "K >= B", "M = J + 1", "L = I", "L = M", "N = L - 1", "E = G", "E = N", "C = E");

    private static List<TimeConstraint> ex_1_5_1tc = fromStrings("K + 1 = J", "K + 3 = J + 1");

    private static List<TimeConstraint> ex_1_5_2tc = fromStrings("K + 1 = J", "K + 2 = J + 1", "K + 3 = J + 2");

    private static List<TimeConstraint> ex_1_5_3tc = fromStrings("K = M", "K + 2 = J + 1", "M + 2 = J");

    private static List<TimeConstraint> ex_1_5_4tc = fromStrings("K >= J", "J >= K + 1");

    private static List<TimeConstraint> ex_1_5_5tc = fromStrings("K >= J", "K >= J + 1");

    private static List<TimeConstraint> ex_1_5_6tc = fromStrings("A = B - 1", "B = C - 1", "C = D - 1");

    private static List<TimeConstraint> ex_1_5_7tc = fromStrings("B - 1 >= A", "C - 1 >= B", "D - 1 >= C");

    private static List<TimeConstraint> ex_1_5_8tc = fromStrings("A >= B - 1", "B >= C - 1", "C >= D - 1");



    public static ProblemComparison ex_1_4_1 = new ProblemComparison(ex_1_4_1tc, "ex_1_4_1");


    public static ProblemComparison ex_1_4_2 = new ProblemComparison(ex_1_4_2tc, "ex_1_4_2");


    public static ProblemComparison ex_1_4_3 = new ProblemComparison(ex_1_4_3tc, "ex_1_4_3");

    public static ProblemComparison ex_1_4_4 = new ProblemComparison(ex_1_4_4tc, "ex_1_4_4");

    public static ProblemComparison ex_1_4_5 = new ProblemComparison(ex_1_4_5tc, "ex_1_4_5");

    public static ProblemComparison ex_1_4_6 = new ProblemComparison(ex_1_4_6tc, "ex_1_4_6");

    public static ProblemComparison ex_1_4_7 = new ProblemComparison(ex_1_4_7tc, "ex_1_4_7");

    public static ProblemComparison ex_1_5_1 = new ProblemComparison(ex_1_5_1tc, "ex_1_5_1");

    public static ProblemComparison ex_1_5_2 = new ProblemComparison(ex_1_5_2tc, "ex_1_5_2");

    public static ProblemComparison ex_1_5_3 = new ProblemComparison(ex_1_5_3tc, "ex_1_5_3");

    public static ProblemComparison ex_1_5_4 = new ProblemComparison(ex_1_5_4tc, "ex_1_5_4");

    public static ProblemComparison ex_1_5_5 = new ProblemComparison(ex_1_5_5tc, "ex_1_5_5");

    public static ProblemComparison ex_1_5_6 = new ProblemComparison(ex_1_5_6tc, "ex_1_5_6");

    public static ProblemComparison ex_1_5_7 = new ProblemComparison(ex_1_5_7tc, "ex_1_5_7");

    public static ProblemComparison ex_1_5_8 = new ProblemComparison(ex_1_5_8tc, "ex_1_5_8");




    public static ProblemComparison edgeTest = new ProblemComparison(fromStrings("A >= B"), "edgetest");


    public static ProblemComparison failing_1 = new ProblemComparison(fromStrings("X0 >= X1", "X3 >= X2", "X2 + 4 = X2 + 5"), "failing_1_batch_1_Random_28");
    public static ProblemComparison failing_1_mod = new ProblemComparison(fromStrings("X2 + 4 >= X2 + 5", "X2 + 5 >= X2 + 4"), "failing_2_batch_1_Random_28_mod");

    public static ProblemComparison failing_2 = new ProblemComparison(fromStrings("X3 - 4 = X4", "X0 + 3 = X2", "X3 >= X3", "X2 = X2"),"failing_2_batch_1_Random_39");

    public static ProblemComparison failing_3 = new ProblemComparison(fromStrings("X3 = X0 - 1", "X1 = X1", "X1 = X0 - 2", "X3 = X3", "X2 >= X3"), "failing_3_batch_1_Random_41");
    public static ProblemComparison failing_4 = new ProblemComparison(fromStrings("X0 >= X0", "X2 >= X1 + 1", "X3 >= X1 - 5", "X0 = X3", "X2 = X2"), "failing_4_batch_1_Random_42");


    public static ProblemComparison failing_5 = new ProblemComparison(fromStrings("X3 = X0 - 1", "X3 + 3 >= X2", "X2 - 4 >= X4"), "failing_5_batch_2_Random_23");

    public static ProblemComparison failing_6 = new ProblemComparison(fromStrings("X2 + 2 >= X3 + 1", "X2 >= X3 - 5", "X1 - 4 = X2 - 2"), "failing_6_batch_3_Random_240");

    public static ProblemComparison failing_7 = new ProblemComparison(fromStrings("X3 = X4", "X3 - 5 >= X0 - 4", "X3 >= X4 - 2", "X4 >= X4"), "failing_7_batch_3_Random_301");

    public static ProblemComparison failing_8 = new ProblemComparison(fromStrings("X4 >= X1 - 2", "X3 >= X4 - 4", "X0 + 1 >= X0", "X1 >= X4 + 2"), "failing_8_batch_3_Random_356");

    public static ProblemComparison failing_9 = new ProblemComparison(fromStrings("X3 >= X2", "X2 >= X0 - 3", "X0 >= X3 - 2", "X1 + 5 = X0"), "failing_9_batch_3_Random_394");

    public static ProblemComparison failing_10 = new ProblemComparison(fromStrings("X3 >= X0", "X4 - 5 = X2", "X2 >= X1", "X0 = X3", "X2 - 4 = X1 + 1"), "failing_10_batch_3_Random_402");


    public static ProblemComparison failing_11 = new ProblemComparison(fromStrings("X2 >= X3", "X1 >= X0", "X2 >= X1", "X0 + 2 >= X1", "X2 = X0 + 3"), "failing_11_batch_3_Random_433");

    public static ProblemComparison failing_12 = new ProblemComparison(fromStrings("X4 = X4", "X4 + 3 = X3", "X1 >= X3 - 1", "X0 + 4 >= X4", "X0 = X3 + 3"), "failing_12_batch_3_Random_436");

    public static ProblemComparison failing_13 = new ProblemComparison(fromStrings("X3 + 1 >= X1 - 4", "X4 - 3 >= X3", "X3 >= X2 - 5", "X2 = X0", "X3 >= X2"), "failing_13_batch_3_Random_471");

    public static ProblemComparison failing_14 = new ProblemComparison(fromStrings("X3 >= X2 - 1", "X4 = X2", "X2 >= X0 + 3", "X1 >= X1", "X0 >= X0"), "failing_14_batch_3_Random_481");

    public static ProblemComparison failing_15 = new ProblemComparison(fromStrings("X2 - 1 = X3 - 4", "X0 - 4 = X3", "X1 - 5 = X3", "X4 + 4 = X2", "X4 >= X4 - 3"), "failing_15_batch_3_Random_496");

    public static ProblemComparison failing_16 = new ProblemComparison(fromStringsCSep("X1 >= X2, X1 >= X3, X0 - 2 = X4, X1 = X0"), "failing_16_batch_4_Random_326");

    public static ProblemComparison failing_17 = new ProblemComparison(fromStringsCSep("X4 >= X4 - 4," +
            "X2 = X1," +
            "X4 >= X3," +
            "X4 = X0," +
            "X4 + 1 >= X3"), "failing_17_batch_4_Random_491");

    public static ProblemComparison failing_18 = new ProblemComparison(fromStringsCSep("X2 + 12 >= X0, X0 + 6 >= X2 - 13, X0 >= X1 + 13"), "failing_18_batch_6_Random_4724");



    public static ProblemComparison failing_19 = new ProblemComparison(fromStringsCSep("X0 = X1 + 7, X2 >= X3 + 2, X0 >= X2, X0 - 11 = X3"), "failing_18_batch_7_Random_158");


    public static List<ProblemComparison> allExamples = Arrays.asList(
            edgeTest,
            ex_1_4_1,
            ex_1_4_2,
            ex_1_4_3,
            ex_1_4_4,
            ex_1_4_5,
            ex_1_4_6,
            ex_1_4_7,
            ex_1_5_1,
            ex_1_5_2,
            ex_1_5_3,
            ex_1_5_4,
            ex_1_5_5,
            ex_1_5_6,
            ex_1_5_7,
            ex_1_5_8);

    public static List<ProblemComparison> allFailings = Arrays.asList(
            failing_1,
            failing_1_mod,
            failing_2,
            failing_3,
            failing_4,
            failing_5,
            failing_6,
            failing_7,
            failing_8,
            failing_9,
            failing_10,
            failing_11,
            failing_12,
            failing_13,
            failing_14,
            failing_15,
            failing_16,
            failing_17,
            failing_18
    );

    public static ProblemRunner examples = new ProblemRunner(allExamples);

    public static ProblemRunner failings = new ProblemRunner(allFailings);


    public static ProblemRunner all = examples.combine(failings);


}
