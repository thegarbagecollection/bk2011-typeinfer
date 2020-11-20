package graph;

import java.util.HashMap;
import java.util.Map;

public class Variable {
    private String _name;

    private Variable(String name) {
        this._name = name;
    }

    public String name() {
        return _name;
    }

    @Override
    public String toString() {
        return name();
    }

    public static class VariableFactory {
        Map<String, Integer> counts;

        private VariableFactory() {
            counts = new HashMap<>();
        }

        private void incrementCount(String name) {
            counts.put(name, counts.get(name) + 1);
        }

        private Variable createVariable(String name) {
            return new Variable(name + counts.get(name));
        }

        private void initCount(String name) {
            if (!counts.containsKey(name)) counts.put(name, 0);
        }

        public Variable newVariable(String name) {
            initCount(name);
            incrementCount(name);
            return createVariable(name);
        }

        public Variable oldVariable(String name) {
            initCount(name);
            return createVariable(name);
        }

    }

    public static VariableFactory newFactory() {
        return new VariableFactory();
    }
}
