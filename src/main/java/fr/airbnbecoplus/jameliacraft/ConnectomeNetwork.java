package fr.airbnbecoplus.jameliacraft;

import fr.airbnbecoplus.jameliacraft.connectome.ConnectomeRunResult;
import fr.airbnbecoplus.jameliacraft.connectome.Neuron;
import fr.airbnbecoplus.jameliacraft.connectome.NeuronActivation;
import fr.airbnbecoplus.jameliacraft.connectome.Synapse;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Locale;

final class ConnectomeNetwork {
    private final Map<Long, Neuron> neuronsByBodyId;
    private final Map<Neuron, List<Synapse>> outgoingSynapses;
    private final Map<Neuron, VisualInput> visualInputs;
    private final Map<Neuron, MotorOutput> motorOutputs;

    private ConnectomeNetwork(
            Map<Long, Neuron> neuronsByBodyId,
            Map<Neuron, List<Synapse>> outgoingSynapses,
            Map<Neuron, VisualInput> visualInputs,
            Map<Neuron, MotorOutput> motorOutputs
    ) {
        this.neuronsByBodyId = Map.copyOf(neuronsByBodyId);
        this.outgoingSynapses = Map.copyOf(outgoingSynapses);
        this.visualInputs = Map.copyOf(visualInputs);
        this.motorOutputs = Map.copyOf(motorOutputs);
    }

    static ConnectomeNetwork load(Path nodesFile, Path edgesFile) throws IOException {
        return load(nodesFile, edgesFile, null, null);
    }

    static ConnectomeNetwork load(
            Path nodesFile,
            Path edgesFile,
            Path visualInputsFile,
            Path motorOutputsFile
    ) throws IOException {
        Map<Long, Neuron> neurons = loadNeurons(nodesFile);
        Map<Neuron, List<RawSynapse>> rawSynapses = loadRawSynapses(edgesFile, neurons);
        Map<Neuron, List<Synapse>> synapses = normalizeSynapses(rawSynapses);
        Map<Neuron, VisualInput> visualInputs = visualInputsFile == null
                ? Map.of() : loadVisualInputs(visualInputsFile, neurons);
        Map<Neuron, MotorOutput> motorOutputs = motorOutputsFile == null
                ? Map.of() : loadMotorOutputs(motorOutputsFile, neurons);
        return new ConnectomeNetwork(neurons, synapses, visualInputs, motorOutputs);
    }

    ConnectomeRunResult run(long inputBodyId, int steps, double decay, double threshold, int resultCount) {
        Neuron inputNeuron = neuronsByBodyId.get(inputBodyId);
        if (inputNeuron == null && !visualInputs.isEmpty()) {
            inputNeuron = visualInputs.keySet().iterator().next();
        }
        if (inputNeuron == null) {
            throw new IllegalArgumentException("bodyid " + inputBodyId + " n'existe pas dans le fichier nodes");
        }

        Map<Neuron, Double> state = new HashMap<>();
        state.put(inputNeuron, 1.0);
        for (int step = 0; step < steps; step++) {
            Map<Neuron, Double> incomingActivity = new HashMap<>();
            for (Map.Entry<Neuron, Double> entry : state.entrySet()) {
                double activation = Math.tanh(Math.max(0, entry.getValue() - threshold));
                if (activation == 0) {
                    continue;
                }
                for (Synapse synapse : outgoingSynapses.getOrDefault(entry.getKey(), List.of())) {
                    incomingActivity.merge(
                            synapse.target(),
                            synapse.weight() * activation,
                            Double::sum
                    );
                }
            }

            Map<Neuron, Double> nextState = new HashMap<>();
            for (Neuron neuron : neuronsByBodyId.values()) {
                double nextValue = decay * state.getOrDefault(neuron, 0.0)
                        + incomingActivity.getOrDefault(neuron, 0.0);
                if (nextValue != 0) {
                    nextState.put(neuron, nextValue);
                }
            }
            state = nextState;
        }

        List<NeuronActivation> activations = state.entrySet().stream()
                .filter(entry -> entry.getValue() > threshold)
                .sorted(Map.Entry.<Neuron, Double>comparingByValue().reversed())
                .limit(resultCount)
                .map(entry -> new NeuronActivation(entry.getKey(), entry.getValue()))
                .toList();

        long activeNeuronCount = state.values().stream()
                .filter(value -> value > threshold)
                .count();
        return new ConnectomeRunResult(inputNeuron, steps, activations, activeNeuronCount);
    }

    Simulation createSimulation(long inputBodyId) {
        Neuron inputNeuron = neuronsByBodyId.get(inputBodyId);
        if (inputNeuron == null && !visualInputs.isEmpty()) {
            inputNeuron = visualInputs.keySet().iterator().next();
        }
        if (inputNeuron == null) {
            throw new IllegalArgumentException("bodyid " + inputBodyId + " n'existe pas dans le fichier nodes");
        }
        return new Simulation(inputNeuron, new HashMap<>());
    }

    MotorCommand step(Simulation simulation, SensorSnapshot sensors) {
        double threshold = 0.01;
        double decay = 0.9;
        for (Map.Entry<Neuron, VisualInput> entry : visualInputs.entrySet()) {
            double signal = sensors.value(entry.getValue().signal(), entry.getValue().side());
            if (signal > 0) simulation.state().put(entry.getKey(), signal);
            else simulation.state().remove(entry.getKey());
        }
        Map<Neuron, Double> incomingActivity = new HashMap<>();

        for (Map.Entry<Neuron, Double> entry : simulation.state().entrySet()) {
            double activation = Math.tanh(Math.max(0, entry.getValue() - threshold));
            for (Synapse synapse : outgoingSynapses.getOrDefault(entry.getKey(), List.of())) {
                incomingActivity.merge(synapse.target(), synapse.weight() * activation, Double::sum);
            }
        }

        Map<Neuron, Double> nextState = new HashMap<>();
        Map<Neuron, Double> candidates = new HashMap<>(simulation.state());
        incomingActivity.forEach((neuron, value) -> candidates.merge(neuron, value, Double::sum));
        for (Map.Entry<Neuron, Double> entry : candidates.entrySet()) {
            double nextValue = decay * simulation.state().getOrDefault(entry.getKey(), 0.0)
                    + incomingActivity.getOrDefault(entry.getKey(), 0.0);
            if (nextValue != 0) {
                nextState.put(entry.getKey(), nextValue);
            }
        }
        if (simulation.state().isEmpty() && !visualInputs.isEmpty()) {
            nextState.put(visualInputs.keySet().iterator().next(), 0.1);
        }
        simulation.state().clear();
        simulation.state().putAll(nextState);

        double left = 0;
        double right = 0;
        double forward = 0;
        double lift = 0;
        double wingBeat = 0;
        int leftSteeringCount = 0;
        int rightSteeringCount = 0;
        int liftCount = 0;
        int wingBeatCount = 0;
        for (Map.Entry<Neuron, Double> entry : simulation.state().entrySet()) {
            MotorOutput output = motorOutputs.get(entry.getKey());
            if (output == null) {
                continue;
            }
            double activity = Math.max(0, entry.getValue());
            switch (output.action().toUpperCase(Locale.ROOT)) {
                case "STEERING" -> {
                    if ("L".equalsIgnoreCase(output.side())) {
                        left += activity;
                        leftSteeringCount++;
                    }
                    if ("R".equalsIgnoreCase(output.side())) {
                        right += activity;
                        rightSteeringCount++;
                    }
                }
                case "FORWARD_ACCEL" -> forward += activity;
                case "LIFT" -> {
                    lift += activity;
                    liftCount++;
                }
                case "WING_BEAT" -> {
                    wingBeat += activity;
                    wingBeatCount++;
                }
                case "BACKWARD_DECEL" -> forward -= activity * 0.5;
                default -> {
                }
            }
        }

        double balancedLeft = leftSteeringCount == 0 ? 0 : left / leftSteeringCount;
        double balancedRight = rightSteeringCount == 0 ? 0 : right / rightSteeringCount;
        double balancedLift = liftCount == 0 ? 0 : lift / liftCount;
        double balancedWingBeat = wingBeatCount == 0 ? 0 : wingBeat / wingBeatCount;
        double turn = Math.max(-3, Math.min(3, (balancedRight - balancedLeft) * 8));
        double speed = Math.max(0, Math.min(0.18, forward * 0.12));
        double verticalSpeed = balancedWingBeat > threshold
                ? Math.max(-0.2, Math.min(0.2, balancedLift * 0.12))
                : 0;
        return new MotorCommand((float) turn, speed, verticalSpeed, balancedWingBeat);
    }

    MotorCommand step(Simulation simulation) {
        return step(simulation, SensorSnapshot.none());
    }

    List<NeuronActivation> currentActivations(Simulation simulation, int resultCount, double threshold) {
        return simulation.state().entrySet().stream()
                .filter(entry -> entry.getValue() > threshold)
                .sorted(Map.Entry.<Neuron, Double>comparingByValue().reversed())
                .limit(resultCount)
                .map(entry -> new NeuronActivation(entry.getKey(), entry.getValue()))
                .toList();
    }

    int neuronCount() {
        return neuronsByBodyId.size();
    }

    int synapseCount() {
        return outgoingSynapses.values().stream().mapToInt(List::size).sum();
    }

    private static Map<Long, Neuron> loadNeurons(Path nodesFile) throws IOException {
        Map<Long, Neuron> neurons = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(nodesFile)) {
            String[] header = parseCsv(reader.readLine());
            int bodyIdColumn = column(header, "bodyid");
            int nameColumn = column(header, "name");
            int typeColumn = column(header, "type");
            int superclassColumn = column(header, "superclass");

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                String[] values = parseCsv(line);
                long bodyId = Long.parseLong(value(values, bodyIdColumn));
                neurons.put(bodyId, new Neuron(
                        bodyId,
                        value(values, nameColumn),
                        value(values, typeColumn),
                        value(values, superclassColumn)
                ));
            }
        }
        return neurons;
    }

    private static Map<Neuron, List<RawSynapse>> loadRawSynapses(
            Path edgesFile,
            Map<Long, Neuron> neurons
    ) throws IOException {
        Map<Neuron, List<RawSynapse>> rawSynapses = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(edgesFile)) {
            String[] header = parseCsv(reader.readLine());
            int sourceColumn = column(header, "source");
            int targetColumn = column(header, "target");
            int weightColumn = column(header, "weight");

            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) {
                    continue;
                }
                String[] values = parseCsv(line);
                Neuron source = neurons.get(Long.parseLong(value(values, sourceColumn)));
                Neuron target = neurons.get(Long.parseLong(value(values, targetColumn)));
                double weight = Double.parseDouble(value(values, weightColumn));
                if (source == null || target == null || !Double.isFinite(weight) || weight < 0) {
                    continue;
                }
                rawSynapses.computeIfAbsent(source, ignored -> new ArrayList<>())
                        .add(new RawSynapse(target, weight));
            }
        }
        return rawSynapses;
    }

    private static Map<Neuron, VisualInput> loadVisualInputs(
            Path file,
            Map<Long, Neuron> neurons
    ) throws IOException {
        Map<Neuron, VisualInput> result = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(file)) {
            String[] header = parseCsv(reader.readLine());
            int bodyId = column(header, "bodyid");
            int signal = column(header, "signal");
            int side = optionalColumn(header, "side");
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) continue;
                String[] values = parseCsv(line);
                Neuron neuron = neurons.get(Long.parseLong(value(values, bodyId)));
                if (neuron != null) {
                    result.put(neuron, new VisualInput(value(values, signal), value(values, side)));
                }
            }
        }
        return result;
    }

    private static Map<Neuron, MotorOutput> loadMotorOutputs(
            Path file,
            Map<Long, Neuron> neurons
    ) throws IOException {
        Map<Neuron, MotorOutput> result = new HashMap<>();
        try (BufferedReader reader = Files.newBufferedReader(file)) {
            String[] header = parseCsv(reader.readLine());
            int bodyId = column(header, "bodyid");
            int action = column(header, "action");
            int side = optionalColumn(header, "side");
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.isBlank()) continue;
                String[] values = parseCsv(line);
                Neuron neuron = neurons.get(Long.parseLong(value(values, bodyId)));
                if (neuron != null) {
                    result.put(neuron, new MotorOutput(value(values, action), value(values, side)));
                }
            }
        }
        return result;
    }

    private static Map<Neuron, List<Synapse>> normalizeSynapses(
            Map<Neuron, List<RawSynapse>> rawSynapses
    ) {
        Map<Neuron, List<Synapse>> normalized = new HashMap<>();
        for (Map.Entry<Neuron, List<RawSynapse>> entry : rawSynapses.entrySet()) {
            double totalWeight = entry.getValue().stream()
                    .mapToDouble(RawSynapse::weight)
                    .sum();
            List<Synapse> synapses = entry.getValue().stream()
                    .map(raw -> new Synapse(
                            entry.getKey(),
                            raw.target(),
                            totalWeight == 0 ? 0 : raw.weight() / totalWeight
                    ))
                    .toList();
            normalized.put(entry.getKey(), synapses);
        }
        return normalized;
    }

    private static int column(String[] header, String name) throws IOException {
        for (int i = 0; i < header.length; i++) {
            if (header[i].equalsIgnoreCase(name)) {
                return i;
            }
        }
        throw new IOException("Colonne CSV manquante: " + name);
    }

    private static int optionalColumn(String[] header, String name) {
        for (int i = 0; i < header.length; i++) {
            if (header[i].equalsIgnoreCase(name)) return i;
        }
        return -1;
    }

    private static String value(String[] values, int index) {
        return index >= 0 && index < values.length ? values[index].trim() : "";
    }

    private static String[] parseCsv(String line) {
        List<String> values = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean quoted = false;
        for (int i = 0; i < line.length(); i++) {
            char character = line.charAt(i);
            if (character == '"') {
                if (quoted && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    current.append('"');
                    i++;
                } else {
                    quoted = !quoted;
                }
            } else if (character == ',' && !quoted) {
                values.add(current.toString());
                current.setLength(0);
            } else {
                current.append(character);
            }
        }
        values.add(current.toString());
        return values.toArray(String[]::new);
    }

    private record RawSynapse(Neuron target, double weight) {
    }

    record SensorSnapshot(double obstacleFront, double obstacleLeft, double obstacleRight,
                          double opticFlow, double loomingCollision,
                          double objectTrackingLeft, double objectTrackingRight) {
        static SensorSnapshot none() {
            return new SensorSnapshot(0, 0, 0, 0, 0, 0, 0);
        }

        double value(String signal, String side) {
            return switch (signal.toUpperCase(Locale.ROOT)) {
                case "FIELD_OBSTACLE" -> sideValue(side);
                case "OPTIC_FLOW" -> opticFlow;
                case "LOOMING_COLLISION" -> loomingCollision;
                case "OBJECT_TRACKING" -> side.equalsIgnoreCase("L")
                        ? objectTrackingLeft
                        : side.equalsIgnoreCase("R") ? objectTrackingRight
                        : Math.max(objectTrackingLeft, objectTrackingRight);
                default -> 0;
            };
        }

        private double sideValue(String side) {
            if ("L".equalsIgnoreCase(side)) return obstacleLeft;
            if ("R".equalsIgnoreCase(side)) return obstacleRight;
            return obstacleFront;
        }
    }

    private record VisualInput(String signal, String side) {
    }

    private record MotorOutput(String action, String side) {
    }

    static final class Simulation {
        private final Neuron inputNeuron;
        private final Map<Neuron, Double> state;

        private Simulation(Neuron inputNeuron, Map<Neuron, Double> state) {
            this.inputNeuron = inputNeuron;
            this.state = state;
        }

        Neuron inputNeuron() {
            return inputNeuron;
        }

        Map<Neuron, Double> state() {
            return state;
        }
    }

    record MotorCommand(
            float turnDegrees,
            double forwardSpeed,
            double verticalSpeed,
            double wingBeatActivity
    ) {
        MotorCommand(double turnDegrees, double forwardSpeed) {
            this((float) turnDegrees, forwardSpeed, 0, 0);
        }
    }
}
