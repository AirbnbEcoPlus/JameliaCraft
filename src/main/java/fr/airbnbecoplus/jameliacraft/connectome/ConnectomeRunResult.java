package fr.airbnbecoplus.jameliacraft.connectome;

import java.util.List;

public record ConnectomeRunResult(
        Neuron inputNeuron,
        int steps,
        List<NeuronActivation> topActivations,
        long activeNeuronCount
) {
}
