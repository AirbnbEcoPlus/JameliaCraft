package fr.airbnbecoplus.jameliacraft.connectome;

public record Synapse(Neuron source, Neuron target, double weight) {
}
