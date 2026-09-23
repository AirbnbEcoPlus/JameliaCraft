package fr.airbnbecoplus.jameliacraft.connectome;

public record Neuron(long bodyId, String name, String type, String superclass) {
    public String displayName() {
        return name == null || name.isBlank() ? "Neuron " + bodyId : name;
    }
}
