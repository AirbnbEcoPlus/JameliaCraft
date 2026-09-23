package fr.airbnbecoplus.jameliacraft;

import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.persistence.PersistentDataType;

import java.util.List;

final class JaneliaInspectorListener implements Listener {
    private final JaneliaCommand janeliaCommand;
    private final NamespacedKey inspectorKey;
    private final NamespacedKey npcKey;

    JaneliaInspectorListener(JaneliaCommand janeliaCommand, NamespacedKey inspectorKey, NamespacedKey npcKey) {
        this.janeliaCommand = janeliaCommand;
        this.inspectorKey = inspectorKey;
        this.npcKey = npcKey;
    }

    @EventHandler
    public void onEntityInteract(PlayerInteractEntityEvent event) {
        ItemStack item = event.getPlayer().getInventory().getItemInMainHand();
        if (item.getType() != Material.STICK
                || item.getItemMeta() == null
                || !item.getItemMeta().getPersistentDataContainer()
                .has(inspectorKey, PersistentDataType.BYTE)) {
            return;
        }

        Entity entity = event.getRightClicked();
        if (!entity.getPersistentDataContainer().has(npcKey, PersistentDataType.BYTE)) {
            event.getPlayer().sendMessage(Component.text("Ce n'est pas un PNJ Janelia."));
            return;
        }

        JaneliaNpcController controller = janeliaCommand.controllerFor(entity.getUniqueId());
        if (controller == null) {
            event.getPlayer().sendMessage(Component.text("Ce PNJ Janelia n'est plus contrôlé."));
            return;
        }

        event.setCancelled(true);
        ConnectomeNetwork.MotorCommand command = controller.lastCommand();
        event.getPlayer().sendMessage(Component.text("=== Informations Janelia ==="));
        event.getPlayer().sendMessage(Component.text("UUID: " + entity.getUniqueId()));
        event.getPlayer().sendMessage(Component.text("Type: " + entity.getType()));
        event.getPlayer().sendMessage(Component.text("Entrée réseau: bodyid " + controller.inputBodyId()));
        event.getPlayer().sendMessage(Component.text("Ticks simulés: " + controller.tickCount()));
        event.getPlayer().sendMessage(Component.text(
                "Neurones actifs (> 0.01): " + controller.activeNeuronCount()));
        event.getPlayer().sendMessage(Component.text(String.format(
                "Commande moteur: rotation %.2f°, vitesse %.4f, vertical %.4f, battement %.4f",
                command.turnDegrees(),
                command.forwardSpeed(),
                command.verticalSpeed(),
                command.wingBeatActivity())));
        event.getPlayer().sendMessage(Component.text(String.format(
                "Position: %.1f %.1f %.1f",
                entity.getLocation().getX(),
                entity.getLocation().getY(),
                entity.getLocation().getZ())));
        event.getPlayer().sendMessage(Component.text("Activations actuelles:"));
        List<fr.airbnbecoplus.jameliacraft.connectome.NeuronActivation> activations =
                controller.currentActivations(8);
        if (activations.isEmpty()) {
            event.getPlayer().sendMessage(Component.text(" - aucune activité au-dessus du seuil"));
        } else {
            activations.forEach(activation -> event.getPlayer().sendMessage(Component.text(String.format(
                    " - %s (%d): %.6f [%s/%s]",
                    activation.neuron().displayName(),
                    activation.neuron().bodyId(),
                    activation.value(),
                    activation.neuron().type(),
                    activation.neuron().superclass()))));
        }
    }
}
