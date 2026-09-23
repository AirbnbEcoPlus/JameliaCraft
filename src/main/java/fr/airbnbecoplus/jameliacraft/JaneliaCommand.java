package fr.airbnbecoplus.jameliacraft;

import fr.airbnbecoplus.jameliacraft.connectome.ConnectomeRunResult;
import net.kyori.adventure.text.Component;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.entity.Villager;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.persistence.PersistentDataType;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

final class JaneliaCommand implements CommandExecutor {
    private final JavaPlugin plugin;
    private final ExecutorService executor = Executors.newSingleThreadExecutor();
    private final Map<UUID, JaneliaNpcController> controllers = new ConcurrentHashMap<>();
    private final NamespacedKey npcKey;
    private final NamespacedKey inspectorKey;
    private volatile ConnectomeNetwork network;
    private int movementTask = -1;

    JaneliaCommand(JavaPlugin plugin) {
        this.plugin = plugin;
        this.npcKey = new NamespacedKey(plugin, "janelia_npc");
        this.inspectorKey = new NamespacedKey(plugin, "janelia_inspector");
    }

    void shutdown() {
        executor.shutdownNow();
        if (movementTask != -1) {
            plugin.getServer().getScheduler().cancelTask(movementTask);
        }
        controllers.clear();
    }

    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (args.length == 0 || args[0].equalsIgnoreCase("help")) {
            sender.sendMessage(Component.text("/jamelia run [bodyid] [steps] ou /jamelia spawn"));
            return true;
        }
        if (args[0].equalsIgnoreCase("spawn")) {
            spawnNpc(sender);
            return true;
        }
        if (args[0].equalsIgnoreCase("inspect")) {
            giveInspector(sender);
            return true;
        }
        if (!args[0].equalsIgnoreCase("run")) {
            sender.sendMessage(Component.text("Commande inconnue. Utilise /jamelia run [bodyid] [steps]."));
            return true;
        }

        long bodyId;
        int steps;
        try {
            bodyId = args.length > 1 ? Long.parseLong(args[1]) : 13483;
            steps = args.length > 2 ? Integer.parseInt(args[2]) : 20;
            if (steps < 1 || steps > 1000) {
                throw new NumberFormatException();
            }
        } catch (NumberFormatException exception) {
            sender.sendMessage(Component.text("Arguments invalides: bodyid doit être un entier et steps doit être entre 1 et 1000."));
            return true;
        }

        sender.sendMessage(Component.text("Janelia: chargement/exécution du réseau en arrière-plan..."));
        CompletableFuture
                .supplyAsync(this::loadNetwork, executor)
                .thenApplyAsync(loaded -> loaded.run(bodyId, steps, 0.9, 0.01,
                        plugin.getConfig().getInt("top-results", 10)), executor)
                .whenComplete((result, error) -> plugin.getServer().getScheduler().runTask(plugin, () -> {
                    if (error != null) {
                        Throwable cause = error instanceof java.util.concurrent.CompletionException
                                ? error.getCause() : error;
                        sender.sendMessage(Component.text("Janelia: échec: " + cause.getMessage()));
                        plugin.getLogger().warning("Impossible d'exécuter le connectome: " + cause);
                        return;
                    }
                    sendResult(sender, result);
                }));
        return true;
    }

    JaneliaNpcController controllerFor(UUID entityId) {
        return controllers.get(entityId);
    }

    NamespacedKey npcKey() {
        return npcKey;
    }

    NamespacedKey inspectorKey() {
        return inspectorKey;
    }

    private void giveInspector(CommandSender sender) {
        if (!(sender instanceof Player player)) {
            sender.sendMessage(Component.text("Cette commande doit être exécutée par un joueur."));
            return;
        }
        ItemStack stick = new ItemStack(Material.STICK);
        ItemMeta meta = stick.getItemMeta();
        meta.displayName(Component.text("Inspecteur Janelia"));
        meta.getPersistentDataContainer().set(inspectorKey, PersistentDataType.BYTE, (byte) 1);
        stick.setItemMeta(meta);
        player.getInventory().addItem(stick);
        player.sendMessage(Component.text("Bâton d'inspection reçu. Fais un clic droit sur un PNJ Janelia."));
    }

    private void spawnNpc(CommandSender sender) {
        if (!(sender instanceof Player player)) {
            sender.sendMessage(Component.text("La commande spawn doit être exécutée par un joueur."));
            return;
        }
        sender.sendMessage(Component.text("Janelia: chargement du réseau avant le spawn..."));
        CompletableFuture
                .supplyAsync(this::loadNetwork, executor)
                .whenComplete((loaded, error) -> plugin.getServer().getScheduler().runTask(plugin, () -> {
                    if (error != null) {
                        sender.sendMessage(Component.text("Janelia: échec: " + error.getMessage()));
                        return;
                    }
                    Villager villager = (Villager) player.getWorld().spawnEntity(
                            player.getLocation(), EntityType.VILLAGER);
                    villager.setAI(false);
                    villager.setInvulnerable(false);
                    villager.setGravity(true);
                    villager.setCustomName("Janelia");
                    villager.setCustomNameVisible(true);
                    villager.getPersistentDataContainer().set(npcKey, PersistentDataType.BYTE, (byte) 1);
                    controllers.put(villager.getUniqueId(),
                            new JaneliaNpcController(loaded, villager, 13483));
                    ensureMovementTask();
                    sender.sendMessage(Component.text(
                            "Janelia: PNJ créé. Il utilise les entrées visuelles et les sorties motrices exportées."));
                }));
    }

    private void ensureMovementTask() {
        if (movementTask != -1) {
            return;
        }
        movementTask = plugin.getServer().getScheduler().scheduleSyncRepeatingTask(plugin, () -> {
            controllers.entrySet().removeIf(entry -> {
                Entity entity = plugin.getServer().getEntity(entry.getKey());
                return entity == null || !entry.getValue().tick(entity);
            });
            if (controllers.isEmpty()) {
                plugin.getServer().getScheduler().cancelTask(movementTask);
                movementTask = -1;
            }
        }, 1L, 1L);
    }

    private ConnectomeNetwork loadNetwork() {
        if (network != null) {
            return network;
        }
        Path directory = Path.of(plugin.getConfig().getString("network-directory", "."));
        String nodesFile = plugin.getConfig().getString("nodes-file");
        String edgesFile = plugin.getConfig().getString("edges-file");
        String visualInputsFile = plugin.getConfig().getString("visual-inputs-file");
        String motorOutputsFile = plugin.getConfig().getString("motor-outputs-file");
        List<Path> candidates = new ArrayList<>();
        candidates.add(directory);
        candidates.add(directory.resolve("..").normalize());
        candidates.add(plugin.getDataFolder().toPath());
        Path nodes = null;
        Path edges = null;
        Path visualInputs = null;
        Path motorOutputs = null;
        for (Path candidate : candidates) {
            Path candidateNodes = candidate.resolve(nodesFile);
            Path candidateEdges = candidate.resolve(edgesFile);
            if (Files.isRegularFile(candidateNodes) && Files.isRegularFile(candidateEdges)) {
                nodes = candidateNodes;
                edges = candidateEdges;
                Path candidateVisual = candidate.resolve(visualInputsFile);
                Path candidateMotor = candidate.resolve(motorOutputsFile);
                visualInputs = Files.isRegularFile(candidateVisual) ? candidateVisual : null;
                motorOutputs = Files.isRegularFile(candidateMotor) ? candidateMotor : null;
                break;
            }
        }
        if (nodes == null || edges == null) {
            throw new IllegalStateException("CSV introuvables. Configure network-directory dans "
                    + plugin.getDataFolder().toPath().resolve("config.yml"));
        }
        try {
            network = ConnectomeNetwork.load(nodes, edges, visualInputs, motorOutputs);
            plugin.getLogger().info("Connectome chargé: " + network.neuronCount() + " neurones, "
                    + network.synapseCount() + " synapses.");
            return network;
        } catch (IOException | RuntimeException exception) {
            throw new IllegalStateException("fichiers introuvables ou CSV invalide (" + nodes + ", " + edges + ")", exception);
        }
    }

    private void sendResult(CommandSender sender, ConnectomeRunResult result) {
        sender.sendMessage(Component.text("Janelia: " + result.activeNeuronCount() + " neurones actifs après "
                + result.steps() + " étapes (entrée: " + result.inputNeuron().displayName()
                + ", bodyid " + result.inputNeuron().bodyId() + ")."));
        if (result.topActivations().isEmpty()) {
            sender.sendMessage(Component.text("Aucun neurone au-dessus du seuil."));
            return;
        }
        sender.sendMessage(Component.text("Top activations:"));
        result.topActivations().forEach(activation -> sender.sendMessage(Component.text(String.format(
                " - %d %-24s %.6f [%s/%s]",
                activation.neuron().bodyId(),
                activation.neuron().displayName(),
                activation.value(),
                activation.neuron().type(),
                activation.neuron().superclass()))));
    }
}
