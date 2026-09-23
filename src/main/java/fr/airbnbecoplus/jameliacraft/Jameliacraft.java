package fr.airbnbecoplus.jameliacraft;

import org.bukkit.plugin.java.JavaPlugin;

public final class Jameliacraft extends JavaPlugin {
    private JaneliaCommand janeliaCommand;

    @Override
    public void onEnable() {
        saveDefaultConfig();
        janeliaCommand = new JaneliaCommand(this);
        if (getCommand("janelia") == null) {
            throw new IllegalStateException("La commande janelia n'est pas déclarée dans plugin.yml");
        }
        getCommand("janelia").setExecutor(janeliaCommand);
        getServer().getPluginManager().registerEvents(
                new JaneliaInspectorListener(janeliaCommand,
                        janeliaCommand.inspectorKey(),
                        janeliaCommand.npcKey()),
                this);
        getLogger().info("JameliaCraft activé. Utilise /jamelia run.");
    }

    @Override
    public void onDisable() {
        if (janeliaCommand != null) {
            janeliaCommand.shutdown();
        }
    }
}
