package fr.airbnbecoplus.jameliacraft;

import fr.airbnbecoplus.jameliacraft.connectome.NeuronActivation;
import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.entity.Villager;
import org.bukkit.util.Vector;
import org.bukkit.block.Block;

import java.util.UUID;
import java.util.List;

final class JaneliaNpcController {
    private final ConnectomeNetwork network;
    private final UUID entityId;
    private final ConnectomeNetwork.Simulation simulation;
    private long tickCount;
    private ConnectomeNetwork.MotorCommand lastCommand =
            new ConnectomeNetwork.MotorCommand(0, 0);

    JaneliaNpcController(ConnectomeNetwork network, Villager villager, long inputBodyId) {
        this.network = network;
        this.entityId = villager.getUniqueId();
        this.simulation = network.createSimulation(inputBodyId);
    }

    boolean tick(Entity entity) {
        if (!entity.isValid()) {
            return false;
        }

        ConnectomeNetwork.SensorSnapshot sensors = sense(entity);
        ConnectomeNetwork.MotorCommand command = network.step(simulation, sensors);
        lastCommand = command;
        tickCount++;
        Location location = entity.getLocation().clone();
        location.setYaw(location.getYaw() + command.turnDegrees());
        entity.teleport(location);
        Vector velocity = location.getDirection().multiply(command.forwardSpeed());
        velocity.setY(command.verticalSpeed() > 0
                ? command.verticalSpeed()
                : entity.getVelocity().getY());
        entity.setVelocity(velocity);
        return true;
    }

    private ConnectomeNetwork.SensorSnapshot sense(Entity entity) {
        Location location = entity.getLocation();
        Vector forward = location.getDirection().setY(0);
        if (forward.lengthSquared() == 0) {
            forward = new Vector(0, 0, 1);
        } else {
            forward.normalize();
        }
        Vector left = new Vector(-forward.getZ(), 0, forward.getX());
        double front = obstacle(entity, forward);
        double leftValue = obstacle(entity, left);
        double rightValue = obstacle(entity, left.clone().multiply(-1));
        double[] playerTracking = visiblePlayerSignal(entity, forward, left);
        return new ConnectomeNetwork.SensorSnapshot(
                front,
                leftValue,
                rightValue,
                0,
                front,
                playerTracking[0],
                playerTracking[1]
        );
    }

    private double[] visiblePlayerSignal(Entity entity, Vector forward, Vector left) {
        Player closest = null;
        double closestDistance = 12.0;
        for (Player player : entity.getWorld().getPlayers()) {
            if (!player.isOnline() || player.getUniqueId().equals(entity.getUniqueId())) {
                continue;
            }
            double distance = player.getLocation().distance(entity.getLocation());
            if (distance < closestDistance && entity instanceof org.bukkit.entity.LivingEntity living
                    && living.hasLineOfSight(player)) {
                closest = player;
                closestDistance = distance;
            }
        }
        if (closest == null) {
            return new double[]{0, 0};
        }

        Vector toPlayer = closest.getLocation().toVector()
                .subtract(entity.getLocation().toVector())
                .setY(0);
        if (toPlayer.lengthSquared() == 0) {
            return new double[]{1, 1};
        }
        toPlayer.normalize();
        double intensity = Math.max(0, 1.0 - closestDistance / 12.0);
        double side = toPlayer.dot(left);
        if (side >= 0) {
            return new double[]{intensity, 0};
        }
        return new double[]{0, intensity};
    }

    private double obstacle(Entity entity, Vector direction) {
        Location point = entity.getLocation().clone().add(0, 0.8, 0);
        Block block = point.getWorld().getBlockAt(point.clone().add(direction.multiply(1.5)));
        return block.getType().isSolid() ? 1.0 : 0.0;
    }

    UUID entityId() {
        return entityId;
    }

    long tickCount() {
        return tickCount;
    }

    ConnectomeNetwork.MotorCommand lastCommand() {
        return lastCommand;
    }

    List<NeuronActivation> currentActivations(int count) {
        return network.currentActivations(simulation, count, 0.01);
    }

    long activeNeuronCount() {
        return simulation.state().values().stream()
                .filter(value -> value > 0.01)
                .count();
    }

    long inputBodyId() {
        return simulation.inputNeuron().bodyId();
    }
}
