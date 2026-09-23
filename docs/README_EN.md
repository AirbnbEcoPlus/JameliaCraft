# JaneliaCraft

[Version française](../README.md)

## Description

JaneliaCraft is a Minecraft plugin that simulates part of the connectome of
the fruit fly (*Drosophila melanogaster*) brain of Janelia Research Campus.

The project combines:

- a Java-based Minecraft plugin;
- an R script used to extract connectome data;
- a neural-network representation stored in CSV files.

## Project status

The project is currently experimental. It simulates a small part of the
connectome, including visual inputs, interneurons, and motor outputs. The
long-term goal is to integrate more connections.

## Requirements

- Java 25;
- R and, optionally, RStudio;
- the R packages `malecns`, `dplyr`, and `Matrix`;
- access to the neuPrint API to extract connectome data.

## Generate the connectome files

From the project root, run the export script:

```bash
Rscript minecraftexport.R
```

The script generates the following files:

- `minecraft_connectome_nodes.csv`;
- `minecraft_connectome_edges.csv`;
- `motor_outputs.csv`;
- `visual_inputs.csv`.

## Start the Minecraft server

On Windows:

```powershell
.\gradlew.bat runServer
```

On Linux or macOS:

```bash
./gradlew runServer
```

Before starting the server, place the four generated CSV files in:

```text
run/plugins/jameliacraft/
```

The server automatically starts with the JaneliaCraft plugin loaded.

## Minecraft commands

The main command is:

```text
/janelia run [bodyid] [steps]
/janelia spawn
/janelia inspect
```

The `jameliacraft.run` permission is required. It is granted by default to
server operators.
