# JaneliaCraft

[English version](docs/README_EN.md)

## Français

### Description

JaneliaCraft est un plugin Minecraft qui permet de simuler une partie du
connectome du cerveau de la mouche du vinaigre (*Drosophila melanogaster*).

Le projet combine :

- un plugin Minecraft développé en Java ;
- un script R qui extrait les données du connectome ;
- une représentation du réseau neuronal sous forme de fichiers CSV.

### État du projet

Le projet est actuellement expérimental. Il simule une petite partie du
connectome, avec des entrées visuelles, des neurones intermédiaires et des
sorties motrices. L'objectif à terme est d'intégrer davantage de connexions.

### Prérequis

- Java 25 ;
- R et, éventuellement, RStudio ;
- les bibliothèques R `malecns`, `dplyr` et `Matrix` ;
- un accès à l'API neuPrint pour extraire les données du connectome.

### Générer les fichiers du connectome

Depuis la racine du projet, exécutez le script d'export :

```bash
Rscript minecraftexport.R
```

Le script génère les fichiers suivants :

- `minecraft_connectome_nodes.csv` ;
- `minecraft_connectome_edges.csv` ;
- `motor_outputs.csv` ;
- `visual_inputs.csv`.

### Lancer le serveur Minecraft

Sous Windows :

```powershell
.\gradlew.bat runServer
```

Sous Linux ou macOS :

```bash
./gradlew runServer
```

Avant de lancer le serveur, placez les quatre fichiers CSV générés dans :

```text
run/plugins/jameliacraft/
```

Le serveur démarre avec le plugin JaneliaCraft chargé automatiquement.

### Commandes Minecraft

La commande principale est :

```text
/janelia run [bodyid] [steps]
/janelia spawn
/janelia inspect
```

La permission `jameliacraft.run` est requise. Elle est accordée par défaut
aux opérateurs du serveur.
