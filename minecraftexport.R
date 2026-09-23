# ============================================================
# EXPORT CONNECTOME PROFONDEUR 2 (VISUEL -> DN)
# male-cns:v1.0 -> CSV Minecraft (Sans NeuroML)
# ============================================================

library(malecns)
library(dplyr)
library(Matrix)

Sys.setenv(NEUPRINT_TOKEN = "")
choose_mcns_dataset("male-cns:v1.0")

MIN_SYNAPSES <- 5

cat("========================================\n")
cat("1. RECHERCHE DES NEURONES\n")
cat("========================================\n")

vpn_meta <- mcns_neuprint_meta("/(LC10|LC4|LPLC2|LPTC|HS|VS)[0-9a-zA-Z_]*") %>%
  filter(!is.na(bodyid))
vpn_ids <- unique(vpn_meta$bodyid)

# Ajout de DNp09 (Marche avant continue)
dn_meta <- mcns_neuprint_meta("/(DNa01|DNa02|DNp01|DNp09|DNb01|DNb02|DNg02|DNp15)[0-9a-zA-Z_]*") %>%
  filter(!is.na(bodyid))
dn_ids <- unique(dn_meta$bodyid)

cat("Capteurs visuels :", length(vpn_ids), "\n")
cat("Neurones Moteurs :", length(dn_ids), "\n")

cat("\n========================================\n")
cat("2. TRACAGE CASCADE - PROFONDEUR 2\n")
cat("========================================\n")

# -- NIVEAU 1 : Sorties directes des capteurs --
cat("Extraction Niveau 1...\n")
edge_L1 <- mcns_connection_table(vpn_ids, partners = "outputs") %>% filter(weight >= MIN_SYNAPSES)
nodes_L1 <- unique(edge_L1$partner)

# -- NIVEAU 2 : Sorties du Niveau 1 --
cat("Extraction Niveau 2...\n")
edge_L2 <- mcns_connection_table(nodes_L1, partners = "outputs") %>% filter(weight >= MIN_SYNAPSES)
nodes_L2 <- unique(edge_L2$partner)

# -- ENTRÉES MOTEURS --
dn_inputs <- mcns_connection_table(dn_ids, partners = "inputs") %>% filter(weight >= MIN_SYNAPSES)
pre_dn_nodes <- unique(dn_inputs$partner)

# -- FILTRAGE DES CHEMINS VALIDES --
# Chemin à 1 saut (VPN -> L1 -> DN)
valid_L1_direct <- intersect(nodes_L1, pre_dn_nodes)

# Chemin à 2 sauts (VPN -> L1 -> L2 -> DN)
valid_L2 <- intersect(nodes_L2, pre_dn_nodes)
valid_L1_indirect <- edge_L2 %>% filter(partner %in% valid_L2) %>% pull(bodyid) %>% unique()

# Combiner tous les relais valides
all_valid_L1 <- unique(c(valid_L1_direct, valid_L1_indirect))
all_valid_L2 <- unique(valid_L2)

cat("Relais Niveau 1 valides :", length(all_valid_L1), "\n")
cat("Relais Niveau 2 valides :", length(all_valid_L2), "\n")

# Construction finale des arêtes
edges_vpn_L1 <- edge_L1 %>%
  filter(partner %in% c(all_valid_L1, dn_ids)) %>%
  transmute(source = bodyid, target = partner, weight, layer = "VPN_to_L1")

# CORRECTION ICI : on utilise 'bodyid' au lieu de 'source' pour le filtre
edges_L1_L2 <- edge_L2 %>%
  filter(bodyid %in% all_valid_L1, partner %in% c(all_valid_L2, dn_ids)) %>%
  transmute(source = bodyid, target = partner, weight, layer = "L1_to_L2")

edges_to_DN <- dn_inputs %>%
  filter(partner %in% c(all_valid_L1, all_valid_L2)) %>%
  transmute(source = partner, target = bodyid, weight, layer = "Relay_to_DN")

all_edges <- bind_rows(edges_vpn_L1, edges_L1_L2, edges_to_DN) %>%
  group_by(source, target) %>%
  summarise(weight = sum(weight), layer = paste(unique(layer), collapse=";"), .groups="drop") %>%
  group_by(source) %>%
  mutate(weight_norm = weight / sum(weight)) %>%
  ungroup()

connected_nodes <- unique(c(all_edges$source, all_edges$target))
nodes_meta <- mcns_neuprint_meta(ids = connected_nodes) %>%
  mutate(
    role = case_when(
      bodyid %in% vpn_ids ~ "INPUT_VISUAL",
      bodyid %in% dn_ids  ~ "OUTPUT_MOTOR",
      TRUE                ~ "INTERNEURON"
    ),
    superclass_export = ifelse(is.na(superclass) | superclass == "", "unknown", superclass)
  )

cat("Total neurones dans le circuit 2-sauts :", nrow(nodes_meta), "\n")

cat("\n========================================\n")
cat("3. EXPORTS POUR MINECRAFT (CSV)\n")
cat("========================================\n")

write.csv(nodes_meta, "minecraft_connectome_nodes.csv", row.names = FALSE)
write.csv(all_edges, "minecraft_connectome_edges.csv", row.names = FALSE)

visual_inputs <- nodes_meta %>% filter(role == "INPUT_VISUAL") %>%
  transmute(
    bodyid, name, type,
    signal = case_when(
      grepl("LC10", type)        ~ "OBJECT_TRACKING",
      grepl("LC4|LPLC2", type)   ~ "LOOMING_COLLISION",
      TRUE                       ~ "OPTIC_FLOW"
    ),
    side = ifelse(grepl("_R|right", name, ignore.case = TRUE), "R", "L")
  )
write.csv(visual_inputs, "visual_inputs.csv", row.names = FALSE)

motor_outputs <- nodes_meta %>% filter(role == "OUTPUT_MOTOR") %>%
  transmute(
    bodyid, name, type,
    action = case_when(
      grepl("DNp01|DNp09", type) ~ "FORWARD_ACCEL",
      grepl("DNa01|DNa02", type) ~ "STEERING",
      grepl("DNb01|DNb02", type) ~ "LIFT",
      grepl("DNg02|DNp15", type) ~ "WING_BEAT",
      TRUE                       ~ "UNMAPPED"
    ),
    side = ifelse(grepl("_R|right", name, ignore.case = TRUE), "R", "L")
  ) %>% filter(action != "UNMAPPED")
write.csv(motor_outputs, "motor_outputs.csv", row.names = FALSE)

cat("\nPipeline terminé avec succès. Les fichiers CSV sont générés.\n")