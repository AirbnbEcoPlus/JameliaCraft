# ============================================================
# MALE CNS CONNECTOME
# 219 PN -> DN -> DOWNSTREAM
# EXPORT NeuroML 2.3 (.nml)
# ============================================================

library(malecns)
library(dplyr)
library(Matrix)

# ============================================================
# 1. DATASET
# ============================================================

Sys.setenv(NEUPRINT_TOKEN ="7200c9c723b6d64d3e817e36196351af0114622a507898921247c6e4c9a3a427")

choose_mcns_dataset("male-cns:v1.0")

dr_malecns()


# ============================================================
# 2. RECUPERER TOUS LES PN
# ============================================================

pn_meta <- mcns_neuprint_meta(
  "/.+_[adl]+PN"
)

cat(
  "Nombre total de PN :",
  nrow(pn_meta),
  "\n"
)

pn_ids <- unique(
  pn_meta$bodyid
)

cat(
  "Nombre de bodyid PN uniques :",
  length(pn_ids),
  "\n"
)


# ============================================================
# 3. CONSTRUCTION DU CONNECTOME
# ============================================================

build_connectome <- function(pn_ids) {
  
  cat("\n")
  cat("========================================\n")
  cat("CONSTRUCTION DU CONNECTOME\n")
  cat("========================================\n")
  
  cat(
    "PN utilisés :",
    length(pn_ids),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # PN -> tous les partenaires
  # ----------------------------------------------------------
  
  cat(
    "\nRécupération des sorties des PN...\n"
  )
  
  pn_outputs <- mcns_connection_table(
    pn_ids,
    partners = "outputs"
  )
  
  cat(
    "Connexions PN trouvées :",
    nrow(pn_outputs),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # PN -> DN
  # ----------------------------------------------------------
  
  dn_outputs <- pn_outputs[
    !is.na(pn_outputs$superclass) &
      pn_outputs$superclass == "descending_neuron",
  ]
  
  
  cat(
    "Connexions PN -> DN :",
    nrow(dn_outputs),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Tous les DN atteints
  # ----------------------------------------------------------
  
  dn_ids <- unique(
    dn_outputs$partner
  )
  
  
  cat(
    "DN uniques :",
    length(dn_ids),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # DN -> downstream
  # ----------------------------------------------------------
  
  cat(
    "\nRécupération des sorties des DN...\n"
  )
  
  dn_connections <- mcns_connection_table(
    dn_ids,
    partners = "outputs"
  )
  
  
  cat(
    "Connexions DN -> downstream :",
    nrow(dn_connections),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # CONSTRUIRE LES ARETES
  # ----------------------------------------------------------
  
  edges <- bind_rows(
    
    dn_outputs %>%
      transmute(
        
        source = bodyid,
        
        target = partner,
        
        weight = weight,
        
        connection_type = "PN_to_DN"
        
      ),
    
    dn_connections %>%
      transmute(
        
        source = bodyid,
        
        target = partner,
        
        weight = weight,
        
        connection_type = "DN_to_downstream"
        
      )
    
  )
  
  
  # ----------------------------------------------------------
  # Nettoyage
  # ----------------------------------------------------------
  
  edges <- edges %>%
    
    filter(
      
      !is.na(source),
      
      !is.na(target),
      
      !is.na(weight)
      
    )
  
  
  # ----------------------------------------------------------
  # Fusion des doublons
  #
  # IMPORTANT :
  # on additionne les synapses si la même paire
  # source -> target apparaît plusieurs fois.
  # ----------------------------------------------------------
  
  edges <- edges %>%
    
    group_by(
      source,
      target
    ) %>%
    
    summarise(
      
      weight = sum(
        weight,
        na.rm = TRUE
      ),
      
      connection_type = paste(
        unique(connection_type),
        collapse = ";"
      ),
      
      .groups = "drop"
      
    )
  
  
  # ----------------------------------------------------------
  # Tous les neurones présents dans le graphe
  # ----------------------------------------------------------
  
  node_ids <- unique(
    
    c(
      edges$source,
      edges$target
    )
    
  )
  
  
  cat(
    "\nNeurones dans le graphe :",
    length(node_ids),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # METADONNEES
  # ----------------------------------------------------------
  
  cat(
    "Récupération des métadonnées...\n"
  )
  
  nodes <- mcns_neuprint_meta(
    ids = node_ids
  )
  
  
  # ----------------------------------------------------------
  # Marqueurs PN / DN
  # ----------------------------------------------------------
  
  nodes <- nodes %>%
    
    mutate(
      
      is_pn =
        bodyid %in% pn_ids,
      
      is_dn =
        bodyid %in% dn_ids
      
    )
  
  
  # ----------------------------------------------------------
  # Retour
  # ----------------------------------------------------------
  
  list(
    
    nodes = nodes,
    
    edges = edges,
    
    pn_ids = pn_ids,
    
    dn_ids = dn_ids
    
  )
  
}


# ============================================================
# 4. CONSTRUIRE LE CONNECTOME
# ============================================================

connectome <- build_connectome(
  pn_ids
)


# ============================================================
# 5. STATISTIQUES
# ============================================================

cat("\n")
cat("========================================\n")
cat("STATISTIQUES CONNECTOME\n")
cat("========================================\n")

cat(
  "PN :",
  length(connectome$pn_ids),
  "\n"
)

cat(
  "DN :",
  length(connectome$dn_ids),
  "\n"
)

cat(
  "Neurones totaux :",
  nrow(connectome$nodes),
  "\n"
)

cat(
  "Connexions totales :",
  nrow(connectome$edges),
  "\n"
)

cat(
  "Nombre total de synapses :",
  sum(connectome$edges$weight),
  "\n"
)


# ============================================================
# 6. SUPERCLASSES
# ============================================================

cat("\n")
cat("Neurones par superclass :\n")

print(
  sort(
    table(
      connectome$nodes$superclass
    ),
    decreasing = TRUE
  )
)


# ============================================================
# 7. MATRICES DE CONNECTIVITE
# ============================================================

build_weight_matrices <- function(
    nodes,
    edges
) {
  
  node_index <- setNames(
    
    seq_len(
      nrow(nodes)
    ),
    
    nodes$bodyid
    
  )
  
  
  # ----------------------------------------------------------
  # Poids biologiques bruts
  # ----------------------------------------------------------
  
  W_raw <- sparseMatrix(
    
    i = node_index[
      as.character(
        edges$target
      )
    ],
    
    j = node_index[
      as.character(
        edges$source
      )
    ],
    
    x = edges$weight,
    
    dims = c(
      nrow(nodes),
      nrow(nodes)
    )
    
  )
  
  
  # ----------------------------------------------------------
  # Log
  # ----------------------------------------------------------
  
  edges$weight_log <-
    log1p(
      edges$weight
    )
  
  
  W_log <- sparseMatrix(
    
    i = node_index[
      as.character(
        edges$target
      )
    ],
    
    j = node_index[
      as.character(
        edges$source
      )
    ],
    
    x = edges$weight_log,
    
    dims = c(
      nrow(nodes),
      nrow(nodes)
    )
    
  )
  
  
  # ----------------------------------------------------------
  # Normalisation computationnelle
  #
  # PAS utilisée pour le fichier NeuroML biologique.
  # ----------------------------------------------------------
  
  edges$weight_norm <-
    
    edges$weight /
    
    ave(
      edges$weight,
      edges$source,
      FUN = sum
    )
  
  
  W_norm <- sparseMatrix(
    
    i = node_index[
      as.character(
        edges$target
      )
    ],
    
    j = node_index[
      as.character(
        edges$source
      )
    ],
    
    x = edges$weight_norm,
    
    dims = c(
      nrow(nodes),
      nrow(nodes)
    )
    
  )
  
  
  list(
    
    raw = W_raw,
    
    log = W_log,
    
    norm = W_norm,
    
    edges = edges
    
  )
  
}


weights <- build_weight_matrices(
  
  connectome$nodes,
  
  connectome$edges
  
)


W <- weights$norm


# ============================================================
# 8. FONCTIONS UTILITAIRES XML
# ============================================================

xml_escape <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "&",
    "&amp;",
    x,
    fixed = TRUE
  )
  
  x <- gsub(
    "<",
    "&lt;",
    x,
    fixed = TRUE
  )
  
  x <- gsub(
    ">",
    "&gt;",
    x,
    fixed = TRUE
  )
  
  x <- gsub(
    '"',
    "&quot;",
    x,
    fixed = TRUE
  )
  
  x <- gsub(
    "'",
    "&apos;",
    x,
    fixed = TRUE
  )
  
  x
  
}


# ============================================================
# 9. IDENTIFIANTS NeuroML
# ============================================================

sanitize_id <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "[^A-Za-z0-9_.-]",
    "_",
    x
  )
  
  # Un identifiant XML/NeuroML ne doit pas commencer
  # par un caractère problématique.
  
  x <- ifelse(
    grepl(
      "^[A-Za-z_]",
      x
    ),
    x,
    paste0(
      "n_",
      x
    )
  )
  
  x
  
}


# ============================================================
# 10. CREER UN ID DE POPULATION
# ============================================================

make_population_id <- function(
    superclass
) {
  
  if (
    is.na(superclass) ||
    superclass == ""
  ) {
    
    return(
      "population_unknown"
    )
    
  }
  
  paste0(
    
    "population_",
    
    sanitize_id(
      superclass
    )
    
  )
  
}


# ============================================================
# 11. CREATION DU FICHIER NeuroML
# ============================================================

export_neuroml <- function(
    
  connectome,
  
  file = "male_cns_219PN.nml"
  
) {
  
  
  nodes <- connectome$nodes
  
  edges <- connectome$edges
  
  
  cat("\n")
  cat("========================================\n")
  cat("EXPORT NeuroML 2.3\n")
  cat("========================================\n")
  
  
  # ----------------------------------------------------------
  # Nettoyage superclass
  # ----------------------------------------------------------
  
  nodes$superclass_export <-
    
    ifelse(
      
      is.na(nodes$superclass) |
        nodes$superclass == "",
      
      "unknown",
      
      nodes$superclass
      
    )
  
  
  # ----------------------------------------------------------
  # POPULATIONS
  #
  # On crée une population par superclass.
  # ----------------------------------------------------------
  
  population_names <- unique(
    
    nodes$superclass_export
    
  )
  
  
  population_id <- setNames(
    
    vapply(
      
      population_names,
      
      make_population_id,
      
      character(1)
      
    ),
    
    population_names
    
  )
  
  
  # ----------------------------------------------------------
  # Index global de chaque neurone
  #
  # NeuroML utilise population[index] dans les références.
  # ----------------------------------------------------------
  
  node_index <- setNames(
    
    seq_len(
      nrow(nodes)
    ) - 1L,
    
    as.character(
      nodes$bodyid
    )
    
  )
  
  
  # ----------------------------------------------------------
  # Index local dans chaque population
  # ----------------------------------------------------------
  
  nodes$local_index <- NA_integer_
  
  
  for (
    pop_name in population_names
  ) {
    
    idx <- which(
      
      nodes$superclass_export ==
        pop_name
      
    )
    
    nodes$local_index[idx] <-
      
      seq_along(idx) - 1L
    
  }
  
  
  # ----------------------------------------------------------
  # Population pour chaque bodyid
  # ----------------------------------------------------------
  
  bodyid_to_population <-
    
    setNames(
      
      population_id[
        nodes$superclass_export
      ],
      
      as.character(
        nodes$bodyid
      )
      
    )
  
  
  bodyid_to_local_index <-
    
    setNames(
      
      nodes$local_index,
      
      as.character(
        nodes$bodyid
      )
      
    )
  
  
  # ----------------------------------------------------------
  # DEBUT DU XML
  # ----------------------------------------------------------
  
  xml <- c(
    
    '<?xml version="1.0" encoding="UTF-8"?>',
    
    '<neuroml',
    
    ' xmlns="http://www.neuroml.org/schema/neuroml2"',
    
    ' xmlns:xs="http://www.w3.org/2001/XMLSchema"',
    
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
    
    ' xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2',
    
    ' https://raw.githubusercontent.com/NeuroML/NeuroML2/development/Schemas/NeuroML2/NeuroML_v2.3.xsd"',
    
    ' id="MaleCNS_219PN">'
    
  )
  
  
  # ----------------------------------------------------------
  # NOTES
  # ----------------------------------------------------------
  
  xml <- c(
    
    xml,
    
    "  <notes>",
    
    "    Male Drosophila CNS connectome extracted with malecns.",
    
    paste0(
      "    PN count: ",
      length(connectome$pn_ids)
    ),
    
    paste0(
      "    DN count: ",
      length(connectome$dn_ids)
    ),
    
    paste0(
      "    Total neurons: ",
      nrow(nodes)
    ),
    
    paste0(
      "    Total connections: ",
      nrow(edges)
    ),
    
    "    Connection weight = biological synapse count.",
    
    "    Computational normalization is intentionally not stored here.",
    
    "  </notes>"
    
  )
  
  
  # ==========================================================
  # CELL MODEL
  # ==========================================================
  
  # Generic IAF cell.
  #
  # Ce n'est PAS une affirmation biologique sur les neurones
  # de Drosophile.
  #
  # C'est un modèle computationnel générique permettant
  # à jNeuroML de disposer d'un type de cellule.
  # ==========================================================
  
  xml <- c(
    
    xml,
    
    "",
    
    '  <iafCell',
    
    '    id="genericIAF"',
    
    '    leakReversal="-65mV"',
    
    '    thresh="-50mV"',
    
    '    reset="-65mV"',
    
    '    C="100pF"',
    
    '    leakConductance="10nS"/>'
    
  )
  
  
  # ==========================================================
  # SYNAPSE
  # ==========================================================
  
  # Synapse excitatrice générique.
  #
  # Le connectome malecns ne donne pas ici directement
  # une conductance/reversal/delay biologiquement calibrés.
  #
  # Les paramètres ci-dessous sont donc des paramètres
  # COMPUTATIONNELS provisoires.
  # ==========================================================
  
  xml <- c(
    
    xml,
    
    "",
    
    '  <expTwoSynapse',
    
    '    id="genericSynapse"',
    
    '    gbase="1nS"',
    
    '    erev="0mV"',
    
    '    tauRise="1ms"',
    
    '    tauDecay="5ms"/>'
    
  )
  
  
  # ==========================================================
  # NETWORK
  # ==========================================================
  
  xml <- c(
    
    xml,
    
    "",
    
    '  <network',
    
    '    id="MaleCNSNetwork">'
    
  )
  
  
  # ==========================================================
  # POPULATIONS
  # ==========================================================
  
  cat(
    "Création des populations...\n"
  )
  
  
  for (
    pop_name in population_names
  ) {
    
    
    idx <- which(
      
      nodes$superclass_export ==
        pop_name
      
    )
    
    
    pop_id <- population_id[
      pop_name
    ]
    
    
    pop_size <- length(
      idx
    )
    
    
    xml <- c(
      
      xml,
      
      paste0(
        
        '    <population id="',
        
        xml_escape(pop_id),
        
        '" component="genericIAF" size="',
        
        pop_size,
        
        '" type="population">'
        
      )
      
    )
    
    
    # --------------------------------------------------------
    # Metadata de population
    # --------------------------------------------------------
    
    xml <- c(
      
      xml,
      
      paste0(
        
        '      <property tag="superclass" value="',
        
        xml_escape(pop_name),
        
        '"/>'
        
      )
      
    )
    
    
    # --------------------------------------------------------
    # Instances
    #
    # On conserve le bodyid comme attribut "id" de l'instance.
    #
    # Le mapping complet bodyid -> population/index est également
    # exporté dans le CSV.
    # --------------------------------------------------------
    
    for (
      k in seq_along(idx)
    ) {
      
      node <- nodes[
        idx[k],
      ]
      
      bodyid <- node$bodyid
      
      xml <- c(
        
        xml,
        
        paste0(
          
          '      <instance id="',
          
          xml_escape(
            as.character(bodyid)
          ),
          
          '">'
          
        ),
        
        '        <location x="0" y="0" z="0"/>',
        
        "      </instance>"
        
      )
      
    }
    
    
    xml <- c(
      
      xml,
      
      "    </population>"
      
    )
    
  }
  
  
  # ==========================================================
  # PROJECTIONS
  # ==========================================================
  
  cat(
    "Création des projections...\n"
  )
  
  
  # ----------------------------------------------------------
  # NeuroML Projection est définie entre deux populations.
  #
  # Donc on regroupe les connexions par :
  #
  # source population
  # target population
  #
  # ----------------------------------------------------------
  
  edges$source_population <-
    
    unname(
      
      bodyid_to_population[
        as.character(
          edges$source
        )
      ]
      
    )
  
  
  edges$target_population <-
    
    unname(
      
      bodyid_to_population[
        as.character(
          edges$target
        )
      ]
      
    )
  
  
  # Retirer les connexions dont les neurones n'ont
  # pas de population valide.
  
  edges_export <- edges %>%
    
    filter(
      
      !is.na(source_population),
      
      !is.na(target_population)
      
    )
  
  
  projection_groups <- split(
    
    edges_export,
    
    list(
      
      edges_export$source_population,
      
      edges_export$target_population
      
    ),
    
    drop = TRUE
    
  )
  
  
  projection_counter <- 0L
  
  
  for (
    group_edges in projection_groups
  ) {
    
    
    if (
      nrow(group_edges) == 0
    ) {
      
      next
      
    }
    
    
    source_pop <-
      
      unique(
        group_edges$source_population
      )[1]
    
    
    target_pop <-
      
      unique(
        group_edges$target_population
      )[1]
    
    
    projection_counter <-
      
      projection_counter + 1L
    
    
    projection_id <- paste0(
      
      "projection_",
      
      projection_counter
      
    )
    
    
    xml <- c(
      
      xml,
      
      "",
      
      paste0(
        
        '    <projection id="',
        
        projection_id,
        
        '" presynapticPopulation="',
        
        xml_escape(source_pop),
        
        '" postsynapticPopulation="',
        
        xml_escape(target_pop),
        
        '" synapse="genericSynapse">'
        
      )
      
    )
    
    
    # --------------------------------------------------------
    # Connexions
    # --------------------------------------------------------
    
    for (
      i in seq_len(
        nrow(group_edges)
      )
    ) {
      
      
      edge <- group_edges[
        i,
      ]
      
      
      source_id <-
        
        as.character(
          edge$source
        )
      
      
      target_id <-
        
        as.character(
          edge$target
        )
      
      
      source_index <-
        
        bodyid_to_local_index[
          source_id
        ]
      
      
      target_index <-
        
        bodyid_to_local_index[
          target_id
        ]
      
      
      # NeuroML weight = nombre de synapses.
      weight_value <-
        
        as.numeric(
          edge$weight
        )
      
      
      # ------------------------------------------------------
      # Delay
      #
      # Le connectome extrait ici ne fournit pas un délai
      # synaptique exploitable.
      #
      # On utilise donc 1 ms comme paramètre computationnel
      # provisoire.
      # ------------------------------------------------------
      
      delay_value <- "1ms"
      
      
      xml <- c(
        
        xml,
        
        paste0(
          
          '      <connectionWD',
          
          ' id="',
          
          i - 1L,
          
          '" preCellId="../',
          
          xml_escape(source_pop),
          
          '[',
          
          source_index,
          
          ']"',
          
          ' postCellId="../',
          
          xml_escape(target_pop),
          
          '[',
          
          target_index,
          
          ']"',
          
          ' weight="',
          
          format(
            weight_value,
            scientific = FALSE,
            trim = TRUE
          ),
          
          '" delay="',
          
          delay_value,
          
          '"/>'
          
        )
        
      )
      
    }
    
    
    xml <- c(
      
      xml,
      
      "    </projection>"
      
    )
    
  }
  
  
  # ==========================================================
  # FIN NETWORK
  # ==========================================================
  
  xml <- c(
    
    xml,
    
    "  </network>",
    
    "</neuroml>"
    
  )
  
  
  # ==========================================================
  # ECRITURE
  # ==========================================================
  
  writeLines(
    
    xml,
    
    con = file,
    
    useBytes = TRUE
    
  )
  
  
  # ==========================================================
  # RETOUR
  # ==========================================================
  
  cat("\n")
  cat(
    "Fichier NeuroML créé :",
    file,
    "\n"
  )
  
  cat(
    "Populations :",
    length(population_names),
    "\n"
  )
  
  cat(
    "Projections :",
    projection_counter,
    "\n"
  )
  
  cat(
    "Connexions exportées :",
    nrow(edges_export),
    "\n"
  )
  
  
  invisible(
    list(
      file = file,
      nodes = nodes,
      edges = edges_export,
      populations = population_names,
      projections = projection_counter,
      
      # mappings nécessaires après l'export
      bodyid_to_population = bodyid_to_population,
      bodyid_to_local_index = bodyid_to_local_index
    )
  )
  
}


# ============================================================
# 12. EXPORT NeuroML
# ============================================================

neuroml_export <- export_neuroml(
  
  connectome,
  
  file = "male_cns_219PN.nml"
  
)

bodyid_to_population <- neuroml_export$bodyid_to_population

bodyid_to_local_index <- neuroml_export$bodyid_to_local_index


# ============================================================
# 13. EXPORT DU MAPPING BIOLOGIQUE
# ============================================================

# Très important :
# le .nml utilise population[index] pour les connexions.
#
# Ce fichier permet de retrouver exactement :
#
# bodyid
# -> population
# -> index NeuroML
# -> type
# -> superclass
# -> nom
# -> côté
# etc.

nodes_export <- neuroml_export$nodes


nodes_export$neuroml_population <-
  
  unname(
    
    bodyid_to_population[
      as.character(
        nodes_export$bodyid
      )
    ]
    
  )


nodes_export$neuroml_index <-
  
  unname(
    
    bodyid_to_local_index[
      as.character(
        nodes_export$bodyid
      )
    ]
    
  )


write.csv(
  
  nodes_export,
  
  file =
    "male_cns_219PN_neuroml_nodes.csv",
  
  row.names = FALSE
  
)


# ============================================================
# 14. EXPORT DES CONNEXIONS BIOLOGIQUES
# ============================================================

edges_export_final <-
  
  neuroml_export$edges %>%
  
  mutate(
    
    source_population =
      bodyid_to_population[
        as.character(source)
      ],
    
    target_population =
      bodyid_to_population[
        as.character(target)
      ],
    
    source_neuroml_index =
      bodyid_to_local_index[
        as.character(source)
      ],
    
    target_neuroml_index =
      bodyid_to_local_index[
        as.character(target)
      ]
    
  )


write.csv(
  
  edges_export_final,
  
  file =
    "male_cns_219PN_neuroml_edges.csv",
  
  row.names = FALSE
  
)


# ============================================================
# 15. SAUVEGARDES R
# ============================================================

saveRDS(
  
  connectome,
  
  file =
    "male_cns_219PN_connectome.rds"
  
)


saveRDS(
  
  weights,
  
  file =
    "male_cns_219PN_weights.rds"
  
)


# ============================================================
# 16. RESUME FINAL
# ============================================================

cat("\n")
cat("========================================\n")
cat("EXPORT TERMINE\n")
cat("========================================\n")

cat(
  "PN :",
  length(connectome$pn_ids),
  "\n"
)

cat(
  "DN :",
  length(connectome$dn_ids),
  "\n"
)

cat(
  "Neurones :",
  nrow(connectome$nodes),
  "\n"
)

cat(
  "Connexions :",
  nrow(connectome$edges),
  "\n"
)

cat(
  "Synapses :",
  sum(connectome$edges$weight),
  "\n"
)

cat(
  "Fichier NeuroML :",
  "male_cns_219PN.nml",
  "\n"
)

cat(
  "Mapping neurones :",
  "male_cns_219PN_neuroml_nodes.csv",
  "\n"
)

cat(
  "Mapping connexions :",
  "male_cns_219PN_neuroml_edges.csv",
  "\n"
)

cat("\n")
cat("========================================\n")

