install.packages("natmanager")
natmanager::check_pat()
natmanager::install(pkgs="natverse/malecns")
browseURL('https://neuprint.janelia.org')


usethis::edit_r_environ()

library(malecns) 
library(rgl)

Sys.setenv(NEUPRINT_TOKEN ="")


choose_mcns_dataset("male-cns:v1.0")

dr_malecns()

meta <- mcns_neuprint_meta('/.+_[adl]+PN')

dim(meta)
head(meta)
names(meta)

mcns_connection_table()

pn_ids <- meta$bodyid[1:10]

conn <- mcns_connection_table(pn_ids)

dim(conn)
names(conn)
head(conn)


table(conn$prepost)

sort(table(conn$type), decreasing = TRUE)[1:20]

sort(table(conn$superclass), decreasing = TRUE)

conn_sorted <- conn[order(-conn$weight), ]

head(conn_sorted, 20)

conn[conn$superclass == "descending_neuron", ]

unique(conn[conn$superclass == "descending_neuron", c("partner", "name", "type", "group")])

conn[conn$superclass == "ascending_neuron", ]


dn <- conn[!is.na(conn$superclass) &
             conn$superclass == "descending_neuron", ]

dn[, c("bodyid", "partner", "weight", "name", "type", "somaSide")]

unique(dn[, c("partner", "name", "type", "group")])

conn_out <- mcns_connection_table(
  pn_ids,
  partners = "outputs"
)


dn_out <- conn_out[
  !is.na(conn_out$superclass) &
    conn_out$superclass == "descending_neuron",
]

dn_out[, c(
  "bodyid",
  "partner",
  "prepost",
  "weight",
  "name",
  "type",
  "somaSide"
)]

dn_ids <- unique(dn_out$partner)
dn_ids


dn_conn <- mcns_connection_table(
  dn_ids,
  partners = "outputs"
)

dn_conn[, c(
  "bodyid",
  "partner",
  "prepost",
  "weight",
  "name",
  "type",
  "group",
  "superclass",
  "somaSide"
)]

dn_to_vnc <- dn_conn[
  !is.na(dn_conn$superclass) &
    dn_conn$superclass != "descending_neuron",
]


library(dplyr)


edges <- bind_rows(
  dn_out %>%
    transmute(
      source = bodyid,
      target = partner,
      weight = weight
    ),
  
  dn_conn %>%
    transmute(
      source = bodyid,
      target = partner,
      weight = weight
    )
)

edges <- edges %>%
  distinct(source, target, .keep_all = TRUE)

head(edges)


node_ids <- unique(c(edges$source, edges$target))

length(node_ids)

nodes <- mcns_neuprint_meta(ids = node_ids)

dim(nodes)
names(nodes)


nrow(nodes)
nrow(edges)

sum(edges$weight)
summary(edges$weight)

table(nodes$superclass, useNA = "ifany")

connectome_graph <- list(
  nodes = nodes,
  edges = edges
)

class(connectome_graph) <- "connectome_graph"


str(connectome_graph, max.level = 1)

edges %>%
  count(source, target) %>%
  filter(n > 1)

library(Matrix)

W <- sparseMatrix(
  i = match(edges$target, nodes$bodyid),
  j = match(edges$source, nodes$bodyid),
  x = edges$weight,
  dims = c(nrow(nodes), nrow(nodes))
)

run_connectome <- function(W, input, steps = 10, decay = 0.9) {
  
  n <- nrow(W)
  
  state <- as.numeric(input)
  
  history <- matrix(
    0,
    nrow = steps,
    ncol = n
  )
  
  for (t in seq_len(steps)) {
    
    activation <- pmax(0, state)
    
    state <- as.numeric(
      decay * state + W %*% activation
    )
    
    history[t, ] <- state
  }
  
  list(
    final_state = state,
    history = history
  )
}


input <- numeric(nrow(nodes))

pn_index <- match(13483, nodes$bodyid)

input[pn_index] <- 1


result <- run_connectome(
  W,
  input,
  steps = 10
)


top <- order(result$final_state, decreasing = TRUE)[1:20]

nodes[top, c(
  "bodyid",
  "name",
  "type",
  "superclass",
  "group",
  "somaSide"
)]


summary(result$final_state)


max(result$final_state)

head(
  sort(result$final_state, decreasing = TRUE),
  20
)

edges$weight_log <- log1p(edges$weight)

edges$weight_norm <- edges$weight /
  ave(
    edges$weight,
    edges$source,
    FUN = sum
  )

summary(edges$weight_log)
summary(edges$weight_norm)

head(edges)

node_index <- setNames(
  seq_len(nrow(nodes)),
  nodes$bodyid
)

W_raw <- sparseMatrix(
  i = node_index[as.character(edges$target)],
  j = node_index[as.character(edges$source)],
  x = edges$weight,
  dims = c(nrow(nodes), nrow(nodes))
)

W_log <- sparseMatrix(
  i = node_index[as.character(edges$target)],
  j = node_index[as.character(edges$source)],
  x = edges$weight_log,
  dims = c(nrow(nodes), nrow(nodes))
)

W_norm <- sparseMatrix(
  i = node_index[as.character(edges$target)],
  j = node_index[as.character(edges$source)],
  x = edges$weight_norm,
  dims = c(nrow(nodes), nrow(nodes))
)

input <- numeric(nrow(nodes))

pn_index <- match(13483, nodes$bodyid)

input[pn_index] <- 1

result_raw <- run_connectome(
  W_raw,
  input,
  steps = 10
)

result_log <- run_connectome(
  W_log,
  input,
  steps = 10
)

result_norm <- run_connectome(
  W_norm,
  input,
  steps = 10
)

c(
  raw = max(result_raw$final_state),
  log = max(result_log$final_state),
  norm = max(result_norm$final_state)
)

c(
  raw = sum(result_raw$final_state > 0),
  log = sum(result_log$final_state > 0),
  norm = sum(result_norm$final_state > 0)
)


c(
  raw_01 = sum(result_raw$final_state > 0.1),
  log_01 = sum(result_log$final_state > 0.1),
  norm_01 = sum(result_norm$final_state > 0.1),
  
  raw_1 = sum(result_raw$final_state > 1),
  log_1 = sum(result_log$final_state > 1),
  norm_1 = sum(result_norm$final_state > 1)
)

summary(result_raw$final_state)
summary(result_log$final_state)
summary(result_norm$final_state)

quantile(
  result_norm$final_state,
  probs = c(0, .5, .9, .95, .99, .999, 1)
)

run_connectome_v2 <- function(
    W,
    input,
    steps = 20,
    decay = 0.9,
    threshold = 0.01
) {
  
  n <- nrow(W)
  
  state <- as.numeric(input)
  
  history <- matrix(
    0,
    nrow = steps,
    ncol = n
  )
  
  for (t in seq_len(steps)) {
    
    activation <- tanh(
      pmax(0, state - threshold)
    )
    
    state <- as.numeric(
      decay * state +
        W %*% activation
    )
    
    history[t, ] <- state
  }
  
  list(
    final_state = state,
    history = history
  )
}

result_v2 <- run_connectome_v2(
  W_norm,
  input,
  steps = 20,
  decay = 0.9,
  threshold = 0.01
)

summary(result_v2$final_state)

c(
  gt_001 = sum(result_v2$final_state > 0.001),
  gt_01  = sum(result_v2$final_state > 0.01),
  gt_1   = sum(result_v2$final_state > 1)
)

apply(
  result_v2$history,
  1,
  function(x) sum(x > 0.01)
)

result_v2_long <- run_connectome_v2(
  W_norm,
  input,
  steps = 200,
  decay = 0.9,
  threshold = 0.01
)

active <- apply(
  result_v2_long$history,
  1,
  function(x) sum(x > 0.01)
)

active

tail(active, 20)

peak_t <- which.max(active)

peak_t

peak_state <- result_v2_long$history[peak_t, ]

peak_nodes <- nodes[
  peak_state > 0.01,
]

table(peak_nodes$superclass)

top_peak <- order(
  peak_state,
  decreasing = TRUE
)[1:20]

nodes[top_peak, c(
  "bodyid",
  "name",
  "type",
  "superclass",
  "somaSide"
)]

interesting_ids <- c(
  10590,   # DNpe052_L
  10118,   # DNb05_L
  27702,   # ANXXX013_L
  801828,  # IN12B015_L
  801280,  # IN06B021_L
  13483    # DA1_lPN_L
)

interesting_idx <- match(
  interesting_ids,
  nodes$bodyid
)

activity_trace <- result_v2_long$history[
  , interesting_idx,
  drop = FALSE
]

colnames(activity_trace) <- nodes$name[
  interesting_idx
]

activity_trace[1:30, ]

peak_time <- apply(
  result_v2_long$history,
  2,
  which.max
)

nodes$peak_time <- peak_time

nodes[
  nodes$bodyid %in% interesting_ids,
  c("bodyid", "name", "superclass", "peak_time")
]
