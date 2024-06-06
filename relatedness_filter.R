# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: relatedness_filter.R                                                     #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. apoe_table (from the apoe_processing.R script)

# 2. list of participant IDs with FreeSurfer-based neuroimaging data (img_eids)

# 3. Relatedness file (UKB pairs of individuals related up to the third degree)
#    https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=531


# ___________________________________ FUNCTIONS ___________________________________ #

### Function to determine the individual to be removed based on APOE genotype

getRemoveID <- function(input_table){
  
  # get list of IDs with the highest edge frequency
  removeCandidates <- input_table[input_table$Freq == input_table$Freq[1], 1]
  removeCandidates <- data.frame(ID = removeCandidates)
  
  # append information on APOE genotype and order by priority
  removeCandidates$apoe <- plyr::mapvalues(removeCandidates$ID, apoe_table$ID, geno_apoe$apoe_geno, warn_missing = FALSE)
  removeCandidates$apoe <- factor(removeCandidates$apoe,
                                  levels = c("e4/e4", "e3/e4", "e2/e3", "e3/e3", "e2/e2", "e2/e4", "e1/e4", "e1/e2"))
  removeCandidates <- removeCandidates[order(removeCandidates$apoe, decreasing = TRUE), ]
  
  # randomly select individual to be removed if all candidates have the same APOE genotype
  # else, remove the one at the top (with the lowest APOE genotype keep priority)
  remove_ID <- NULL
  if (length(unique(removeCandidates$apoe)) == 1) {
    remove_ID <- sample(removeCandidates$ID, 1)
  } else {
    remove_ID <- removeCandidates$ID[1]
  }
  
  cat("Removing individual: ", remove_ID, "\n", file = log_file, append = TRUE)
  
  return(remove_ID)
}


# _____________________________________ SCRIPT ____________________________________ #

### Filter for pairs where both individuals have neuroimaging data

apoe_table <- apoe_table %>%
  filter(ID %in% img_eids)

img_relatedness <- relatedness %>%
  filter(ID1 %in% apoe_table$ID & ID2 %in% apoe_table$ID) %>%
  select(ID1, ID2)


### Relatedness filter (as described in the supplementary text)

library("igraph")
set.seed(1) # set seed

g <- graph_from_data_frame(img_relatedness, directed = F, vertices = NULL) # construct a network graph of related pairs
g_data <- split(V(g), components(g)$membership) # get cluster membership information

length(g_data) # how many clusters of related individuals are there total?

remove <- c() # list of IDs to remove
epoch <- 1 # to track epoch

relatedness_remain <- img_relatedness

log_file <- file(paste0("relatedness_log_", Sys.Date(), ".txt"), open = "w")

while (length(g_data) > 0) {
  
  cat("###------------------------------------------------------------\n", file = log_file)
  cat("EPOCH: ", epoch, "\n", file = log_file)
  cat("Number of clusters: ", length(g_data), "\n", file = log_file)

  for (i in 1:length(g_data)) { # loop through the clusters
    cluster_size <- length(g_data[[i]]) # get the cluster size
    
    cat("-------------------------\n", file = log_file)
    cat("Working on cluster ", i, "\n", file = log_file)
    cat("Number of individuals in cluster: ", cluster_size, "\n", file = log_file)
    
    indv_in_cluster <- attr(g_data[[i]], "names") # get the individuals in the cluster
    
    # subset all pairings in the cluster
    pair_IDs <- relatedness_remain[relatedness_remain$ID1 %in% indv_in_cluster | relatedness_remain$ID2 %in% indv_in_cluster, ]
    
    # get the number of edges (connections) each individual in the cluster has
    edge_info <- as.data.frame(table(c(pair_IDs$ID1, pair_IDs$ID2))) 
    names(edge_info)[1] <- "ID"
    edge_info$ID <- as.character(edge_info$ID)
    edge_info <- edge_info[order(edge_info$Freq, decreasing = TRUE), ] # order by number of edges (from largest to smallest)

    remove <- c(remove, getRemoveID(edge_info))
  }
  
  # Remove relatedness data for individuals in 'remove'
  relatedness_remain <- relatedness_remain[!(relatedness_remain$ID1 %in% remove) & !(relatedness_remain$ID2 %in% remove), ]
  
  # Reconstruct the graph and obtain the updated cluster information
  g_remain <- graph_from_data_frame(relatedness_remain, directed = FALSE, vertices = NULL)
  g_data <- split(V(g_remain), components(g_remain)$membership)
  
  epoch <- epoch + 1
}

close(log_file)

img_eids_filtered <- img_eids[!img_eids %in% remove] # final list of IDs

# _________________________________ END OF SCRIPT _________________________________ #
