setwd(dirname(parent.frame(2)$ofile))

if (!exists("clusters") || !exists("gerp.affinity"))
  source("6.cluster_with_lag.R")

intersperse <- function(x, y)
  # R is column major, so it reads the two rows alternately when downcasting.
  c(rbind(x, y))

predicted.clusters <- split(2:ncol(selected), clusters)
predicted.clusters <- lapply(predicted.clusters, function(company.indices) {
  x <- selected[, company.indices, drop = FALSE]
  attr(x, "members") <- company.indices
  x
})

clusters.to.keep <- unlist(lapply(predicted.clusters, function(cluster) dim(cluster)[2] > 1))
if (length(which(!clusters.to.keep)))
  print(paste("Singleton clusters ", paste(unlist(lapply(predicted.clusters[which(!clusters.to.keep)], colnames)), collapse = "', '"), " eliminated", sep = "'"))
predicted.clusters <- predicted.clusters[clusters.to.keep]

lag.structs <- attr(gerp.affinity, "pass.through.attr")
lag.struct.stats <- lapply(predicted.clusters, function(cluster) {
  members <- attr(cluster, "members")
  pairs <- combn(members, 2)
  leads.and.lags <- apply(pairs, 2, function(pair) {
    stopifnot(pair[1] < pair[2])
    lag.struct <- lag.structs[[pair[1] - 1]][[pair[2] - 1]]
    # pair[1] leads over pair[2] and  pair[2] leads over pair[1].
    # Distances were calculated only for the lower triangle.
    list(lag.struct$i.leads, lag.struct$i.lags)
  })
  leads.and.lags <- do.call(cbind, lapply(leads.and.lags, function(lead.and.lag) do.call(cbind, lead.and.lag)))
  colnames(leads.and.lags) <- intersperse(apply(pairs, 2, paste, collapse = " "), apply(pairs, 2, function(pair) paste(rev(pair), collapse = " ")))
  
  # Company on the left leads over company on the top.
  mean.matrix <- matrix(NA, nrow = length(members), ncol = length(members))
  rownames(mean.matrix) <- colnames(selected)[members]
  colnames(mean.matrix) <- colnames(selected)[members]
  diag(mean.matrix) <- 0
  
  sd.matrix <- matrix(NA, nrow = length(members), ncol = length(members))
  rownames(sd.matrix) <- colnames(selected)[members]
  colnames(sd.matrix) <- colnames(selected)[members]
  diag(sd.matrix) <- 0
  
  for (col.name in colnames(leads.and.lags)) {
    pair <- colnames(selected)[as.integer(strsplit(col.name, " ")[[1]])]
    mean.matrix[pair[[1]], pair[[2]]] <- mean(leads.and.lags[, col.name])
    sd.matrix[pair[[1]], pair[[2]]] <- sd(leads.and.lags[, col.name])
  }
  
  # Focus on the rows when selecting companies.
  # Higher mean and lower standard deviation is better.
  list(means = cbind(mean.matrix, `Aggregate` = rowMeans(mean.matrix)), sds = cbind(sd.matrix, `Aggregate` = rowMeans(sd.matrix)))
})

cluster.leaders <- unname(unlist(lapply(lag.struct.stats, function(cluster) names(which.max(cluster$means[, "Aggregate"] / cluster$sds[, "Aggregate"])))))
leader.stats <- lapply(1:length(cluster.leaders), function(cluster.index)
  rbind(`lag means` = lag.struct.stats[[cluster.index]]$means[cluster.leaders[cluster.index], ],
        `lag SDs` = lag.struct.stats[[cluster.index]]$sds[cluster.leaders[cluster.index], ])
)
