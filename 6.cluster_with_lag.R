library(kernlab)
library(TSdist)
library(parallel)
library(stats)
library(fossil)

invalidate.affinities <- FALSE
baseline <- '3'
if(exists("baseLine"))
  baseline <- baseLine

setwd(dirname(parent.frame(2)$ofile))

if (invalidate.affinities)
  rm(selected)
if (!exists("selected") || !exists("true.groups"))
  source("4.company_selector.R")

#arg.num.groups <- 24  
#source("4.company_selector.R")

adjusted.rand.index <- function(x, y) {
  if (length(x) != length(y)) 
    stop("different sizes of the label assignments")
  
  tab <- table(x, y)
  if (all(dim(tab) == c(1, 1))) 
    return(1)
  
  a <- sum(choose(tab, 2))
  b <- sum(choose(rowSums(tab), 2)) - a
  c <- sum(choose(colSums(tab), 2)) - a
  d <- choose(sum(tab), 2) - a - b - c
  
  (a - (a + b) * (a + c) / (a + b + c + d)) / ((a + b + a + c) /2 - (a + b) * (a + c) / (a + b + c + d))
}

parallelized.kernel.matrix <- function(measure, kernel) {
  pass.through.attr <- NULL
  
  # Run a cluster since computation is computationally heavy
  cl <- parallel::makePSOCKcluster(getOption("cl.cores", detectCores()))
  parallel::clusterExport(cl, c("measure"), envir = environment())
  parallel::clusterEvalQ(cl, library(TSdist))
  # At ncol(measure), the only cell in the upper triangle is
  # the diagonal cell, which is simply 1, so skip that iteration.
  scores <- matrix(c(unlist(lapply(1:(ncol(measure) - 1), function(i) {
    # Skip the upper triangular part of the matrix by filling the first
    # part of the column with NAs. The matrix is symmetric, and we can
    # just copy the values later. Also skip the diagonal cells (i, i)
    # because they will always be 1.
    result <- parallel::parLapply(cl, (i + 1):ncol(measure), function(j) kernel(measure, i, j))
    result.attr <- lapply(result, attributes)
    if (!all(unlist(lapply(result.attr, is.null)))) {
      pass.through.attr <- get("pass.through.attr", envir = parent.frame(n = 2))
      pass.through.attr[[i]] <- c(rep(list(NULL), i), result.attr)
      assign("pass.through.attr", pass.through.attr, envir = parent.frame(n = 2))
    }
    c(rep(NA, i), unlist(result))
  })), rep(NA, ncol(measure))), ncol = ncol(measure))
  parallel::stopCluster(cl)
  diag(scores) <- 1
  scores[upper.tri(scores)] <- t(scores)[upper.tri(scores)]
  colnames(scores) <- colnames(measure)
  rownames(scores) <- colnames(measure)
  
  scores <- as.kernelMatrix(scores)
  attr(scores, "pass.through.attr") <- pass.through.attr
  scores
}

correlation.matrix <- function(measure)
  parallelized.kernel.matrix(measure, function(measure, i, j) cor(measure[, i], measure[, j]))

# Goal: pick sigma so that mean of all affinities is close to 0.80?
# Maybe pick sigma close to mean of all erpDistances?
gerp.matrix <- function(measure, scale = FALSE, sigma = if (scale) nrow(measure) else (nrow(measure) / 100) ^ 0.55, max.lag = Inf) {
  force(sigma)
  if (scale)
    # Under random walk/efficient market hypothesis,
    # returns have mean around 0, but variance surprisingly differs a lot.
    measure <- as.data.frame(apply(measure, 2, function(col) col / sd(col)))
  
  parallelized.kernel.matrix(measure, function(measure, i, j) {
    if (is.infinite(max.lag))
      dist <- TSdist::erpDistance(measure[, i], measure[, j], g = 0, lead.lag.info = TRUE)
    else
      dist <- TSdist::erpDistance(measure[, i], measure[, j], g = 0, sigma = max.lag, lead.lag.info = TRUE)
    result <- exp(-dist ^ 2 / (2 * sigma ^ 2))
    attributes(result) <- NULL
    attr(result, "i.leads") <- attr(dist, "x.cum.leads")
    attr(result, "i.lags") <- attr(dist, "y.cum.leads")
    attributes(dist) <- NULL
    attr(result, "dist") <- dist
    result
  })
}

cluster.centers <- function(clusters, obs, num.centers = length(clusters@size))
  clusters@centers <- do.call(cbind, lapply(1:num.centers, function(cluster) rowMeans(obs[, which(clusters == cluster), drop = FALSE])))

cluster.inertias <- function(clusters, obs, num.centers = length(clusters@size), centers = cluster.centers(clusters, obs, num.centers))
  clusters@withinss <- unlist(lapply(1:num.centers, function(cluster) sum((obs[, which(clusters == cluster), drop = FALSE] - centers[, cluster]) ^ 2)))

# Pick the dates.
#selected <- selected[selected$dates >= as.Date("2004-05-12") & selected$dates <= as.Date("2005-07-14"), ]
selected <- selected[selected$dates >= as.Date("2008-05-12") & selected$dates <= as.Date("2009-07-14"), ]

if (!exists("gerp.affinity") || invalidate.affinities) {
  # Until we can figure out how to persist the lag structures, don't cache affinity matrix.
  if (TRUE || invalidate.affinities)
    file.remove("gerp.affinity.csv")
  if (!file.exists("gerp.affinity.csv")) {
    # Skip the date column.
    gerp.affinity <- gerp.matrix(selected[, -1], scale = TRUE, max.lag = 14)
    write.csv(gerp.affinity, "gerp.affinity.csv")
  } else {
    gerp.affinity <- as.kernelMatrix(as.matrix(read.csv("gerp.affinity.csv", check.names = FALSE, row.names = 1)))
  }
}
if (!exists("corr.affinity") || invalidate.affinities) {
  if (invalidate.affinities)
    file.remove("corr.affinity.csv")
  if (!file.exists("corr.affinity.csv")) {
    # Skip the date column.
    corr.affinity <- correlation.matrix(selected[, -1]) ^ 2
    write.csv(corr.affinity, "corr.affinity.csv")
  } else {
    corr.affinity <- as.kernelMatrix(as.matrix(read.csv("corr.affinity.csv", check.names = FALSE, row.names = 1)))
  }
}

group.sector.mapping <- unique(industry.groups.from.cache[, c("GICS_INDUSTRY_GROUP_NAME", "GICS_INDUSTRY_GROUP")])
rownames(group.sector.mapping) <- 1:nrow(group.sector.mapping)
colnames(group.sector.mapping) <- c("Group", "Sector")
group.sector.mapping$Sector <- group.sector.mapping$Sector %/% 100
true.sectors <- as.factor(group.sector.mapping$Sector[match(true.groups, group.sector.mapping$Group)])
ground.truth <- true.groups # true.sectors

source("5.override_kmeans.R")
tryCatch({
  centers <- length(levels(ground.truth))
  # Average of industry groups for initial centers. Otherwise specc() is non-deterministic.
  attr(centers, "kmeans.pass.through") <- split(1:(ncol(selected) - 1), ground.truth)
  
  clusters <- stats::kmeans(t(data.matrix(selected))[-1,], centers)
  print(paste("KMEANS fossil rand index:", fossil::rand.index(as.integer(ground.truth), as.integer(clusters$cluster))))
  print(paste("KMEANS adj rand index:", adjusted.rand.index(as.integer(ground.truth), as.integer(clusters$cluster)), "Inertia:", clusters$tot.withinss))
  
  clusters <- kernlab::specc(gerp.affinity, centers)
  print(paste("GERP fossil rand index:", fossil::rand.index(as.integer(ground.truth), as.integer(clusters))))
  print(paste("GERP rand index:", adjusted.rand.index(as.integer(ground.truth), as.integer(clusters)), "Inertia:", sum(cluster.inertias(clusters, selected[, -1]))))
  
  clusters <- kernlab::specc(corr.affinity, centers)
  print(paste("CORR fossil rand index:", fossil::rand.index(as.integer(ground.truth), as.integer(clusters))))
  print(paste("CORR rand index:", adjusted.rand.index(as.integer(ground.truth), as.integer(clusters)), "Inertia:", sum(cluster.inertias(clusters, selected[, -1]))))
  
  switch(as.character(baseline),
         `0` = {
           #GERP clusters
           clusters <- kernlab::specc(gerp.affinity, centers)
         },
         `1` = {
           # Baseline 1: do trading stategy based on clusters produced by correlation.
           clusters <- kernlab::specc(corr.affinity, centers)
         },
         `2` = {
           # Baseline 2: do trading strategy based on industry groups as clusters.
           clusters <- ground.truth
         },
         '3' = 
           #Baseline 3: Kmeans
           clusters <- stats::kmeans(t(data.matrix(selected))[-1,], centers)$cluster
  )
}, finally = {
  attr(kmeans, "revert")()
})