setwd(dirname(parent.frame(2)$ofile))

for.report <- TRUE

if (!exists("cluster.leaders"))
  source("7.cluster_leader.R")
if (!exists("lag.struct.coords"))
  source("10.lead_lag.R")

for (i in 1:length(predicted.clusters))
  attr(predicted.clusters[[i]], "leader") <- which(colnames(predicted.clusters[[i]]) == cluster.leaders[i])
if (for.report)
  pdf(file = paste(switch(baseline + 1, "gerp_cluster", "corr_cluster", "industry_group"), "lags.pdf", sep = "_"), title = "Clusters", width = 22, height = 10)
lapply(predicted.clusters, function(cluster) {
  members <- attr(cluster, "members") - 1
  leader <- attr(cluster, "leader")
  cluster <- apply(cluster, 2, function(returns) Reduce(function(acc, add) c(acc, tail(acc, 1) * add), 1 + returns))
  colors <- rep(1, length(members))
  colors[leader] <- 2
  matplot(selected$dates, cluster, type = "l", col = colors, lty = 1, lwd = 2, xlab = "Date", ylab = "Price movements", main = paste(colnames(cluster), collapse = ", "), xaxt = "n")
  axis(1, at = seq(min(selected$dates), max(selected$dates), by = "month"), labels = format(seq(min(selected$dates), max(selected$dates), by = "month"), "%m/%y"))
  colors <- rep(3:6, length(members) - 1)
  for (i in 1:length(members))
    if (i != leader)
      lines(lag.struct.coords(gerp.affinity, selected, c(members[leader], members[i])), col = colors[i], lwd = 1)
})
if (for.report)
  dev.off()

for (i in 1:length(group.clusters))
  attr(group.clusters[[i]], "leader") <- which(colnames(group.clusters[[i]]) == group.leaders[i])
if (for.report)
  pdf(file = "industry_group_lags.pdf", title = "Industries", width = 22, height = 10)
lapply(group.clusters, function(cluster) {
  members <- attr(cluster, "members") - 1
  leader <- attr(cluster, "leader")
  cluster <- apply(cluster, 2, function(returns) Reduce(function(acc, add) c(acc, tail(acc, 1) * add), 1 + returns))
  colors <- rep(1, length(members))
  colors[leader] <- 2
  matplot(selected$dates, cluster, type = "l", col = colors, lty = 1, lwd = 2, xlab = "Date", ylab = "Price movements", main = paste(colnames(cluster), collapse = ", "), xaxt = "n")
  axis(1, at = seq(min(selected$dates), max(selected$dates), by = "month"), labels = format(seq(min(selected$dates), max(selected$dates), by = "month"), "%m/%y"))
  colors <- rep(3:6, length(members) - 1)
  for (i in 1:length(members))
    if (i != leader)
      lines(lag.struct.coords(gerp.affinity, selected, c(members[leader], members[i])), col = colors[i], lwd = 1)
})
if (for.report)
  dev.off()
