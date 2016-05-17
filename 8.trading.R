setwd(dirname(parent.frame(2)$ofile))

for.report <- FALSE

source("0.external_dependencies.R")
check.cran.pkg("zoo")
library(zoo)
check.cran.pkg("parallel")
library(parallel)
if (for.report) {
  library(devEMF)
  check.cran.pkg("devEmf")
}

if (!exists("cluster.leaders"))
  source("7.cluster_leader.R")

# 10th and 50th percentiles in normal distribution.
threshold <- qnorm(c(`short` = 0.1, `long` = 0.5))
# Sell 14 trading days after buying.
holding.period <- 14

calc.equity.curves <- function(inventories, predicted.clusters, cluster.leaders) {
  cl <- makePSOCKcluster(getOption("cl.cores", detectCores()))
  clusterExport(cl, c("inventories", "predicted.clusters", "cluster.leaders"), envir = environment())
  clusterEvalQ(cl, library(zoo))
  equity.curves <- do.call(cbind, parLapply(cl, 1:ncol(inventories), function(cluster.index) {
    to.trade.returns <- predicted.clusters[[cluster.index]]
    cluster.leader <- which(names(to.trade.returns) %in% cluster.leaders[cluster.index])
    if (length(cluster.leader))
      to.trade.returns <- to.trade.returns[, -cluster.leader, drop = FALSE]
    
    cluster.inventory <- inventories[, cluster.index]
    holding.dates <- c(which(cluster.inventory != 0), nrow(to.trade.returns))
    zoo::na.locf(Reduce(function(cumulative.value, row) {
      # Divide the current value of the portfolio evenly into the companies
      # in the cluster.
      invest.in.each <- tail(cumulative.value, 1) / ncol(to.trade.returns)
      # Assumes continuous rebalancing of the portfolio, even if the same
      # companies are held on consecutive days.
      cumulative.value[row] <- sum((1 + cluster.inventory[row] * to.trade.returns[row, ]) * invest.in.each)
      cumulative.value
    }, holding.dates, 1)) # Start with $1 in the portfolio.
  }))
  stopCluster(cl)
  
  equity.curves
}

inventories <- do.call(cbind, lapply(1:length(cluster.leaders), function(cluster.index) {
  # Get returns series of cluster leader.
  leader.series <- predicted.clusters[[cluster.index]][, cluster.leaders[cluster.index]]
  # Normalize returns series to z-scores.
  leader.series <- (leader.series - mean(leader.series)) / sd(leader.series)
  # 0 on boring days, -1 on signal days.
  short.signals <- -as.integer(leader.series < threshold["short"])
  # 0 on boring days, +1 on signal days.
  long.signals <- as.integer(leader.series > threshold["long"])
  # Thresholds are picked so that long and short signal cannot be
  # made on the same day. Simply adding the two series will give
  # us the combined set of signals.
  all.signals <- short.signals + long.signals
  # Ensure that trades are made AFTER the signals are observed.
  all.signals <- c(0, head(all.signals, -1))
  enter.dates <- which(all.signals != 0)
  # Extend the enter dates to the holding period length.
  trade.positions <- lapply(enter.dates, function(signal.date)
    c(rep(NA, signal.date - 1), rep(all.signals[signal.date], holding.period))
  )
  # If holding periods overlap, later trades will preempt earlier
  # trade holdings and any exit trades. Make sure that nothing is
  # held on the last day of price data (all positions are exited).
  c(head(Reduce(function(accumulated, marginal) {
    to.overwrite <- which(!is.na(marginal))
    accumulated[to.overwrite] <- marginal[to.overwrite]
    accumulated
  }, trade.positions, rep(0, length(all.signals))), length(all.signals) - 1), 0)
}))

strategy.days <- as.numeric(do.call(`-`, as.list(rev(range(selected$dates)))), units = "days")

#benchmark <- calc.equity.curves(rbind(0, matrix(1, nrow = nrow(inventories) - 2, ncol = ncol(inventories)), 0), predicted.clusters, cluster.leaders)
#benchmark.returns <- c(tail(benchmark, 1)) ^ (365.25 / strategy.days) - 1
#print(round(benchmark.returns * 100, 2))
#benchmark <- rowMeans(benchmark)
#benchmark.returns <- c(tail(benchmark, 1)) ^ (365.25 / strategy.days) - 1
#print(paste("Benchmark: ", round(benchmark.returns * 100, 2), "%", sep = ""))

# Buy and hold strategy.
benchmark <- calc.equity.curves(rbind(0, matrix(1, nrow = nrow(inventories) - 2, ncol = 1), 0), list(selected[, -1]), NA)
benchmark.returns <- c(tail(benchmark, 1)) ^ (365.25 / strategy.days) - 1
benchmark <- benchmark[, 1]
print(paste("Benchmark: ", round(benchmark.returns * 100, 2), "%", sep = ""))

equity.curves <- calc.equity.curves(inventories, predicted.clusters, cluster.leaders)
ann.returns <- c(tail(equity.curves, 1)) ^ (365.25 / strategy.days) - 1
print(round(ann.returns * 100, 2))
# Interpretation of doing mean: Invest $1 in each cluster, so $24 invested upfront.
# Sum the final portfolio values for each cluster (grown from the $1) and divide
# by the $24 initially invested.
equity.curve <- rowMeans(equity.curves)
ann.returns <- c(tail(equity.curve, 1)) ^ (365.25 / strategy.days) - 1
print(paste("Active: ", round(ann.returns * 100, 2), "%", sep = ""))

if (for.report)
  emf('returns.emf', width = 8, height = 4)
matplot(selected$dates, cbind(equity.curve, benchmark), lty = 1, type = "l", xaxt = "n")
axis(1, at = seq(min(selected$dates), max(selected$dates), by = "quarter"), labels = format(seq(min(selected$dates), max(selected$dates), by = "quarter"), "%m/%y"))
plot(selected$dates, equity.curve - benchmark, type = "l", main = paste("Excess return (annualized ", round(ann.returns * 100, 2), "% cf. ", round(benchmark.returns * 100, 2), "%)", sep = ""), ylab = "erp_cluster - buy_and_hold")
abline(h = 0)
if (for.report)
  dev.off()
