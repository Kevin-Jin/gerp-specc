setwd(dirname(parent.frame(2)$ofile))

if (!exists("gerp.affinity"))
  source("6.cluster_with_lag.R")

arr.index <- function(arr, x) {
  dim <- dim(arr)
  if (length(dim) == 2)
    if (length(x) == 1)
      c((x - 1) %% dim[1] + 1, (x - 1) %/% dim[1] + 1)
  else
    rbind((x - 1) %% dim[1] + 1, (x - 1) %/% dim[1] + 1)
}

most.similar.companies <- function(affinity)
  arr.index(affinity, order(local({ diag(affinity) <- NA; affinity[upper.tri(affinity)] <- NA; affinity }), decreasing = TRUE, na.last = NA))

# companies must be in the range of 1:(ncol(selected) - 1)
lag.struct.coords <- function(gerp.affinity, selected, companies = setNames(most.similar.companies(gerp.affinity)[, 1], c("leader", "lagger")), increments = nrow(selected) / 100) {
  pair <- apply(selected[, companies + 1], 2, function(returns) Reduce(function(acc, add) c(acc, tail(acc, 1) * add), 1 + returns))
  lag.structs <- attr(gerp.affinity, "pass.through.attr")
  lag.struct <- 1:nrow(selected)
  lag.struct <- cbind(lag.struct, lag.struct +
      if (companies[1] < companies[2])
        lag.structs[[companies[1]]][[companies[2]]]$i.leads
    else if (companies[2] < companies[1])
      lag.structs[[companies[2]]][[companies[1]]]$i.lags
    else
      stop("Must have two distinct companies")
  )
  lag.struct <- lag.struct[seq(1, nrow(lag.struct), increments), ]
  lag.struct <- apply(lag.struct, 2, function(dim) selected$dates[ifelse(dim <= 0, NA, dim)])
  
  do.call(rbind, lapply(split(lag.struct, row(lag.struct)), function(match)
    if (!any(is.na(match)))
      rbind(cbind(match, c(pair[selected$dates == match[1], 1], pair[selected$dates == match[2], 2])), NA)
    else
      NULL
  ))
}

plot.lag.struct <- function(gerp.affinity, selected, companies = setNames(most.similar.companies(gerp.affinity)[, 1], c("leader", "lagger")), increments = nrow(selected) / 100) {
  pair <- apply(selected[, companies + 1], 2, function(returns) Reduce(function(acc, add) c(acc, tail(acc, 1) * add), 1 + returns))
  matplot(selected$dates, pair, type = "l", lty = 1, xlab = "Date", ylab = "Price movements", main = paste(paste(colnames(pair), collapse = ", "), "; Similarity: ", round(do.call(`[`, c(list(gerp.affinity), as.list(companies))), 3), sep = ""), xaxt = "n")
  axis(1, at = seq(min(selected$dates), max(selected$dates), by = "month"), labels = format(seq(min(selected$dates), max(selected$dates), by = "month"), "%m/%y"))
  lines(lag.struct.coords(gerp.affinity, selected, companies, increments), col = 3)
}

#plot.lag.struct(gerp.affinity, selected, c(`leader` = 9, `lagger` = 70), 1)
