setwd(dirname(parent.frame(2)$ofile))

invalidate.samples <- FALSE

if (!file.exists("selected.csv") || invalidate.samples) {
  if (!exists("groups"))
    source("3.industry_group_cleaner.R")
  
  # Skip date column for each group.
  from.each <- min(unlist(lapply(groups, length))) - 1
  print(paste("Smallest group has", from.each, "companies, so sampling", from.each * length(groups), "companies total"))
  new.groups <- setNames(lapply(groups, function(group)
    group[, sample(2:length(group), from.each)]
  ), names(groups))
  new.groups <- new.groups[order(names(new.groups))]
  
  dates <- groups[[1]]$dates
  # Get prices before transformation to z-scores.
  selected <- cbind(dates, do.call(cbind, unname(lapply(new.groups, function(group) single.df[match(dates, single.df$dates), colnames(group)]))))
  # Convert prices to returns. Differencing always removes the first observation.
  selected <- cbind(dates = dates[-1], rbind(head(selected, -1)[, -1] / tail(selected, -1)[, -1] - 1))
  
  true.groups <- as.factor(unlist(lapply(names(groups), function(group.name) rep(group.name, ncol(groups[[group.name]])))))
  names(true.groups) <- colnames(selected[, -1])
  
  write.csv(selected, "selected.csv", row.names = FALSE)
} else if (!exists("selected")) {
  source("1.download_historicals.R")
  if (!exists("industry.groups.from.cache"))
    industry.groups.from.cache <- load.industry.groups.from.cache()
  
  # GICS sectors are simply first two digits of industry group code.
  #industries <- sort(levels(as.factor(industry.groups.from.cache$GICS_INDUSTRY_GROUP %/% 100)))
  industries <- sort(levels(as.factor(industry.groups.from.cache$GICS_INDUSTRY_GROUP_NAME)))
  
  selected <- read.csv("selected.csv", stringsAsFactors = FALSE, check.names = FALSE)
  selected$dates <- as.Date(selected$dates)
  
  true.groups <- as.factor(unlist(lapply(industries, function(group.name) rep(group.name, (ncol(selected) - 1) / length(industries)))))
  names(true.groups) <- colnames(selected[, -1])
}
