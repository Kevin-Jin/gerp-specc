setwd(dirname(parent.frame(2)$ofile))

invalidate.samples <- FALSE

groups.to.use <- 1:24
sample.companies.per.group <- 3

if (!file.exists("selected.csv") || invalidate.samples) {
  if (!exists("groups"))
    source("3.clean_industry_groups.R")
  
  groups.to.use <- groups[groups.to.use]
  
  # Skip date column for each group.
  from.each <- min(unlist(lapply(groups.to.use, length))) - 1
  if (from.each <= sample.companies.per.group) {
    print(paste("Smallest group has", from.each, "companies, so sampling", from.each * length(groups.to.use), "companies total"))
  } else {
    from.each <- sample.companies.per.group
    print(paste("Sampling", from.each * length(groups.to.use), "companies total"))
  }
  new.groups <- setNames(lapply(groups.to.use, function(group)
    group[, sample(2:length(group), from.each)]
  ), names(groups.to.use))
  new.groups <- new.groups[order(names(new.groups))]
  
  dates <- groups.to.use[[1]]$dates
  # Get prices before transformation to z-scores.
  selected <- cbind(dates, do.call(cbind, unname(lapply(new.groups, function(group) single.df[match(dates, single.df$dates), colnames(group)]))))
  # Convert prices to returns. Differencing always removes the first observation.
  selected <- cbind(dates = dates[-1], rbind(head(selected, -1)[, -1] / tail(selected, -1)[, -1] - 1))
  
  #true.groups <- as.factor(industry.groups.from.cache[colnames(selected[, -1]), "GICS_INDUSTRY_GROUP"] %/% 100)
  true.groups <- as.factor(industry.groups.from.cache[colnames(selected[, -1]), "GICS_INDUSTRY_GROUP_NAME"])
  names(true.groups) <- colnames(selected[, -1])
  
  write.csv(selected, "selected.csv", row.names = FALSE)
} else if (!exists("selected")) {
  source("1.download_historicals.R")
  if (!exists("industry.groups.from.cache"))
    industry.groups.from.cache <- load.industry.groups.from.cache()
  
  selected <- read.csv("selected.csv", stringsAsFactors = FALSE, check.names = FALSE)
  selected$dates <- as.Date(selected$dates)
  
  # GICS sectors are simply first two digits of industry group code.
  #true.groups <- as.factor(industry.groups.from.cache[colnames(selected[, -1]), "GICS_INDUSTRY_GROUP"] %/% 100)
  true.groups <- as.factor(industry.groups.from.cache[colnames(selected[, -1]), "GICS_INDUSTRY_GROUP_NAME"])
  names(true.groups) <- colnames(selected[, -1])
}
