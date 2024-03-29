setwd(dirname(parent.frame(2)$ofile))

invalidate.combined <- FALSE

source("0.external_dependencies.R")

source("1.download_historicals.R")

if (!file.exists("combined.csv") || invalidate.combined) {
  check.cran.pkg("zoo")
  library(zoo)
  
  if (!exists("prices.from.cache"))
    prices.from.cache <- load.historicals.from.cache()
  
  #dates <- do.call(seq, c(as.list(Reduce(function(acc, add) c(min(acc[1], add[1]), max(acc[2], add[2])), lapply(prices.from.cache, function(x) range(x$date)))), list(by = "day")))
  dates <- sort(Reduce(function(acc, add) unique(c(acc, add[!is.na(add)])), lapply(prices.from.cache, function(x) x$date)))
  single.df <- cbind(dates, data.frame(lapply(prices.from.cache, function(x) x$PX_LAST[match(dates, x$date)]), check.names = FALSE))
  single.df[, -1] <- zoo::na.locf(single.df[, -1], na.rm = FALSE, maxgap = 5)
  write.csv(single.df, "combined.csv", row.names = FALSE, na = "")
} else if (!exists("single.df")) {
  single.df <- read.csv("combined.csv", stringsAsFactors = FALSE, check.names = FALSE)
  single.df$dates <- as.Date(single.df$dates)
}
