setwd(dirname(parent.frame(2)$ofile))

source("0.external_dependencies.R")

cache.historicals.from.bbg <- function() {
  check.cran.pkg("Rblpapi")
  library(Rblpapi)
  blpConnect()
  
  if (!dir.exists("historicals"))
    dir.create("historicals", recursive = TRUE)
  
  tickers <- bds(paste(c("SPX", "Index"), collapse = " "), "INDX_MEMBERS")
  historicals <- bdh(paste(tickers[, 1], "Equity"), "PX_LAST", as.Date("1986-04-04"), as.Date("2016-04-05"))
  lapply(names(historicals), function(ticker)
    write.csv(historicals[[ticker]], paste("historicals/", gsub("/", "-", ticker), ".csv", sep = ""), row.names = FALSE)
  )
  
  historicals
}

cache.industry.groups.from.bbg <- function(tickers) {
  check.cran.pkg("Rblpapi")
  library(Rblpapi)
  blpConnect()
  
  industry.groups <- bdp(tickers, c("GICS_INDUSTRY_GROUP", "GICS_INDUSTRY_GROUP_NAME", "LONG_COMPANY_NAME_REALTIME", "NAME_RT"))
  write.csv(industry.groups, "industry_groups.csv", row.names = TRUE)
  
  industry.groups
}

load.historicals.from.cache <- function() {
  data <- lapply(list.files("historicals", "^.*\\.csv$"), function(name) {
    ticker <- gsub("-", "/", substr(name, 0, nchar(name) - 4))
    historical <- read.csv(paste("historicals/", name, sep = ""), stringsAsFactors = FALSE)
    historical$date <- as.Date(historical$date)
    list(ticker = ticker, historical = historical)
  })
  
  setNames(lapply(data, function(x) x$historical), unlist(lapply(data, function(x) x$ticker)))
}

load.industry.groups.from.cache <- function()
  read.csv("industry_groups.csv", stringsAsFactors = FALSE, row.names = 1)

validate.cache.prices <- function(from.bbg, from.cache)
  all(unlist(lapply(names(from.bbg), function(ticker)
    all.equal(from.bbg[[ticker]], from.cache[[ticker]])
  ))) && length(from.bbg) == length(from.cache)

validate.cache.industry.groups <- function(from.bbg, from.cache)
  all.equal(from.bbg, from.cache)

initialize.workspace <- function() {
  prices.from.bbg <- cache.historicals.from.bbg()
  prices.from.cache <<- load.historicals.from.cache()
  print(validate.cache.prices(prices.from.bbg, prices.from.cache))
  
  industry.groups.from.bbg <- cache.industry.groups.from.bbg(names(prices.from.bbg))
  industry.groups.from.cache <<- load.industry.groups.from.cache()
  print(validate.cache.industry.groups(industry.groups.from.bbg, industry.groups.from.cache))
}

if (!dir.exists("historicals") && !file.exists("combined.csv"))
  prices.from.cache <- cache.historicals.from.bbg()

if (!file.exists("industry_groups.csv")) {
  if (!exists("prices.from.cache"))
    prices.from.cache <- load.historicals.from.cache()
  industry.groups.from.cache <- cache.industry.groups.from.bbg(names(prices.from.cache))
}
