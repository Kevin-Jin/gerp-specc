# TODO: in a sense, the clustering is a training set for the trading model, which uses the lag structures.
# Don't trade on the training set. Reserve 2 years for the validation set and train the lag structures
# on the 5 years preceding the validation set.
# TODO: train the model in the years preceding 2008 and validate on 2008. Train the model on 2008
# and validate past 2008. Train the model in the years preceding 2000 and traing on 2000.
# Train the model on 2000 and validate past 2000.

setwd(dirname(parent.frame(2)$ofile))

source("2.merge_df.R")

if (!exists("industry.groups.from.cache"))
  industry.groups.from.cache <- load.industry.groups.from.cache()

# Get the securities in each group.
#groups <- split(industry.groups.from.cache, industry.groups.from.cache$GICS_INDUSTRY_GROUP %/% 100)
groups <- split(industry.groups.from.cache, industry.groups.from.cache$GICS_INDUSTRY_GROUP_NAME)
groups <- lapply(groups, function(group) single.df[c("dates", rownames(group))])

pdf(file = "industry_group_plots.pdf", title = "Industry groups", width = 22, height = 17)
groups <- setNames(lapply(names(groups), function(group.name) {
  group <- groups[[group.name]]
  # Filter to 3000 most recent trading days.
  group <- tail(group, 3000)
  # Filter to companies that have been public for the entire time period.
  group <- group[, as.numeric(group[1, ]) != 0]
  
  # Perform standardization to make the price data unitless and comparable.
  group[, -1] <- apply(group[, -1], 2, function(col) (col - mean(col)) / sd(col))
  # Average the unitless price data.
  group <- cbind(group, rowMeans(group[, -1]))
  
  # Identify the outlier time series.
  correlations <- apply(group[, -c(1, ncol(group))], 2, function(col) cor(col, group[, ncol(group)]))
  # Defined as having a low absolute correlation and in bottom decile for the group.
  outliers <- which(correlations < 0.5 & correlations < quantile(correlations, probs = 0.1))
  if (length(outliers))
    print(paste(group.name, paste(names(outliers), collapse = ", "), sep = ": "))
  
  # Leave out the date column and highlight the average (last column).
  colors <- c(rep(3:8, length.out = ncol(group) - 2), 1)
  line.patterns <- c(rep(1:5, length.out = ncol(group) - 2), 1)
  line.widths <- c(rep(1, length.out = ncol(group) - 2), 3)
  # Highlight the outliers in the plot.
  colors[outliers] <- 2
  line.patterns[outliers] <- 1
  line.widths[outliers] <- 3
  
  # Transform to log returns.
#  group[, -1] <- rbind(NA, log(group[-1, -1] / group[-nrow(group), -1]))
  matplot(group[, 1], group[, -1], type = "l", col = colors, lty = line.patterns, lwd = line.widths, xlab = "Date", ylab = "Z-transformed Price", main = group.name, xaxt = "n")
  # Label the dates.
  axis(1, at = seq(min(group[, 1]), max(group[, 1]), by = "quarter"), labels = format(seq(min(group[, 1]), max(group[, 1]), by = "quarter"), "%m/%y"))
  
  # Leave out outliers and average column.
  group[-c(outliers + 1, ncol(group))]
}), names(groups))
dev.off()

pdf(file = "industry_group_plots_filtered.pdf", title = "Industry groups", width = 22, height = 17)
lapply(names(groups), function(group.name) {
  group <- groups[[group.name]]
  matplot(group[, 1], group[, -1], type = "l", xlab = "Date", ylab = "Z-transformed Price", main = group.name, xaxt = "n")
  # Label the dates.
  axis(1, at = seq(min(group[, 1]), max(group[, 1]), by = "quarter"), labels = format(seq(min(group[, 1]), max(group[, 1]), by = "quarter"), "%m/%y"))
})
dev.off()

print(paste("Companies remaining:", sum(unlist(lapply(groups, ncol)) - 1)))
