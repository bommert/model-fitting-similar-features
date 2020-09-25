library(ggplot2)
library(data.table)

load("sd_results_tuned.RData")

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

results$measure = factor(results$measure,
  levels = c("intersectionCount", "unadjusted", "acc", "stabilitySelectionL0", "truth"))
levels(results$measure) = c("adj", "unadj", "acc", "stabs", "truth")

results$block.size = paste("Block Size:", results$block.size)
results$block.size = factor(results$block.size,
  levels = paste("Block Size:", c(1, 5, 15, 25)))

colnames(results)[colnames(results) == "acc.test"] = "Test Accuracy"
colnames(results)[colnames(results) == "false.positive"] = "False Positive"
colnames(results)[colnames(results) == "false.negative"] = "False Negative"

results.melt = melt(results, measure.vars = c("Test Accuracy", "False Positive", "False Negative"))

setup = results[, .N, by = c("p", "n", "matrix.type")]

ggs = lapply(1:nrow(setup), function(i) {
  dat = results.melt[p == setup$p[i] & n == setup$n[i] & matrix.type == setup$matrix.type[i], ]
  gg = ggplot(data = dat,
    mapping = aes(x = measure, y = value, color = measure)) +
    geom_boxplot(lwd = 0.75) +
    facet_grid(variable ~ block.size, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Approach") + ylab(element_blank()) +
    ggtitle(paste("n = 100, p =", setup$p[i]))

  if (length(unique(dat$measure)) == 4) {
    gg = gg + scale_color_manual(values = gg_color_hue(5)[c(1:3, 5)])
  }
  return(gg)
})

pdf("sd.pdf", height = 3.75, width = 8.3)
a = lapply(ggs, print)
dev.off()
