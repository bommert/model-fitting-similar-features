library(ggplot2)
library(data.table)
load("rd_results_tuned.RData")

results$prob.name = factor(results$prob.name,
  levels = c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
    "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti"))

ms1 = c("intersectionCount", "unadjusted", "acc", "stabilitySelectionL0")
ms2 = c("adj", "unadj", "acc", "stabs")
results$measure = factor(results$measure, levels = ms1)
levels(results$measure) = ms2

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


pdf("rd.pdf", height = 5, width = 7.7)
ggplot(data = results, mapping = aes(x = measure, y = acc.test, color = measure)) +
  geom_boxplot(lwd = 0.75) +
  facet_wrap("prob.name", scales = "free", nrow = 4) +
  theme_bw() +
  scale_color_manual(values = gg_color_hue(5)[1:4]) +
  theme(legend.position = "none") +
  xlab("Approach") + ylab("Classification Accuracy")

ggplot(data = results, mapping = aes(x = measure, y = nfeats, color = measure)) +
  geom_boxplot(lwd = 0.75) +
  facet_wrap("prob.name", scales = "free", nrow = 4) +
  theme_bw() +
  scale_color_manual(values = gg_color_hue(5)[1:4]) +
  theme(legend.position = "none") +
  xlab("Approach") + ylab("Number of Selcted Features")
dev.off()
