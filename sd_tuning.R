library(data.table)
load("sd_results.RData")
source("tuning_funs.R")

by.cols = c("data.seed", "p", "n", "block.size", "matrix.type")
select.cols = c("prob.name", "feats", "acc.test", "acc.train",
  "false.positive", "false.negative", by.cols)

results = dplyr::bind_rows(
  tuning.single(by.cols, select.cols),
  tuning.multi("unadjusted", by.cols, select.cols),
  tuning.multi("intersectionCount", by.cols, select.cols),
  tuning.stab.l0(by.cols, select.cols),
  truth(select.cols)
)

results[, nfeats := lengths(feats)]

save(results, file = "sd_results_tuned.RData")
