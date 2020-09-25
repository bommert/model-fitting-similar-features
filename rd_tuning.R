library(data.table)
load("rd_results.RData")
source("tuning_funs.R")

by.cols = c("prob.name", "outer.iter")
select.cols = c("prob.name", "outer.iter", "feats", "acc.test", "acc.train")

results = dplyr::bind_rows(
  tuning.single(by.cols, select.cols),
  tuning.multi("unadjusted", by.cols, select.cols),
  tuning.multi("intersectionCount", by.cols, select.cols),
  tuning.stab.l0(by.cols, select.cols)
)

results[, nfeats := lengths(feats)]

save(results, file = "rd_results_tuned.RData")
