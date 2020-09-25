res = reduceResultsList(findDone(), fun = function(job, res) {
  if (job$algo.name == "tuning") {
    data.table(
      job.id = job$job.id,
      algo.name = job$algo.name,
      prob.name = job$prob.name,
      outer.iter = job$prob.pars$outer.iter,
      maxSuppSize = job$algo.pars$maxSuppSize,
      acc.tune = res$acc.tune,
      acc.test = res$acc.test,
      acc.train = res$acc.train,
      feats = list(res$feats),
      unadjusted.tune = res$unadjusted.tune,
      intersectionCount.tune = res$intersectionCount.tune)
  } else {
    data.table(
      job.id = job$job.id,
      algo.name = job$algo.name,
      prob.name = job$prob.name,
      outer.iter = job$prob.pars$outer.iter,
      acc.tune = res$acc.tune,
      acc.test = res$acc.test,
      acc.train = res$acc.train,
      feats = list(res$feats),
      pfer = res$pfer,
      cutoff = res$cutoff)
  }
})

result = dplyr::bind_rows(res)

save(result, file = "rd_results.RData")
