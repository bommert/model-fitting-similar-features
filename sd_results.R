res = reduceResultsList(ids, fun = function(job, res) {
  if (job$algo.name == "tuning") {
    data.table(
      job.id = job$job.id,
      algo.name = job$algo.name,
      prob.name = job$prob.name,
      data.seed = job$prob.pars$data.seed,
      n = job$prob.pars$n,
      p = job$prob.pars$p,
      block.size = job$prob.pars$block.size,
      matrix.type = job$prob.pars$matrix.type,
      maxSuppSize = job$algo.pars$maxSuppSize,
      acc.tune = res$acc.tune,
      acc.test = res$acc.test,
      acc.train = res$acc.train,
      false.positive = res$false.positive,
      false.negative = res$false.negative,
      feats = list(res$feats),
      unadjusted.tune = res$unadjusted.tune,
      intersectionCount.tune = res$intersectionCount.tune)
  } else if (job$algo.name == "truth") {
    data.table(
      job.id = job$job.id,
      algo.name = job$algo.name,
      prob.name = job$prob.name,
      data.seed = job$prob.pars$data.seed,
      n = job$prob.pars$n,
      p = job$prob.pars$p,
      block.size = job$prob.pars$block.size,
      matrix.type = job$prob.pars$matrix.type,
      acc.test = res$acc.test,
      acc.train = res$acc.train,
      false.positive = res$false.positive,
      false.negative = res$false.negative,
      feats = list(res$feats))
  } else if (job$algo.name == "stabilitySelL0") {
    data.table(
      job.id = job$job.id,
      algo.name = job$algo.name,
      prob.name = job$prob.name,
      data.seed = job$prob.pars$data.seed,
      n = job$prob.pars$n,
      p = job$prob.pars$p,
      block.size = job$prob.pars$block.size,
      matrix.type = job$prob.pars$matrix.type,
      acc.tune = res$acc.tune,
      acc.test = res$acc.test,
      acc.train = res$acc.train,
      false.positive = res$false.positive,
      false.negative = res$false.negative,
      feats = list(res$feats),
      pfer = res$pfer,
      cutoff = res$cutoff)
  }
})

result = dplyr::bind_rows(res)
save(result, file = "sd_results.RData")
