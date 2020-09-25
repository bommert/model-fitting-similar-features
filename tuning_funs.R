eps.constr = function(acc, stab, p.acc = 0.025, p.stab = 0.1) {
  cho = rep(TRUE, length(acc))
  cho[is.na(stab)] = FALSE
  best.acc = max(acc[cho])
  cho[cho] = acc[cho] >= best.acc - p.acc
  best.stab = max(stab[cho])
  cho[cho] = stab[cho] >= best.stab - p.stab
  best.acc.end = max(acc[cho])
  cho = (acc == best.acc.end) & cho
  best.stab.end = max(stab[cho])
  cho = (stab == best.stab.end) & cho
  res = as.numeric(cho)
  return(res)
}

removeDuplicates = function(res, by.cols) {
  dup = res[, .N, by = by.cols]
  dup = dup[N > 1, ]

  if (nrow(dup) > 0) {
    dup = cbind(dup, dup.id = 1:nrow(dup))
    res2 = cbind(res, id = 1:nrow(res))
    all = merge(dup, res2)

    all.split = split(all, all$dup.id)
    rem = lapply(all.split, function(part) {
      ids = part$id
      sample(ids, length(ids) - 1)
    })
    rem = unlist(rem)
    res = res[-rem, ]
  }
  return(res)
}


tuning.single = function(by.cols, select.cols) {
  result.part = result[algo.name == "tuning", ]
  best = result.part[, list(acc.tune = max(acc.tune)), by = by.cols]
  res = merge(best, result.part)
  res = subset(res, select = c(select.cols, "maxSuppSize"))
  res = removeDuplicates(res, by.cols)
  cbind(res, measure = "acc")
}

tuning.multi = function(measure, by.cols, select.cols) {
  mt = paste0(measure, ".tune")
  tmp = result[algo.name == "tuning", ]
  tmp[, ec := eps.constr(acc.tune, get(mt)), by = by.cols]
  best = tmp[, list(ec = max(ec, na.rm = TRUE)), by = by.cols]
  res = merge(best, tmp)
  res = subset(res, select = c(select.cols, "maxSuppSize"))
  res = removeDuplicates(res, by.cols)
  cbind(res, measure = measure)
}

tuning.stab.l0 = function(by.cols, select.cols) {
  result.part = result[algo.name == "stabilitySelL0",]
  best =  result.part[, list(acc.tune = max(acc.tune, na.rm = TRUE)), by = by.cols]
  res = merge(best, result.part)
  res = subset(res, select = c(select.cols, "cutoff", "pfer"))
  res = removeDuplicates(res, by.cols)
  cbind(res, measure = "stabilitySelectionL0")
}

truth = function(select.cols) {
  res = result[algo.name == "truth", select.cols, with = FALSE]
  cbind(res, measure = "truth")
}
