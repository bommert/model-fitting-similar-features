library(batchtools)
load("rins.RData")
load("simmats.RData")

makeExperimentRegistry("rd", packages = c("mlr", "stabm", "stabs"))

############### Problems ##########################################

extractOuterIter = function(job, data, outer.iter) {
  list(
    task.test = data$rins$outer.test[[outer.iter]],
    task.train = data$rins$outer.train[[outer.iter]],
    rin.train = data$rins$inner[[outer.iter]],
    sim.mat = data$sim.mats[[outer.iter]]
  )
}

for (i in seq_along(rins)) {
  addProblem(name = names(rins)[i], seed = i,
    data = list(rins = rins[[i]], sim.mats = sim.mats[[i]]),
    fun = extractOuterIter
  )
}

################### Algorithms #######################################

tuning = function(job, data, instance, maxSuppSize) {
  source("RLearner_classif_L0Learn.R")

  lrn = makeLearner("classif.L0Learn", maxSuppSize = maxSuppSize, algorithm = "CDPSI")
  res = resample(lrn, instance$task.train, instance$rin.train, models = FALSE,
    extract = extractSelectedFeatures, measures = list(acc))
  acc.tune = res$aggr

  # stability
  feats.tune = res$extract
  rm(res)

  p = getTaskNFeats(instance$task.train)
  stability = list(
    unadjusted.tune = stabilityUnadjusted(feats.tune, p = p, impute.na = 0),
    intersectionCount.tune =  stabilityIntersectionCount(feats.tune, sim.mat = instance$sim.mat, impute.na = 0)
  )

  nfeats = lengths(feats.tune)
  imputed = (min(nfeats) == 0) || (sort(nfeats, decreasing = TRUE)[2] == p)

  mod = train(lrn, instance$task.train)
  feats = extractSelectedFeatures(mod)

  p.train = predict(mod, instance$task.train)
  acc.train = mean(p.train$data$response == p.train$data$truth)

  p.test = predict(mod, instance$task.test)
  acc.test = mean(p.test$data$response == p.test$data$truth)

  result = list(acc.tune = acc.tune, feats = feats, acc.train = acc.train, acc.test = acc.test,
    imputed = imputed, feats.tune = feats.tune)
  result = c(result, stability)
  return(result)
}

stabilitySelL0 = function(job, data, instance) {

  source("RLearner_classif_L0Learn.R")
  lrn = makeLearner("classif.logreg")

  cutoff = runif(1, min = 0.6, max = 0.9)
  pfer = runif(1, min = 0.1, max = 10)

  l0 = function(x, y, q, ...) {
    lrn = makeLearner("classif.L0Learn", maxSuppSize = q, algorithm = "CDPSI")
    task = makeClassifTask(data = cbind(data.frame(x), y = as.factor(y)), target = "y")
    mod = train(lrn, task)
    feats = extractSelectedFeatures(mod)
    selected = getTaskFeatureNames(task) %in% feats
    names(selected) = getTaskFeatureNames(task)
    return(list(selected = selected))
  }

  f = function(train, test) {
    dat = getTaskData(train, target.extra = TRUE)
    s = stabsel(x = dat$data, y = dat$target, fitfun = l0, PFER = pfer, cutoff = cutoff)
    feats = names(s$selected)

    train.part = subsetTask(train, features = feats)
    test.part = subsetTask(test, features = feats)
    mod = train(lrn, train.part)

    p.train = predict(mod, train.part)
    acc.train = mean(p.train$data$response == p.train$data$truth)

    p.test = predict(mod, test.part)
    acc.test = mean(p.test$data$response == p.test$data$truth)

    return(list(feats = feats, acc.train = acc.train, acc.test = acc.test))
  }

  # resampling
  accs = sapply(seq_along(instance$rin.train$train.inds), function(i) {
    train = subsetTask(instance$task.train, subset = instance$rin.train$train.inds[[i]])
    test = subsetTask(instance$task.train, subset = instance$rin.train$test.inds[[i]])
    f(train, test)$acc.test
  })

  acc.tune = mean(accs)

  ret = f(instance$task.train, instance$task.test)

  result = list(feats = ret$feats, acc.tune = acc.tune,
    acc.train = ret$acc.train, acc.test = ret$acc.test,
    pfer = pfer, cutoff = cutoff)
  return(result)
}

addAlgorithm(name = "tuning", fun = tuning)
addAlgorithm(name = "stabilitySelL0", fun = stabilitySelL0)


#################### Experiments ######################################

# Designs

design.prob = data.frame(
  outer.iter = 1:10
)

design.tuning = data.table(
  maxSuppSize = 1:50 * 2
)

design.stab = data.frame()

prob.designs = lapply(seq_along(sim.mats), function(i) design.prob)
names(prob.designs) = names(sim.mats)


addExperiments(
  algo.designs = list(tuning = design.tuning),
  prob.designs = prob.designs
)

addExperiments(
  algo.designs = list(stabilitySelL0 = design.stab),
  prob.designs = prob.designs,
  repls = 50
)
