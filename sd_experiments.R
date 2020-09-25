library(batchtools)

makeExperimentRegistry("simdata",
  packages = c("mlr", "stabm", "stabs", "mvtnorm"))


####################### Problem ########################################

probGen = function(data, job, data.seed, matrix.type, p, n, block.size) {

  source("sd_data_generation.R")
  dat = dataGen(data.seed, matrix.type, p, n, block.size)

  task.train = makeClassifTask(data = dat$train, target = "y")
  task.test = makeClassifTask(data = dat$test, target = "y")

  rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)
  rin.train = makeResampleInstance(rdesc, task.train)

  return(list(sim.mat = dat$sim.mat, correct.features = dat$correct.features,
    task.train = task.train, rin.train = rin.train, task.test = task.test))
}

addProblem(name = "prob", seed = 1,
  fun = probGen
)

#################### Algorithms #########################################

tuning = function(job, data, instance, maxSuppSize) {

  source("RLearner_classif_L0Learn.R")
  source("sd_false_features.R")

  lrn = makeLearner("classif.L0Learn", maxSuppSize = maxSuppSize, algorithm = "CDPSI")
  res = resample(lrn, instance$task.train, instance$rin.train, models = FALSE,
    extract = extractSelectedFeatures, measures = list(acc))
  acc.tune = res$aggr

  # stability
  feats.tune = res$extract
  rm(res)

  stability = list(
    unadjusted.tune = stabilityUnadjusted(feats.tune, p = getTaskNFeats(instance$task.train), impute.na = 0),
    intersectionCount.tune =  stabilityIntersectionCount(feats.tune, sim.mat = instance$sim.mat, impute.na = 0)
  )
  imputed = min(lengths(feats.tune)) == 0

  mod = train(lrn, instance$task.train)
  feats = extractSelectedFeatures(mod)

  p.train = predict(mod, instance$task.train)
  acc.train = mean(p.train$data$response == p.train$data$truth)

  p.test = predict(mod, instance$task.test)
  acc.test = mean(p.test$data$response == p.test$data$truth)

  ff = falseFeatures(feats, instance$correct.features, instance$sim.mat)

  result = list(acc.tune = acc.tune, feats = feats, acc.train = acc.train, acc.test = acc.test,
    false.positive = ff$false.positive, false.negative = ff$false.negative,
    imputed = imputed, feats.tune = feats.tune)
  result = c(result, stability)
  return(result)
}


stabilitySelL0 = function(job, data, instance) {

  source("RLearner_classif_L0Learn.R")
  source("sd_false_features.R")
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
  ff = falseFeatures(ret$feats, instance$correct.features, instance$sim.mat)

  result = list(feats = ret$feats, acc.tune = acc.tune,
    acc.train = ret$acc.train, acc.test = ret$acc.test, pfer = pfer, cutoff = cutoff,
    false.positive = ff$false.positive, false.negative = ff$false.negative)
  return(result)
}


truth = function(job, data, instance) {
  task.train = subsetTask(instance$task.train, features = instance$correct.features)
  task.test = subsetTask(instance$task.test, features = instance$correct.features)
  lrn = makeLearner("classif.logreg")
  mod = train(lrn, task.train)

  p.train = predict(mod, task.train)
  acc.train = mean(p.train$data$response == p.train$data$truth)

  p.test = predict(mod, task.test)
  acc.test = mean(p.test$data$response == p.test$data$truth)

  return(list(acc.train = acc.train, acc.test =  acc.test,
    feats = instance$correct.features, false.positive = 0, false.negative = 0))
}


addAlgorithm(name = "tuning", fun = tuning)
addAlgorithm(name = "truth", fun = truth)
addAlgorithm(name = "stabilitySelL0", fun = stabilitySelL0)


##################### Experiments ########################################

# Designs

design.prob = expand.grid(
  data.seed = 1:50,
  n = 100,
  p = c(200, 2000, 10000),
  block.size = c(1, 5, 15, 25),
  matrix.type = "blocks",
  stringsAsFactors = FALSE
)

design.tuning = data.table(
  maxSuppSize = 1:50 * 2
)

design.truth = data.frame()

design.stab = data.frame()


addExperiments(
  algo.designs = list(tuning = design.tuning),
  prob.designs = list(prob = design.prob),
  repls = 1
)

addExperiments(
  algo.designs = list(truth = design.truth),
  prob.designs = list(prob = design.prob),
  repls = 1
)

addExperiments(
  algo.designs = list(stabilitySelL0 = design.stab),
  prob.designs = list(prob = design.prob),
  repls = 50
)

