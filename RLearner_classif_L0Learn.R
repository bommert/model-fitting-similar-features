#' @export
makeRLearner.classif.L0Learn = function() {
  makeRLearnerClassif(
    cl = "classif.L0Learn",
    package = "L0Learn",
    par.set = makeParamSet(
      # makeDiscreteLearnerParam(id = "penalty", values = c("L0", "L0L2", "L0L1"),
      #   default = "L0", tuneable = FALSE),
      makeDiscreteLearnerParam(id = "algorithm", values = c("CD", "CDPSI"),
        default = "CD", tunable = FALSE),
      makeIntegerLearnerParam(id = "maxSuppSize", lower = 1, default = 100),
      makeIntegerLearnerParam(id = "nLambda", lower = 1, default = 100),
      makeIntegerLearnerParam(id = "maxIters", lower = 1, default = 200),
      makeNumericLearnerParam(id = "tol", lower = 0, default = 1e-6)
    ),
    properties = c("twoclass", "numerics"),
    name = "Logistic regression with L0 penalty",
    short.name = "L0Learn",
    callees = "L0Learn"
  )
}

#' @export
trainLearner.classif.L0Learn = function(.learner, .task, .subset, .weights = NULL, ...) {
  z = getTaskData(.task, .subset, target.extra = TRUE, recode.target = "-1+1")
  L0Learn::L0Learn.fit(x = as.matrix(z$data), y = z$target, loss = "Logistic",
    penalty = "L0L2", nGamma = 1, gammaMax = 0.01, gammaMin = 0.01, ...)
}

#' @export
predictLearner.classif.L0Learn = function(.learner, .model, .newdata, lambda = NULL, ...) {
  checkmate::assertNumber(lambda, lower = 0, na.ok = FALSE, null.ok = TRUE)
  if (is.null(lambda)) {
    lambda = min(.model$learner.model$lambda[[1]])
  }

  vn = .model$learner.model$varnames
  dat = subset(.newdata, select = vn)
  dat = as.matrix(dat)

  p = predict(object = .model$learner.model, newx = dat, lambda = lambda)

  # avoid error if minimal lambda is used several times
  if (ncol(p) > 1) {
    p = p[, 1]
  }
  p = as.vector(p)

  td = .model$task.desc
  levs = c(td$negative, td$positive)
  p2 = factor(ifelse(p == -1, levs[1], levs[2]), levels = levs)

  return(p2)
}

#' @export
extractSelectedFeaturesInternal.classif.L0Learn = function(learner, model, lambda = NULL) {
  checkmate::assertNumber(lambda, lower = 0, na.ok = FALSE, null.ok = TRUE)
  if (is.null(lambda)) {
    lambda = min(model$learner.model$lambda[[1]])
  }
  coef.mat = coef(model$learner.model, lambda = lambda)

  # avoid error if minimal lambda is used several times
  if (ncol(coef.mat) > 1) {
    coef.mat = coef.mat[, 1, drop = FALSE]
  }

  indices = coef.mat@i
  feats = model$features[indices]
  return(feats)
}
