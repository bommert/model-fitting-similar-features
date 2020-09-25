library(OpenML)
library(BBmisc)
library(mlr)
library(data.table)
library(datamicroarray)
library(Matrix)

data.ids = c(1066, 40, 851, 1038, 1478, 41163, 1484, 1458, 1233)

datasets = lapply(data.ids, getOMLDataSet)
datasets = lapply(datasets, convertOMLDataSetToMlr)
names(datasets) = chosen.oml

data.subset.classes = function(task, classes) {
  target = getTaskData(task, target.extra = TRUE)$target
  keep = target %in% classes
  data = getTaskData(task)
  data = data[keep, ]
  data[[getTaskTargetNames(task)]] = droplevels(data[[getTaskTargetNames(task)]])
  ret = makeClassifTask(id = getTaskId(task), data = data, target = getTaskTargetNames(task))
  return(ret)
}

datasets$har = data.subset.classes(datasets$har, c("4", "5"))
datasets$dilbert = data.subset.classes(datasets$dilbert, c("1", "3"))
datasets$eating = data.subset.classes(datasets$eating, c("Apple", "Nectarine"))

christensen = function() {
  data("christensen")
  y = christensen$y
  y[y == "placenta"] = "other"
  y = droplevels(y)
  y2 = factor(y, levels = c("blood", "other"))
  data = as.data.frame(christensen$x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = y))
  task = makeClassifTask(id = "christensen", data = data, target = "y")
  return(task)
}

gravier = function() {
  data("gravier")
  data = as.data.frame(gravier$x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = gravier$y))
  task = makeClassifTask(id = "gravier", data = data, target = "y")
  return(task)
}

chiaretti = function() {
  library(ALL)
  data("ALL")

  x = t(exprs(ALL))
  x = log(x)
  y = as.character(ALL$BT)
  y[grep("B", y)] = "B"
  y[grep("T", y)] = "T"

  data = as.data.frame(x)
  colnames(data) = paste0("V", 1:ncol(data))
  data = cbind(data, data.frame(y = y))
  task = makeClassifTask(id = "chiaretti", data = data, target = "y")
  return(task)
}

datasets$christensen = christensen()
datasets$gravier = gravier()
datasets$chiaretti = chiaretti()

datasets = lapply(datasets, removeConstantFeatures)

# rename classes
renameClasses = function(task, old.class.names, new.class.names) {
  dat = getTaskData(task)
  levs = levels(dat[[getTaskTargetNames(task)]])

  if (!identical(levs, old.class.names)) {
    if (identical(levs, old.class.names[2:1])) {
      new.class.names = new.class.names[2:1]
    } else {
      warning("Old class names did not match!")
      return(task)
    }
  }

  levels(dat[[getTaskTargetNames(task)]]) = new.class.names
  pos = new.class.names[which(levs == task$task.desc$positive)]
  task2 = makeClassifTask(id = getTaskId(task), data = dat,
    target = getTaskTargetNames(task), positive = pos)
  return(task2)
}

datasets$`kc1-binary` = renameClasses(datasets$`kc1-binary`, c("_TRUE", "FALSE"), c("Defective", "Working"))
datasets$tecator = renameClasses(datasets$tecator, c("P", "N"), c("Positive", "Negative"))
datasets$har = renameClasses(datasets$har, c("4", "5"), c("Sitting", "Standing"))
datasets$lsvt = renameClasses(datasets$lsvt, c("1", "2"), c("Acceptable", "Unacceptable"))
datasets$christensen = renameClasses(datasets$christensen, c("blood", "other"), c("Blood", "Other"))
datasets$gravier = renameClasses(datasets$gravier, c("good", "poor"), c("Good", "Poor"))
datasets$arcene = renameClasses(datasets$arcene, c("1", "2"), c("Cancer", "Control"))

# order
datasets = datasets[c("sonar", "kc1-binary", "tecator", "har", "gina_agnostic", "dilbert",
  "lsvt", "christensen", "gravier", "eating", "arcene", "chiaretti")]


set.seed(2019)

# resample instances for nested 10 fold CV
iters = 10
rdesc = makeResampleDesc("CV", iters = iters, stratify = TRUE)

makeRins = function(task) {
  # outer CV
  rin = makeResampleInstance(rdesc, task)

  outer.test = lapply(1:iters, function(i) {
    subsetTask(task, subset = rin$test.inds[[i]])
  })

  outer.train = lapply(1:iters, function(i) {
    tk = subsetTask(task, subset = rin$train.inds[[i]])
    removeConstantFeatures(tk)
  })

  # inner CV
  inner = lapply(outer.train, function(tsk) {
    makeResampleInstance(rdesc, tsk)
  })

  return(list(outer.test = outer.test, outer.train = outer.train, inner = inner))
}

rins = lapply(datasets, makeRins)

save(rins, file = "rins.RData")



# list of sparse similarity matrices with threshold = 0.9
simMats = function(task, threshold = 0.9) {
  trafoIndex = function(index, nr) {
    i1 = ceiling(index / nr)
    i2 = index - (i1 - 1) * nr
    return(c(i1, i2))
  }

  dat = getTaskData(task, target.extra = TRUE)$data
  cc = abs(cor(dat))

  gt = which(cc >= threshold)
  gt.mat = convertListOfRowsToDataFrame(lapply(gt, trafoIndex, nr = nrow(cc)))
  w = which(gt.mat[, 1] >= gt.mat[, 2])

  sparse.mat = sparseMatrix(gt.mat[w, 1], gt.mat[w, 2], x = cc[gt[w]], symmetric = TRUE)

  colnames(sparse.mat) = rownames(sparse.mat) = getTaskFeatureNames(task)
  return(sparse.mat)
}

sim.mats = lapply(rins, function(r) {
  lapply(r$outer.train, simMats)
})

save(sim.mats, file = "simmats.RData")
