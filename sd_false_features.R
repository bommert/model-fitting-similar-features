library(stabm)

falseFeatures = function(features, correct.features, sim.mat) {

  len = length(features)
  if (len > 0) {
    mbm = stabilityIntersectionCount(
      features = list(features, correct.features), sim.mat = sim.mat,
      correction.for.chance = "none")
  } else {
    mbm = 0
  }
  fp = len - mbm
  fn = length(correct.features) - mbm
  return(list(false.positive = fp, false.negative = fn))
}
