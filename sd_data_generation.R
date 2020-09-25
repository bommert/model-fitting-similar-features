dataGen = function(seed, matrix.type, p, n, block.size) {
  rs = .Random.seed
  on.exit(.Random.seed <<- rs)
  myseed = round(pi * seed * 100)
  set.seed(myseed)

  n.inds = 5

  if (matrix.type == "blocks") {
    mat = matrix(0.1, ncol = p, nrow = p)

    if (block.size > 1) {
      # only use block.sizes such that p is a multiple of block.size
      n.blocks = ceiling(p / block.size)
      for (i in seq_len(n.blocks)) {
        kl = (i - 1) * block.size + 1
        ku = min(kl + block.size - 1, p)
        mat[kl:ku, kl:ku] = 0.95
      }
    }

    diag(mat) = 1
    inds = (1:n.inds - 1) * block.size + floor(block.size / 2) + 1
  } else {
    # only odd block.sizes are possible
    if (block.size > 1) {
      r = 0.9 ^ (1 / ((block.size - 1) / 2))
    } else {
      r = 0.5
    }

    mat = matrix(1, ncol = p, nrow = p)

    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        mat[i, j] = mat[j, i] = r ^ abs(i - j)
      }
    }

    inds = round(seq((block.size + 1), (p - block.size), length.out = n.inds))
  }

  dat = rmvnorm(n = 2 * n, sigma = mat)
  colnames(dat) = paste0("V", 1:p)

  xb = rowSums(dat[, inds, drop = FALSE])
  e.xb = exp(xb)
  pis = e.xb / (1 + e.xb)
  y = sapply(pis, function(pii) rbinom(1, size = 1, prob = pii))
  data = cbind(as.data.frame(dat), y = y)

  train = data[1:n, ]
  test = data[(n + 1):(2 * n), ]

  correct.features = paste0("V", inds)

  sim.mat = abs(cor(dat[1:n, ]))
  colnames(sim.mat) = rownames(sim.mat) = colnames(dat)

  return(list(train = train, test = test, sim.mat = sim.mat, correct.features = correct.features))
}

