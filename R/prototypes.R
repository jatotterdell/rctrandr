#' Complete randomisation
#'
#' Standard simple randomisation
#'
#' @param target_alloc The target allocation ratios
#' @param sample_size The number of allocations to generate
#' @return A list
complete_rand <- function(
  target_alloc,
  sample_size
) {
  # Normalise weights
  w <- target_alloc / sum(target_alloc)
  cw <- c(0, cumsum(w))
  # Random numbers
  rand_num <- runif(sample_size)
  # Treatment allocations
  trt <- findInterval(rand_num, c(0, cumsum(w)))
  # Imbalance
  n <- matrix(0, sample_size + 1, length(w))
  d <- numeric(sample_size)
  # Predictability
  g <- 0
  for(i in 1:sample_size) {
    trt[i] <- findInterval(rand_num[i], cw)
    n[i + 1, ] <- n[i, ]
    n[i + 1, trt[i]] <- n[i, trt[i]] + 1
    d[i] <- sqrt(sum((n[i + 1, ] - i*w)^2))
  }

  return(list(imbalance = d, predictability = g, sample_size = n, rand_num = rand_num, trt = trt))
}

#' Restricted randomisation
#'
#' @param target_alloc
#' @param sample_size
restricted_rand <- function(
  target_alloc,
  sample_size
) {
  print("Not implemented")
}

#' Permuted block randomisation
#'
#' @param target_alloc The target allocation ratio
#' @param sample_size The number of allocations to generate
#' @param blocksize The size of each block
#' @return A list
permuted_block_rand <- function(
  target_alloc,
  sample_size,
  blocksize
) {
  # Check that blocksize is integer ratio of weight sums
  # if() {}
  # conditional <- function(i, w, b, k, n) {
  #   k <- floor((i - 1) / b)
  #   (w * b + w * b * k - n) / (b + b * k - (i - 1))
  # }
  arms <- length(target_alloc)
  # Normalise weights
  w <- target_alloc / sum(target_alloc)
  R <- sum(target_alloc)
  L <- R / blocksize
  # Random numbers
  rand_num <- runif(sample_size)
  # Conditional selection probability
  p <- matrix(0, sample_size, arms)
  # Imbalance
  d <- rep(0, sample_size)
  # Allocation history
  n <- matrix(0, sample_size + 1, arms)
  # Constant history
  k <- numeric(sample_size + 1)
  # Treatment assignment
  trt <- rep(0, sample_size)
  for(i in 1:sample_size) {
    k[i] <- floor((i - 1) / blocksize)
    p[i, ] <- (w * blocksize + w * blocksize * k[i] - n[i, ]) / (blocksize + blocksize * k[i] - (i - 1))
    trt[i] <- findInterval(rand_num[i], c(0, cumsum(p[i, ])))
    n[i + 1, ] <- n[i, ]
    n[i + 1, trt[i]] <- n[i, trt[i]] + 1
    d[i] <- sqrt(sum((n[i + 1, ] - i*w)^2))
  }
  return(list(
    imbalance = d,
    rand_num = rand_num,
    trt = trt,
    sample_size = n[-1, ],
    selection_prob = p))
}


#' Block urn randomisation
#'
#' @param target_alloc The target allocation ratio
#' @param sample_size The number of allocations to generate
#' @param blocksize The size of each block
#' @return A list
#' @references Zhao, W., & Weng, Y. (2011). Block urn design—a new randomization algorithm for sequential trials with two or more treatments and balanced or unbalanced allocation. Contemporary clinical trials, 32(6), 953-961.
block_urn_rand <- function(
  target_alloc,
  sample_size,
  blocksize
) {
  # RUN ARGUMENT CHECKS...
  arms <- length(target_alloc)
  # Normalise weights
  w <- target_alloc / sum(target_alloc)
  W <- sum(target_alloc)
  L <- R / blocksize
  # Random numbers
  rand_num <- runif(sample_size)
  # Conditional selection probability
  p <- matrix(0, sample_size + 1, arms)
  # Imbalance
  d <- rep(0, sample_size)
  # Allocation history
  n <- matrix(0, sample_size + 1, arms)
  # Constant history
  k <- numeric(sample_size + 1)
  # Treatment assignment
  trt <- rep(0, sample_size)
  for(i in 1:sample_size) {
    k[i] <- min(floor(n[i, ] / target_alloc))
    p[i, ] <- (w * blocksize + W * w * k[i] - n[i, ]) / (blocksize + W * k[i] - (i - 1))
    trt[i] <- findInterval(rand_num[i], c(0, cumsum(p[i, ])))
    n[i + 1, ] <- n[i, ]
    n[i + 1, trt[i]] <- n[i, trt[i]] + 1
    d[i] <- sqrt(sum((n[i + 1, ] - i*w)^2))
  }
  return(list(
    imbalance = d,
    rand_num = rand_num,
    trt = trt,
    sample_size = n[-1, ],
    selection_prob = p))
}


wei_urn_rand <- function(
  n_arms = 4,
  sample_size = 100,
  w = 1,
  alpha = 0,
  beta = 2
) {
  # Sample size
  n <- matrix(0, sample_size + 1, n_arms)
  # Random number
  y <- runif(sample_size)
  # Conditional selection probability
  p <- matrix(0, sample_size, n_arms)
  # Treatment assignment
  trt <- rep(0, sample_size)
  for(i in 1:sample_size) {
    p[i, ] <- (w + alpha*n[i, ] + beta*(i - 1 - n[i, ])) / (w*n_arms + alpha*(i - 1) + beta*(i - 1)*(n_arms - 1))
    # Generate allocation
    trt[i] <- findInterval(y[i], c(0, cumsum(p[i, ])))
    # Update sample sizes
    n[i + 1, ] <- n[i, ]
    n[i + 1, trt[i]] <- n[i + 1, trt[i]] + 1
  }
  return(
    list(
      rand_num = y,
      trt = trt,
      sample_size = n,
      selection_prob = p
    )
  )
}



#' Modified Wei's Urn Design
#'
#' @param target_alloc The target allocation ratios
#' @param sample_size The number of allocations to generate
#' @return A list
modified_wei_urn_rand <- function(
  target_alloc,
  sample_size,
  alpha,
  beta
) {

}


#' @references Zhao, W., & Ramakrishnan, V. (2016). Generalization of Wei’s urn design to unequal allocations in sequential clinical trials. Contemporary Clinical Trials Communications, 2, 75–79. https://doi.org/10.1016/j.conctc.2015.12.007
modified_urn_pa <- function() {

}

#' Modified Urn Design with Equal Allocation
#'
#' @description
#' A generalization of Wei's urn design for unequal allocations.
#'
#' @param target_alloc The target allocation ratio.
#'
#' @references Zhao, W., & Ramakrishnan, V. (2016). Generalization of Wei’s urn design to unequal allocations in sequential clinical trials. Contemporary Clinical Trials Communications, 2, 75–79. https://doi.org/10.1016/j.conctc.2015.12.007
modified_urn_ea <- function(
  target_alloc,
  sample_size,
  alpha = 0,
  beta = 2
) {
  M <- sum(target_alloc)
  if(!(M > 1)) stop("sum(target_alloc) must be > 1.")
  arms <- length(target_alloc)
  prob_alloc <- target_alloc / sum(target_alloc)
  # Sample size
  n <- matrix(0, sample_size + 1, arms)
  # Random number
  y <- runif(sample_size)
  # Conditional selection probability
  p <- matrix(0, sample_size, arms)
  # Treatment assignment
  trt <- rep(0, sample_size)
  for(i in 1:sample_size) {
    p[i, ] <- (target_alloc + alpha * n[i, ] + beta * target_alloc * (i - 1 - n[i, ]) + beta * n[i, ] * (target_alloc - 1)) /
                (M + alpha * (i - 1) + beta * (M - 1) * ( i - 1))
    # Generate allocation
    trt[i] <- findInterval(y[i], c(0, cumsum(p[i, ])))
    # Update sample sizes
    n[i + 1, ] <- n[i, ]
    n[i + 1, trt[i]] <- n[i + 1, trt[i]] + 1
  }
  return(
    list(
      rand_num = y,
      trt = trt,
      sample_size = n,
      selection_prob = p
    )
  )
}

#' Mass-weighted urn randomisation
#'
#' @param target_alloc The target allocation ratios
#' @param sample_size The number of allocations to generate
#' @param alpha Parameter to control imbalance between arms
#' @return A list detailing the mass-weighted-urn process.
#' @references Zhao (2015) Mass weighted urn design--A new randomization algorithm for unequal allocations. Contemporary clinical trials, 43, 209–216. https://doi.org/10.1016/j.cct.2015.06.008
mass_weighted_urn_rand <- function(
  target_alloc,
  sample_size,
  alpha = 4
) {
  arms <- length(target_alloc)
  prob_alloc <- target_alloc / sum(target_alloc)
  # Masses
  x <- matrix(0, sample_size + 1, arms)
  x[1, ] <- alpha * prob_alloc
  # Sample size
  n <- matrix(0, sample_size + 1, arms)
  # Random number
  y <- runif(sample_size)
  # Conditional selection probability
  p <- matrix(0, sample_size + 1, arms)
  # Imbalance
  d <- rep(0, sample_size)
  # Allocation Predictability
  g <- rep(0, sample_size + 1)
  # Treatment assignment
  trt <- rep(0, sample_size)

  imbalance_cap <- sqrt(sum(((alpha - 1)*(1 - prob_alloc) + (arms - 1))^2))

  for(i in 2:(sample_size + 1)) {
    # Update allocation probabilities
    p[i - 1, ] <- pmax(alpha * prob_alloc - n[i - 1, ] + (i - 1)*prob_alloc, 0)
    p[i - 1, ] <- p[i - 1, ] / sum(p[i - 1, ])
    trt[i-1] <- findInterval(y[i - 1], c(0, cumsum(p[i - 1, ])))
    # Update sample sizes
    n[i, ] <- n[i - 1, ]
    n[i, trt[i-1]] <- n[i, trt[i-1]] + 1
    # Update urn masses
    x[i, trt[i-1]] <- x[i - 1, trt[i-1]] - 1 + prob_alloc[trt[i-1]]
    x[i, -trt[i-1]] <- x[i - 1, -trt[i-1]] + prob_alloc[-trt[i-1]]
    # Calculate imbalance
    d[i - 1] <- sqrt(sum((n[i, ] - (i - 1)*prob_alloc)^2))
    # Calculate allocation predictability
    g[i] <- d[i - 1] / alpha
  }
  return(list(
    max_imbalance_bound = imbalance_cap,
    imbalance = d,
    alloc_predict = g,
    rand_num = y,
    trt = trt,
    mass = x,
    sample_size = n,
    selection_prob = p))
}


