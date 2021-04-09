#' @title Mass-weighted Urn Randomisation Model
#'
#' @description A mass-weighted urn randomisation model class
#'
#' @name MassWeightedUrnRand
#' @return Returns R6 object of class MassWeightedUrnRand
#' @export
MassWeightedUrnRand <- R6::R6Class("MassWeightedUrnRand",
  inherit = RandModel,
  public = list(

    #' @field alpha Imbalance control parameter
    alpha = 5,
    #' @field mass The current mass vector
    mass = NULL,

    #' @description
    #' Creates a new instance of `MassWeightedUrnRand` class.
    #'
    #' @param target The target allocation ratio
    #' @param history The model history
    #' @return A new `MassWeightedUrnRand` object.
    initialize = function(
      target = NULL,
      history = NULL,
      rng_seed = NULL,
      alpha = 5) {
      super$initialize(
        "MassWeightedUrnRand",
        "MWUR",
        "Mass-weighted urn randomisation",
        target,
        history,
        rng_seed)
      self$alpha <- alpha
      self$mass  <- alpha * target / sum(target)
    },

    get_conditional_prob = function() {
      n <- self$num_allocations()
      w <- self$target / sum(self$target)
      p <- pmax(self$alpha * w - n + (sum(n) + 1)*w, 0)
      return(p / sum(p))
    },

    randomise = function(n) {
      # Persist existing RNG into R session
      if(!exists(".Random.seed", .GlobalEnv)) set.seed(NULL)
      assign(".Random.seed", self$get_rng(), envir = .GlobalEnv)
      # Generate allocations
      y <- numeric(n)
      for(i in 1:n) {
        p <- self$get_conditional_prob()
        u <- runif(1)
        y[i] <- findInterval(u, cumsum(c(0, p)))
        self$history <- y[i]
      }
      # Save the updated RNG state and history
      private$.rng <- .GlobalEnv$.Random.seed
      y
    }
  )
)
