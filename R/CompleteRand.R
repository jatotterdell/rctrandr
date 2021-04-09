#' @title Complete Randomisation Model
#'
#' @description A complete randomisation model class
#'
#' @name CompleteRand
#' @return Returns R6 object of class CompleteRand
#' @export
CompleteRand <- R6::R6Class("CompleteRand",
  inherit = RandModel,
  public = list(

    #' @description
    #' Creates a new instance of `CompleteRand` class.
    #'
    #' @param target The target allocation ratio
    #' @param history The model history
    #' @param rng_seed The RNG initial seed
    #' @return A new `CompleteRand` object.
    initialize = function(
      target = NULL,
      history = NULL,
      rng_seed = NULL) {
        super$initialize(
          "CompleteRand",
          "CR",
          "Complete randomisation",
          target,
          history,
          rng_seed)
      },

    #' @description
    #' Return the conditional probability vector.
    #'
    #' @return A numeric vector giving the current conditional probability of assignment.
    get_conditional_prob = function() {
      return( self$target / sum(self$target) )
    },

    #' @description
    #' Generate `n` randomisations from the model
    #'
    #' @param n The number of randomisations to generate
    #' @return An integer vector giving the treatment number for each randomisation.
    randomise = function(n) {
      # Persist existing RNG
      if(!exists(".Random.seed", .GlobalEnv)) set.seed(NULL)
      assign(".Random.seed", self$get_rng(), envir = .GlobalEnv)
      # Generate allocations
      p <- self$get_conditional_prob()
      u <- runif(n)
      y <- findInterval(u, cumsum(c(0, p)))
      # Save the updated RNG state and history
      private$.rng <- .GlobalEnv$.Random.seed
      self$history <- y
      y
    }
  )
)
