#' @title Mass-weighted Urn Randomisation Model
#'
#' @description A mass-weighted urn randomisation model class
#'
#' @name MassWeightedUrnRand
#' @return Returns R6 object of class [MassWeightedUrnRand].
#' @export
MassWeightedUrnRand <- R6::R6Class("MassWeightedUrnRand",
  inherit = RandModel,
  public = list(

    #' @field imbalance_tolerance Imbalance control parameter
    imbalance_tolerance = 5,

    #' @description
    #' Creates a new instance of `MassWeightedUrnRand` class.
    #'
    #' @param target_allocation The target allocation ratio
    #' @param history The model history
    #' @param rng_seed The RNG seed for the model
    #' @param imbalance_tolerance The imbalance parameter
    #' @return A new `MassWeightedUrnRand` object.
    initialize = function(
      target_allocation = NULL,
      history = NULL,
      rng_seed = NULL,
      imbalance_tolerance = 5) {
      super$initialize(
        "MassWeightedUrnRand",
        "MWUR",
        "Mass-weighted urn randomisation",
        target_allocation,
        history,
        rng_seed)
      self$imbalance_tolerance <- imbalance_tolerance
    },

    conditional_prob = function() {
      n <- self$num_allocations()
      w <- self$target_allocation / sum(self$target_allocation)
      p <- pmax(self$imbalance_tolerance * w - n + (sum(n) + 1)*w, 0)
      return(p / sum(p))
    },

    get_parameter_descriptions = function() {
      list(
        "target_allocation" = self$target_allocation,
        "imbalance_tolerance" = self$imbalance_tolerance
      )
    }
  )
)
