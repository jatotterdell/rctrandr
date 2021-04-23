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
    #' @param target A (named) numeric vector giving the target allocation ratio.
    #' @param history The model history of allocations. If NULL assumes no history.
    #' @param rng_seed The RNG initial seed in the form of `.Random.seed`.
    #' @return A new `CompleteRand` object.
    initialize = function(
      target_allocation = NULL,
      history = NULL,
      rng_seed = NULL) {
        super$initialize(
          "CompleteRand",
          "CR",
          "Complete randomisation",
          target_allocation,
          history,
          rng_seed)
      }
  )
)
