#' @title Modified Urn Randomisation Model
#'
#' @description A modified urn randomisation model class
#'
#' ***NOT IMPLEMENTED***
#'
#' @name ModifiedUrnRand
#' @return Returns R6 object of class [ModifiedUrnRand].
#' @export
ModifiedUrnRand <- R6::R6Class(
  "ModifiedUrnRand",
  inherit = RandModel,

  public = list(

   #' @field imbalance_tolerance Imbalance control parameter
   imbalance_tolerance = 5,

   #' @description
   #' Creates a new instance of `ModifiedUrnRand` class.
   #'
   #' @param target_allocation The target allocation ratio
   #' @param history The model history
   #' @param rng_seed The RNG seed for the model
   #' @return A new `ModifiedUrnRand` object.
   initialize = function(
     target_allocation = NULL,
     history = NULL,
     rng_seed = NULL) {
     super$initialize(
       "ModifiedUrnRand",
       "MUR",
       "Modified urn randomisation",
       target_allocation,
       history,
       rng_seed)
     self$imbalance_tolerance <- imbalance_tolerance
   },

   conditional_prob = function() {
   },

   get_parameter_descriptions = function() {
     list(
       "target_allocation" = self$target_allocation
     )
   }
  )
)
