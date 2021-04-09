#' @title
#' Generalised Randomisation Model object
#'
#' @description
#' A generalised randomisation model object for defining custom randomisation schemes.
#'
#' @details
#' A generic randomisation model from which specific models inherit.
#'
#' @name RandModel
#' @return Returns [R6::R6Class()] object of class RandModel
#'
#' @export
#' @import randtoolbox
RandModel <- R6::R6Class("RandModel",
  lock_objects = FALSE,

  private = list(
    .history = NULL,
    .rng = NULL
  ),

  public = list(

    #' @field name The name of the RandModel
    name = character(0),
    #' @field short_name The shorthand name of the RandModel
    short_name = character(0),
    #' @field description A description of the RandModel
    description = character(0),
    #' @field target The target allocation ratio/probabilities
    target = NULL,
    #' @field rng_seed The integer seed to use for generating allocations
    rng_seed = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name Name.
    #' @param short_name Short name.
    #' @param description Description.
    #' @param target Target allocation ratio
    #' @param history Randomisation model history
    #' @param rng_seed The RNG initial seed (integer)
    #' @return A new `RandModel` object.
    initialize = function(
      name = NULL,
      short_name = NULL,
      description = NULL,
      target = NULL,
      history = NULL,
      rng_seed = NULL
    ) {
      #------
      # Checks
      #------
      checkmate::assert_character(c(name, short_name, description), null.ok = TRUE)
      checkmate::assert_numeric(target)
      checkmate::assert_numeric(history, null.ok = TRUE)
      checkmate::assert_integerish(rng_seed, null.ok = TRUE)

      self$name <- name
      self$short_name <- short_name
      self$description <- description
      self$target <- target
      private$.history <- history
      self$rng_seed <- rng_seed

      set.seed(rng_seed)
      private$.rng <- .Random.seed

      lockBinding("name", self)
      lockBinding("short_name", self)
      lockBinding("description", self)

      invisible(self)
    },

    #' @description
    #' Print a RandModel.
    #'
    #' @param ... Any additional arguments
    print = function(...) {
      cat("<", self$name, "> Randomisation Model", "\n", sep = "")
      cat("History length: ", length(self$get_history()), "\n", sep = "")
      cat("Target allocation: ", sprintf("%.2f", self$get_target()), "\n")
      invisible(self)
    },

    #' @description
    #' Return the `RandModels` name.
    get_name = function() {
      self$name
    },

    #' @description
    #' Return the `RandModels` description
    get_description = function() {
      self$description
    },

    #' @description
    #' Return the target allocation for the model
    get_target = function() {
      self$target
    },

    #' @description
    #' Return the history of the model
    get_history = function() {
      self$history
    },

    #' @description
    #' Return the state of the RNG of the model
    get_rng = function() {
      private$.rng
    },

    #' @description
    #' Return the number of arms
    num_arms = function() {
      length(self$target)
    },

    #' @description
    #' Return the number of allocations to each arm.
    #' @return A table giving the number of allocations to each arm.
    num_allocations = function() {
      a <- self$num_arms()
      h <- self$get_history()
      table(factor(h, levels = 1:a))
    },

    #' @description
    #' Generate randomisations from the model.
    #' @param n The number of assignments to generate
    #' @return A vector of arm allocations of length `n`.
    randomise = function(n) {
      cat("Not implemented")
      numeric(0)
    }
  ),

  active = list(
    history = function(values) {
      if(missing(values)) {
        private$.history
      } else {
        private$.history <- c(private$.history, values)
        self
      }
    }
  )
)


