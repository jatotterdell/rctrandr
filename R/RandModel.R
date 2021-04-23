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

  public = list(

    #' @field name The name of the [RandModel]
    name = character(0),
    #' @field short_name The shorthand name of the [RandModel]
    short_name = character(0),
    #' @field description A description of the [RandModel]
    description = character(0),
    #' @field target_allocation The target allocation ratio/probabilities
    target_allocation = NULL,
    #' @field rng_seed The integer seed to use for generating allocations
    rng_seed = NULL,
    #' @field history The history of model allocations.
    history = NULL,

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
      target_allocation = NULL,
      history = NULL,
      rng_seed = NULL
    ) {
      #------
      # Checks
      #------
      checkmate::assert_character(c(name, short_name, description), null.ok = TRUE)
      checkmate::assert_numeric(target_allocation)
      checkmate::assert_numeric(history, null.ok = TRUE)
      checkmate::assert_integerish(rng_seed, null.ok = TRUE)

      self$name <- name
      self$short_name <- short_name
      self$description <- description
      self$target_allocation <- target_allocation
      self$history <- history

      if(is.null(rng_seed)) {
        if(!exists(".Random.seed", .GlobalEnv)) set.seed(NULL)
        rng_seed <- .GlobalEnv$.Random.seed
      }
      self$rng_seed <- rng_seed

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
      cat("Total allocations: ", sum(self$num_allocations()), "\n", sep = "")
      cat("Target allocation: ", sprintf("%.2f", self$target_allocation), "\n")
      invisible(self)
    },

    #' @description
    #' The name of the randomisation model.
    #' @return A string giving the model name.
    get_name = function() {
      self$name
    },

    #' @description
    #' The short name of the randomisation model.
    #' @return A string giving the model short name.
    get_short_name = function() {
      self$short_name
    },

    #' @description
    #' The description of the randomisation model.
    #' @return A string giving the model description.
    get_description = function() {
      self$description
    },

    #' @description
    #' The parameters required for the randomisation model.
    #' @return A list giving the required model parameters.
    get_parameter_descriptions = function() {
      return(NULL)
    },

    #' @description
    #' The parameters required by the randomisation model to generate an allocation.
    #' @return A list giving the required model inputs.
    get_parameter_input_descriptions = function() {
      return(NULL)
    },

    #' @description
    #' Return the target allocation for the model
    get_target_allocation = function() {
      return(self$target_allocation)
    },

    #' @description
    #' Return the history of the model
    get_history = function() {
      return(self$history)
    },

    #' @description
    #' @return The state of the RNG of the model.
    get_rng = function() {
      private$.rng
    },

    #' @description
    #' Return the number of arms.
    #' @return The number of arms in the model.
    num_arms = function() {
      return(base::length(self$target_allocation))
    },

    #' @description
    #' Return the names of the arms
    #' @return A string vector
    arm_names = function() {
      return(names(self$target_allocation))
    },

    #' @description
    #' Return the number of allocations to each arm.
    #' @return A table giving the number of allocations to each arm.
    num_allocations = function() {
      a <- self$num_arms()
      h <- self$get_history()
      return(as.integer(table(factor(h, levels = 1:a))))
    },

    #' @description
    #' Return the current conditional allocation probability given current state.
    #' @return A numeric vector with one element per arm giving that arms allocation probability.
    conditional_prob = function() {
      p <- self$target_allocation / sum(self$target_allocation)
      return(p)
    },

    #' @description
    #' Generate a single allocation from the randomisation model
    random_allocation = function() {
      # Persist existing RNG into R session
      if(!exists(".Random.seed", .GlobalEnv)) set.seed(NULL)
      assign(".Random.seed", self$rng_seed, envir = .GlobalEnv)
      # Generate allocation
      p <- self$conditional_prob()
      u <- runif(1)
      y <- findInterval(u, cumsum(c(0, p)))
      self$history <- c(self$history, y)
      # Save the updated RNG state and history
      self$rng_seed <- .GlobalEnv$.Random.seed
      y
    }
  )
)


