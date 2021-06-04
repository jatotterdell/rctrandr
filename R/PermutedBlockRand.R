#' @title Permuted Block Randomisation Model
#'
#' @description A permuted block randomisation model class
#'
#' ***NOT IMPLEMENTED***
#'
#' @name PermutedBlockRand
#' @return Returns R6 object of class PermutedBlockRand
#' @export
PermutedBlockRand <- R6::R6Class(
  "PermutedBlockRand",
  inherit = RandModel,

  public = list(
    blocksize = NULL,

    initialize = function(
      target = NULL,
      history = NULL,
      blocksize = NULL) {
      super$initialize(
        "PermutedBlockRand",
        "PBR",
        "Permuted block randomisation",
        target,
        history)
      self$blocksize <- blocksize
      invisible(self)
    },

    conditional_prob = function() {
    },

    get_parameter_descriptions = function() {
      list(
        "target_allocation" = self$target_allocation,
        "block_size" = self$blocksize
      )
    }
  )
)
