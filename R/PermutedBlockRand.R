#' @title Permuted Block Randomisation Model
#'
#' @description A permuted block randomisation model class
#'
#' @name PermutedBlockRand
#' @return Returns R6 object of class PermutedBlockRand
#' @export
PermutedBlockRand <- R6::R6Class("PermutedBlockRand",
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
    }
  )
)
