#' csltostan.R
#'
#' @author C.Marsh
#' @description A function that will generate a STAN file that will replicate a Casal2 model.
#' this function assumes that the config.csl2 file has !include statements that have the model description
#' This is a note that if there are any model configurations in this file other that !includes we will not process them
#' There should be a warning for this case.
#' @date 15/4/2018
#' @copyright BigBlueData ltd
#'

csltostan = function(casal2_configuration_file = "config.csl2", path = "") {
  readLines()


}

