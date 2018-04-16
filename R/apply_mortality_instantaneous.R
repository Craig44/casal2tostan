#' convert an instantaneous mortality process in Casal2 to STAN syntax
#'
#' @author C.Marsh
#' @keywords internal
#'
apply_mortality_instantaneous = function(mortality_list, year_ndx, initialisation_phase = FALSE, observation = FALSE, time_step_ndx) {
  instant_mort_syntax = c(
  )
  if (initialisation_phase) {
    ## in initialisation and don't need to do anything tricky yah
    for (c in 1:length(mortality_list$categories$value)) {
      this_line = c(
        paste0("temp_partition = numbers_at_age_",mortality_list$categories$value[c],"[1,];"),
        paste0("numbers_at_age_",mortality_list$categories$value[c],"[1,] = temp_partition .* exp(-", mortality_list$m$value ," * ", mortality_list$selectivities$value,");")
      )
      instant_mort_syntax = c(instant_mort_syntax, this_line, "\n");
    }
  } else {
    ## it gets complicated

  }
  ## apply a tab so that the STAN model syntax looks nice
  instant_mort_syntax = paste("\t\t",instant_mort_syntax, sep = "");
  return(instant_mort_syntax);
}


