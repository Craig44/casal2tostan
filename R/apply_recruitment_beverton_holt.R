#' convert an beverton holt recruitment process in Casal2 to STAN syntax
#'
#' @author C.Marsh
#' @keywords internal
#'
apply_recruitment_beverton_holt = function(recruitment_list, year_ndx, initialisation_phase = FALSE, b0_initialised = TRUE) {
  r0 = 0;
  ## Category labels
  BH_syntax = c(

  )
  if (initialisation_phase) {
    ## We are entering the initialisation phase
    if (b0_initialised == "T") {
      r0 = 1;
    } else {
      r0 = recruitment_list$r0$value
    }
    ## just seed this r0 every iteration
    for (c in 1:length(recruitment_list$categories$value)) {
      this_line = paste0("numbers_at_age_",recruitment_list$categories$value[c],"[1,1] = ", r0 ," * ", recruitment_list$proportions$value,";")
      BH_syntax = c(BH_syntax, this_line, "\n");
    }
  } else {
    ## will get to this apply BH during execution

  }
  ## apply a tab so that the STAN model syntax looks nice
  BH_syntax = paste("\t\t",BH_syntax, sep = "");
  return(BH_syntax);
}


