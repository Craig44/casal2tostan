#' convert an ageing process in Casal2 to STAN syntax
#'
#' @author C.Marsh
#' @keywords internal
#'
apply_ageing = function(ageing_list, year_ndx, initialisation_phase = FALSE) {
  ## Category labels
  ageing_syntax = c(

  )
  if (initialisation_phase) {
    for (c in 1:length(ageing_list$categories$value)) {
      this_line = c(
        paste0("temp_partition = numbers_at_age_",ageing_list$categories$value[c],"[1,];"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[1,2:(n_ages - 1)] = temp_partition[1:(n_ages - 2)];"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[1,n_ages] = sum(temp_partition[(n_ages - 1): n_ages]);"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[1,1] = 0.0;")
      )
      ageing_syntax = c(ageing_syntax, this_line, "\n");
    }
  } else {
    ## start the loop of categories in this process
    for (c in 1:length(ageing_list$categories$value)) {
      this_line = c(
        paste0("temp_partition = numbers_at_age_",ageing_list$categories$value[c],"[",year_ndx,",];"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[",year_ndx + 1,",2:(n_ages - 1)] = temp_partition[1:(n_ages - 2)];"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[",year_ndx + 1,",n_ages] = sum(temp_partition[(n_ages - 1): n_ages]);"),
        paste0("numbers_at_age_",ageing_list$categories$value[c],"[",year_ndx + 1,",1] = 0.0;")
      )
      ageing_syntax = c(ageing_syntax, this_line, "\n");
    }
  }
  ## apply a tab so that the STAN model syntax looks nice
  ageing_syntax = paste("\t\t",ageing_syntax, sep = "")
}
