#' Utility extract function
#'
#' @author C.Marsh
#' @keywords internal
#'
cleanfile = function(file) {
  ## remove white space at the beginning of a subcommand or command e.g
  while (any(regexpr(" ", file) == 1)) {
    index <- regexpr(" ", file) == 1
    file <- ifelse(index, substring(file, 2), file)
  }
  file <- file[substring(file, 1, 1) != "#"]
  ## Find and remove any lines that begin or end with { or } which is also a comment
  index1 <- ifelse(substring(file, 1, 1) == "/*", 1:length(file), 0)
  index2 <- ifelse(substring(file, 1, 1) == "*/", 1:length(file), 0)
  index1 <- index1[index1 != 0]
  index2 <- index2[index2 != 0]
  if (length(index1) != length(index2))
    stop(paste("Error in the file ", casal2_configuration_file, ". Cannot find a matching '/*' or '*/'", sep = ""))
  if (length(index1) > 0 || length(index2) > 0) {
    index <- unlist(apply(cbind(index1, index2), 1, function(x) seq(x[1],x[2])))
    file <- file[!is.in(1:length(file), index)]
  }
  for(j in 1:length(file)) {
    index <- regexpr("#", file[j])
    if (index != -1) {
      file[j] = substring(text = file[j],first = 0, last = index - 1)
    }
  }
  return(file);
}
