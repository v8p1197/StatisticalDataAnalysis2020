.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "hasName", force=TRUE)
}

#'
#' Removes NA from a feature
#'
#' Takes a features of a dataset and removes every NA value, returning only the the unique feature values
#' @param feature the feature to remove NA values from
#' @return the non-NA unique values of the input feature
#' @usage na.omit.unique(feature)
#' @export
na.omit.unique <- function(feature) {
  return (unique(na.omit(feature)))
}

#'
#'
rename <- function(data, name.old, name.new) {
  names(data)[names(data) == name.old] <- name.new
  return (data)
}
