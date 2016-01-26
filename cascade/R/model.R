#' Compute derivatives for cascade model.
#' 
#' @title Compute derivatives for cascade model
#'
#' @param y initial.
#'
#' @param p parameters.
#'
#' @export
#' @useDynLib cascade
cascade_derivs <- function(y, p) {
    .Call("r_initmod", p, PACKAGE = "cascade")
    .Call("r_derivs", y, PACKAGE = "cascade")
}