#' Incidence for model.
#'
#' @title Incidence for model
#'
#' @param ... vector of named arguments
#'
#' @export
incidence <- function(...) {
    default <- c(
        63519,
        56004,
        56977,
        57945,
        56648,
        69780,
        69780)
    replace <- c(...)
    if(length(replace) == length(default)) {
        default <- replace
    }
    default
}
