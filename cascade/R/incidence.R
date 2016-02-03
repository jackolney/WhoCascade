#' Incidence for model.
#'
#' @title Incidence for model
#'
#' @param ... vector of named arguments
#'
#' @export
incidence <- function(...) {
    default <- c(
        0,
        308,
        420,
        865,
        954,
        1215,
        1704,
        2387,
        3382,
        4767,
        6743,
        9446,
        13910,
        19694,
        28680,
        41376,
        59359,
        83276,
        113518,
        150306,
        187115,
        220538,
        241640,
        248919,
        236519,
        210539,
        177433,
        145592,
        119287,
        100222,
        89626,
        85019,
        80806,
        77795,
        74978,
        72741,
        72769,
        74467,
        64143,
        61835,
        63519,
        56004,
        56977,
        57945,
        56648,
        69780)
    replace <- c(...)
    if(length(replace) == length(default)) {
        default <- replace
    }
    default
}
