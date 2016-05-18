#' Parameters for model.
#'
#' @title Parameters for model
#'
#' @param ... vector of named arguments
#'
#' @export
parameters <- function(...) {
    default <- c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.328285,
        Nu_4 = 0.497247,
        Nu_5 = 0.559090,
        Nu_6 = 0.846406,
        Rho = 0.205,
        Epsilon = 16.949,
        Kappa = 1.079,
        Gamma = 2.556,
        Theta = 1.511,
        Omega = 0.033,
        p = 0.95,
        s_1 = 0.1,
        s_2 = 0.4,
        s_3 = 0.7,
        s_4 = 1,
        s_5 = 1.3,
        s_6 = 1.6,
        s_7 = 1.9,
        Sigma = 0,
        Delta_1 = 0.5178,
        Delta_2 = 0.8862,
        Delta_3 = 0.8832,
        Delta_4 = 1.1581,
        Delta_5 = 2.5663,
        Alpha_1 = 0.004110,
        Alpha_2 = 0.011670,
        Alpha_3 = 0.009385,
        Alpha_4 = 0.016394,
        Alpha_5 = 0.027656,
        Alpha_6 = 0.047877,
        Alpha_7 = 1.081964,
        Tau_1 = 0.003905,
        Tau_2 = 0.011087,
        Tau_3 = 0.008916,
        Tau_4 = 0.015574,
        Tau_5 = 0.026273,
        Tau_6 = 0.045482,
        Tau_7 = 1.02785,
        Mu = 0.005,
        ART_All = 1,
        ART_500 = 1,
        ART_350 = 1,
        ART_200 = 1,
        Dx_unitCost = 10,
        Linkage_unitCost = 40,
        Annual_Care_unitCost = 40,
        Annual_ART_unitCost = 367,
        prop_preART_500 = 0.5251,
        prop_preART_350500 = 0.2315,
        prop_preART_250350 = 0.1787,
        prop_preART_200250 = 0.0615,
        prop_preART_100200 = 0.0011,
        prop_preART_50100 = 0.0008,
        prop_preART_50 = 0.0014,
        w1 = 1.35,
        w2 = 1,
        w3 = 1.64,
        w4 = 5.17,
        w5 = 0.1,
        beta = 0.0275837,
        q = 0.5,
        t_1 = 0,
        t_2 = 0,
        t_3 = 0,
        t_4 = 0,
        t_5 = 0)
    replace <- c(...)
    if (length(replace) > 0L) {
        stopifnot(is.numeric(replace))
        stopifnot(all(names(replace) %in% names(default)))
        default[names(replace)] <- replace
    }
    default
}
