GetCalibInitial <- function(p, ...) {
    default <- initial(
        p,
        UnDx_500 = 0,
        UnDx_350500 = 0,
        UnDx_250350 = 0,
        UnDx_200250 = 0,
        UnDx_100200 = 0,

        UnDx_50100 = 0,
        UnDx_50 = 0,
        Dx_500 = 0,
        Dx_350500 = 0,
        Dx_250350 = 0,
        Dx_200250 = 0,
        Dx_100200 = 0,

        Dx_50100 = 0,
        Dx_50 = 0,
        Care_500 = 0,
        Care_350500 = 0,
        Care_250350 = 0,
        Care_200250 = 0,

        Care_100200 = 0,
        Care_50100 = 0,
        Care_50 = 0,
        PreLtfu_500 = 0,
        PreLtfu_350500 = 0,

        PreLtfu_250350 = 0,
        PreLtfu_200250 = 0,
        PreLtfu_100200 = 0,
        PreLtfu_50100 = 0,
        PreLtfu_50 = 0,

        Tx_Na_500 = 0,
        Tx_Na_350500 = 0,
        Tx_Na_250350 = 0,
        Tx_Na_200250 = 0,
        Tx_Na_100200 = 0,

        Tx_Na_50100 = 0,
        Tx_Na_50 = 0,
        Tx_A_500 = 0,
        Tx_A_350500 = 0,
        Tx_A_250350 = 0,
        Tx_A_200250 = 0,

        Tx_A_100200 = 0,
        Tx_A_50100 = 0,
        Tx_A_50 = 0,
        Ltfu_500 = 0,
        Ltfu_350500 = 0,
        Ltfu_250350 = 0,

        Ltfu_200250 = 0,
        Ltfu_100200 = 0,
        Ltfu_50100 = 0,
        Ltfu_50 = 0
    )
    replace <- c(...)
    if(length(replace) > 0L) {
        stopifnot(is.numeric(replace))
        stopifnot(all(names(replace) %in% names(default)))
        default[names(replace)] <- replace
    }
    default
}
