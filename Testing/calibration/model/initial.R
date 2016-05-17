GetInitial <- function(
    user_plhiv,
    user_diag,
    user_care,
    user_tx,
    user_vs,
    user_ltfu,
    p_preArt500,
    p_preArt350500,
    p_preArt250350,
    p_preArt200250,
    p_preArt100200,
    p_preArt50100,
    p_preArt50,
    p_onArt500,
    p_onArt350500,
    p_onArt250350,
    p_onArt200250,
    p_onArt100200,
    p_onArt50100,
    p_onArt50,
    ...
    ) {
    default <- c(
        UnDx_500 = (user_plhiv - user_diag) * p_preArt500,
        UnDx_350500 = (user_plhiv - user_diag) * p_preArt350500,
        UnDx_250350 = (user_plhiv - user_diag) * p_preArt250350,
        UnDx_200250 = (user_plhiv - user_diag) * p_preArt200250,
        UnDx_100200 = (user_plhiv - user_diag) * p_preArt100200,
        UnDx_50100 = (user_plhiv - user_diag) * p_preArt50100,
        UnDx_50 = (user_plhiv - user_diag) * p_preArt50,

        Dx_500 = (user_diag - user_care - user_ltfu) * p_preArt500,
        Dx_350500 = (user_diag - user_care - user_ltfu) * p_preArt350500,
        Dx_250350 = (user_diag - user_care - user_ltfu) * p_preArt250350,
        Dx_200250 = (user_diag - user_care - user_ltfu) * p_preArt200250,
        Dx_100200 = (user_diag - user_care - user_ltfu) * p_preArt100200,
        Dx_50100 = (user_diag - user_care - user_ltfu) * p_preArt50100,
        Dx_50 = (user_diag - user_care - user_ltfu) * p_preArt50,

        Care_500 = (user_care - user_tx) * p_preArt500,
        Care_350500 = (user_care - user_tx) * p_preArt350500,
        Care_250350 = (user_care - user_tx) * p_preArt250350,
        Care_200250 = (user_care - user_tx) * p_preArt200250,
        Care_100200 = (user_care - user_tx) * p_preArt100200,
        Care_50100 = (user_care - user_tx) * p_preArt50100,
        Care_50 = (user_care - user_tx) * p_preArt50,

        PreLtfu_500 = 0 * p_preArt500,
        PreLtfu_350500 = 0 * p_preArt350500,
        PreLtfu_250350 = 0 * p_preArt250350,
        PreLtfu_200250 = 0 * p_preArt200250,
        PreLtfu_100200 = 0 * p_preArt100200,
        PreLtfu_50100 = 0 * p_preArt50100,
        PreLtfu_50 = 0 * p_preArt50,

        Tx_Na_500 = (user_tx - user_vs) * p_onArt500,
        Tx_Na_350500 = (user_tx - user_vs) * p_onArt350500,
        Tx_Na_250350 = (user_tx - user_vs) * p_onArt250350,
        Tx_Na_200250 = (user_tx - user_vs) * p_onArt200250,
        Tx_Na_100200 = (user_tx - user_vs) * p_onArt100200,
        Tx_Na_50100 = (user_tx - user_vs) * p_onArt50100,
        Tx_Na_50 = (user_tx - user_vs) * p_onArt50,

        Tx_A_500 = user_vs * p_onArt500,
        Tx_A_350500 = user_vs * p_onArt350500,
        Tx_A_250350 = user_vs * p_onArt250350,
        Tx_A_200250 = user_vs * p_onArt200250,
        Tx_A_100200 = user_vs * p_onArt100200,
        Tx_A_50100 = user_vs * p_onArt50100,
        Tx_A_50 = user_vs * p_onArt50,

        Ltfu_500 = (user_ltfu) * p_preArt500,
        Ltfu_350500 = (user_ltfu) * p_preArt350500,
        Ltfu_250350 = (user_ltfu) * p_preArt250350,
        Ltfu_200250 = (user_ltfu) * p_preArt200250,
        Ltfu_100200 = (user_ltfu) * p_preArt100200,
        Ltfu_50100 = (user_ltfu) * p_preArt50100,
        Ltfu_50 = (user_ltfu) * p_preArt50,

        # Keeping track
        NewInf = 0,
        HivMortality = 0,
        NaturalMortality = 0,

        # Costs
        Dx_Cost = 0,
        Linkage_Cost = 0,
        Annual_Care_Cost = 0,
        Annual_ART_Cost = 0
    )
    replace <- c(...)
    if (length(replace) > 0L) {
        stopifnot(all(names(replace) %in% names(default)))
        default[names(replace)] <- replace
    }
    default
}
