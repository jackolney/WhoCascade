GetInitial <- function(p, iterationResult, masterCD4) {
    if(!is.list(iterationResult)) stop("Not a list.")

    i2015_PLHIV       <- iterationResult[iterationResult$indicator == "PLHIV","value"]
    i2015_PLHIV_Diag  <- iterationResult[iterationResult$indicator == "PLHIV Diagnosed","value"]
    i2015_PLHIV_Care  <- iterationResult[iterationResult$indicator == "PLHIV in Care","value"]
    i2015_PLHIV_ART   <- iterationResult[iterationResult$indicator == "PLHIV on ART","value"]
    i2015_PLHIV_Viral <- iterationResult[iterationResult$indicator == "PLHIV Suppressed","value"]

    iCD4_500    <- masterCD4[1,"prop.Off.ART.500"][[1]]
    iCD4_350500 <- masterCD4[1,"prop.Off.ART.350500"][[1]]
    iCD4_250350 <- masterCD4[1,"prop.Off.ART.250350"][[1]]
    iCD4_200250 <- masterCD4[1,"prop.Off.ART.200250"][[1]]
    iCD4_100200 <- masterCD4[1,"prop.Off.ART.100200"][[1]]
    iCD4_50100  <- masterCD4[1,"prop.Off.ART.50100"][[1]]
    iCD4_50     <- masterCD4[1,"prop.Off.ART.50"][[1]]

    iCD4_ART_500    <- masterCD4[1,"prop.On.ART.500"][[1]]
    iCD4_ART_350500 <- masterCD4[1,"prop.On.ART.350500"][[1]]
    iCD4_ART_250350 <- masterCD4[1,"prop.On.ART.250350"][[1]]
    iCD4_ART_200250 <- masterCD4[1,"prop.On.ART.200250"][[1]]
    iCD4_ART_100200 <- masterCD4[1,"prop.On.ART.100200"][[1]]
    iCD4_ART_50100  <- masterCD4[1,"prop.On.ART.50100"][[1]]
    iCD4_ART_50     <- masterCD4[1,"prop.On.ART.50"][[1]]

    default <- initial(
        p,
        UnDx_500 =       (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_500,
        UnDx_350500 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_350500,
        UnDx_250350 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_250350,
        UnDx_200250 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_200250,
        UnDx_100200 =    (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_100200,
        UnDx_50100 =     (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_50100,
        UnDx_50 =        (i2015_PLHIV - i2015_PLHIV_Diag) * iCD4_50,

        Dx_500 =         (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_500,
        Dx_350500 =      (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_350500,
        Dx_250350 =      (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_250350,
        Dx_200250 =      (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_200250,
        Dx_100200 =      (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_100200,
        Dx_50100 =       (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_50100,
        Dx_50 =          (i2015_PLHIV_Diag - i2015_PLHIV_Care) * iCD4_50,

        Care_500 =       (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_500,
        Care_350500 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_350500,
        Care_250350 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_250350,
        Care_200250 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_200250,
        Care_100200 =    (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_100200,
        Care_50100 =     (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_50100,
        Care_50 =        (i2015_PLHIV_Care - i2015_PLHIV_ART) * iCD4_50,

        PreLtfu_500 =    0,
        PreLtfu_350500 = 0,
        PreLtfu_250350 = 0,
        PreLtfu_200250 = 0,
        PreLtfu_100200 = 0,
        PreLtfu_50100 =  0,
        PreLtfu_50 =     0,

        Tx_Na_500 =      (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_500,
        Tx_Na_350500 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_350500,
        Tx_Na_250350 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_250350,
        Tx_Na_200250 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_200250,
        Tx_Na_100200 =   (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_100200,
        Tx_Na_50100 =    (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_50100,
        Tx_Na_50 =       (i2015_PLHIV_ART - i2015_PLHIV_Viral) * iCD4_ART_50,

        Tx_A_500 =       i2015_PLHIV_Viral * iCD4_ART_500,
        Tx_A_350500 =    i2015_PLHIV_Viral * iCD4_ART_350500,
        Tx_A_250350 =    i2015_PLHIV_Viral * iCD4_ART_250350,
        Tx_A_200250 =    i2015_PLHIV_Viral * iCD4_ART_200250,
        Tx_A_100200 =    i2015_PLHIV_Viral * iCD4_ART_100200,
        Tx_A_50100 =     i2015_PLHIV_Viral * iCD4_ART_50100,
        Tx_A_50 =        i2015_PLHIV_Viral * iCD4_ART_50,

        Ltfu_500 =       0,
        Ltfu_350500 =    0,
        Ltfu_250350 =    0,
        Ltfu_200250 =    0,
        Ltfu_100200 =    0,
        Ltfu_50100 =     0,
        Ltfu_50 =        0
    )
    default
}
