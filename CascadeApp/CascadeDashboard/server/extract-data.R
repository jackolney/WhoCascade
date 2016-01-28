ExtractCascadeData <- function(year) {
    result <- CallModel()

    DX <- sum(result$Dx[year], result$Care[year], result$PreLtfu[year], result$ART[year], result$Ltfu[year])
    CX <- sum(result$Care[year], result$ART[year])
    TX <- result$ART[year]
    VS <- result$Vs[year]
    res <- c(1, DX, CX, TX, VS)
    def <- c("% PLHIV", "% Diagnosed", "% In Care", "% Treatment", "% Suppressed")
    df <- data.frame(def, res)
    df$def <- factor(df$def, levels = c("% PLHIV", "% Diagnosed", "% In Care", "% Treatment", "% Suppressed"))
    df
}

ExtractPowersCascadeData <- function(year) {
    result <- CallModel()

    UNDX <- result$UnDx[year]
    DX <- result$Dx[year]
    CX <- result$Care[year]
    PLX <- result$PreLtfu[year]
    TXN <- result$Tx[year] - result$Vs[year]
    VS <- result$Vs[year]
    LX <- result$Ltfu[year]

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS,
             PLX, LX)

    state <- c("% Suppressed", "% On Treatment (non-adherent)", "% In Care", "% Diagnosed", "% Undiagnosed", "% pre-ART LTFU", "% LTFU",
               "% Suppressed", "% On Treatment (non-adherent)", "% In Care", "% Diagnosed", "% pre-ART LTFU", "% LTFU",
               "% Suppressed", "% On Treatment (non-adherent)", "% In Care",
               "% Suppressed", "% On Treatment (non-adherent)",
               "% Suppressed",
               "% pre-ART LTFU", "% LTFU")

    order <- c(rep("All",7),
               rep("Diagnosed",6),
               rep("Care",3),
               rep("Treatment",2),
               rep("Suppressed",1),
               rep("LTFU",2))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed", "LTFU"))
    df$state <- factor(df$state, levels = c("% Suppressed", "% On Treatment (non-adherent)", "% In Care", "% Diagnosed", "% Undiagnosed", "% pre-ART LTFU", "% LTFU"))
    df
}

Extract909090Data <- function() {
    year <- 251

    PLHIV <- result$N[year]

    DX <- sum(
        result$Dx_500[year], result$Dx_350500[year], result$Dx_250350[year], result$Dx_200250[year], result$Dx_100200[year], result$Dx_50100[year], result$Dx_50[year],
        result$Care_500[year], result$Care_350500[year], result$Care_250350[year], result$Care_200250[year], result$Care_100200[year], result$Care_50100[year], result$Care_50[year],
        result$PreLtfu_500[year], result$PreLtfu_350500[year], result$PreLtfu_250350[year], result$PreLtfu_200250[year], result$PreLtfu_100200[year], result$PreLtfu_50100[year], result$PreLtfu_50[year],
        result$Tx_Na_500[year], result$Tx_Na_350500[year], result$Tx_Na_250350[year], result$Tx_Na_200250[year], result$Tx_Na_100200[year], result$Tx_Na_50100[year], result$Tx_Na_50[year],
        result$Tx_A_500[year], result$Tx_A_350500[year], result$Tx_A_250350[year], result$Tx_A_200250[year], result$Tx_A_100200[year], result$Tx_A_50100[year], result$Tx_A_50[year],
        result$Ltfu_500[year], result$Ltfu_350500[year], result$Ltfu_250350[year], result$Ltfu_200250[year], result$Ltfu_100200[year], result$Ltfu_50100[year], result$Ltfu_50[year]
        )

    TX <- sum(
        result$Tx_Na_500[year], result$Tx_Na_350500[year], result$Tx_Na_250350[year], result$Tx_Na_200250[year], result$Tx_Na_100200[year], result$Tx_Na_50100[year], result$Tx_Na_50[year],
        result$Tx_A_500[year], result$Tx_A_350500[year], result$Tx_A_250350[year], result$Tx_A_200250[year], result$Tx_A_100200[year], result$Tx_A_50100[year], result$Tx_A_50[year]
        )

    VS <- sum(result$Tx_A_500[year], result$Tx_A_350500[year], result$Tx_A_250350[year], result$Tx_A_200250[year], result$Tx_A_100200[year], result$Tx_A_50100[year], result$Tx_A_50[year])

    un_90 <- dx / PLHIV
    un_9090 <- tx / dx
    un_909090 <- vs / tx

    res <- c(un_90, un_9090, un_909090)
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    scenario <- c("Baseline")
    df <- data.frame(def, res, scenario)
    df$def <- factor(df$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    df
}
