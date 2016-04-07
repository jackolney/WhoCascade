ExtractCascadeData <- function(year) {
    result <- CallModel()

    # We iterate across _all_ results, and present the mean, and ranges.
    # Also switched to absolute values, not proportions.
    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    CX_data <- unlist(lapply(result, function(x) sum(x$Care[year], x$ART[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    NX <- mean(NX_data)
    DX <- mean(DX_data)
    CX <- mean(CX_data)
    TX <- mean(TX_data)
    VS <- mean(VS_data)

    NX_range <- range(NX_data)
    DX_range <- range(DX_data)
    CX_range <- range(CX_data)
    TX_range <- range(TX_data)
    VS_range <- range(VS_data)

    res <- c(NX, DX, CX, TX, VS)
    min <- c(NX_range[1], DX_range[1], CX_range[1], TX_range[1], VS_range[1])
    max <- c(NX_range[2], DX_range[2], CX_range[2], TX_range[2], VS_range[2])

    def <- c("# PLHIV", "# Diagnosed", "# In Care", "# Treatment", "# Suppressed")
    df <- data.frame(def, res, min, max)
    df$def <- factor(df$def, levels = c("# PLHIV", "# Diagnosed", "# In Care", "# Treatment", "# Suppressed"))
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

Extract909090Data <- function(...) {
    data <- c(...)
    if(length(data) > 0L) {
        result <- data
    } else {
        result <- CallModel()
    }

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

    un_90 <- DX / PLHIV
    un_9090 <- TX / DX
    un_909090 <- VS / TX

    res <- c(un_90, un_9090, un_909090)
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    scenario <- c("Baseline")
    df <- data.frame(def, res, scenario)
    df$def <- factor(df$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    df
}
