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

    UNDX <- mean(unlist(lapply(result, function(x) sum(x$UnDx[year]))))
    DX   <- mean(unlist(lapply(result, function(x) sum(x$Dx[year]))))
    CX   <- mean(unlist(lapply(result, function(x) sum(x$Care[year]))))
    PLX  <- mean(unlist(lapply(result, function(x) sum(x$PreLtfu[year]))))
    TXN  <- mean(unlist(lapply(result, function(x) sum(x$Tx[year] - x$Vs[year]))))
    VS   <- mean(unlist(lapply(result, function(x) sum(x$Vs[year]))))
    LX   <- mean(unlist(lapply(result, function(x) sum(x$Ltfu[year]))))

    res <- c(VS, TXN, CX, DX, UNDX, PLX, LX,
             VS, TXN, CX, DX, PLX, LX,
             VS, TXN, CX,
             VS, TXN,
             VS)

    state <- c("# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# Undiagnosed", "# pre-ART LTFU", "# LTFU",
               "# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# pre-ART LTFU", "# LTFU",
               "# Suppressed", "# On Treatment (non-adherent)", "# In Care",
               "# Suppressed", "# On Treatment (non-adherent)",
               "# Suppressed")

    order <- c(rep("All"       ,7),
               rep("Diagnosed" ,6),
               rep("Care"      ,3),
               rep("Treatment" ,2),
               rep("Suppressed",1))

    df <- data.frame(state, res, order)
    df$order <- factor(df$order, levels = c("All", "Diagnosed", "Care", "Treatment", "Suppressed"))
    df$state <- factor(df$state, levels = c("# Suppressed", "# On Treatment (non-adherent)", "# In Care", "# Diagnosed", "# Undiagnosed", "# pre-ART LTFU", "# LTFU"))
    df
}

Extract909090Data <- function(...) {
    data <- c(...)
    if(length(data) > 0L) {
        result <- data
    } else {
        result <- CallModel()
    }

    # Always aiming for 2020 here (5.02 / 0.02)
    year <- 251

    NX_data <- unlist(lapply(result, function(x) sum(x$N[year])))
    DX_data <- unlist(lapply(result, function(x) sum(x$Dx[year], x$Care[year], x$PreLtfu[year], x$ART[year], x$Ltfu[year])))
    TX_data <- unlist(lapply(result, function(x) sum(x$ART[year])))
    VS_data <- unlist(lapply(result, function(x) sum(x$Vs[year])))

    NX <- mean(NX_data)
    DX <- mean(DX_data)
    TX <- mean(TX_data)
    VS <- mean(VS_data)

    NX_range <- range(NX_data)
    DX_range <- range(DX_data)
    TX_range <- range(TX_data)
    VS_range <- range(VS_data)

    UN_90 <- DX / NX
    UN_9090 <- TX / DX
    UN_909090 <- VS / TX

    UN_90_min <- DX_range[1] / NX_range[1]
    UN_9090_min <- TX_range[1] / DX_range[1]
    UN_909090_min <- VS_range[1] / TX_range[1]

    UN_90_max <- DX_range[2] / NX_range[2]
    UN_9090_max <- TX_range[2] / DX_range[2]
    UN_909090_max <- VS_range[2] / TX_range[2]

    res <- c(UN_90, UN_9090, UN_909090)
    min <- c(UN_90_min, UN_9090_min, UN_909090_min)
    max <- c(UN_90_max, UN_9090_max, UN_909090_max)
    def <- c("% Diagnosed","% On Treatment","% Suppressed")
    df <- data.frame(def, res, min, max)
    df$def <- factor(df$def, levels = c("% Diagnosed", "% On Treatment", "% Suppressed"))
    df
}
