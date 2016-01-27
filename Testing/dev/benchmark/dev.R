rm(list=ls())
require(deSolve)

source("TheModel.R")
source("Parameters.R")
source("Initial.R")

# p <- Parameters()
# y <- Initial(p)

p <- Parameters(
    beta = 0,
    Alpha_1 = 0,
    Alpha_2 = 0,
    Alpha_3 = 0,
    Alpha_4 = 0,
    Alpha_5 = 0,
    Alpha_6 = 0,
    Alpha_7 = 0,
    Tau_1 = 0,
    Tau_2 = 0,
    Tau_3 = 0,
    Tau_4 = 0,
    Tau_5 = 0,
    Tau_6 = 0,
    Tau_7 = 0,
    Mu = 0
    )
y <- Initial(p)

ref <- ComplexCascade(0, y, p)[[1]]

Time <- seq(0, 5, 0.02)
theref <- as.data.frame(ode(times = Time, y = y, func = ComplexCascade, parms = p))
head(theref)

# Calculating N using the 'ComplexCascade' is too much effort.
# Do it on the fly, after simulations have run -- but try to avoid %>% (flags?)

N <- theref$UnDx_500 + theref$UnDx_350500 + theref$UnDx_250350 + theref$UnDx_200250 + theref$UnDx_100200 + theref$UnDx_50100 + theref$UnDx_50 + theref$Dx_500 + theref$Dx_350500 + theref$Dx_250350 + theref$Dx_200250 + theref$Dx_100200 + theref$Dx_50100 + theref$Dx_50 + theref$Care_500 + theref$Care_350500 + theref$Care_250350 + theref$Care_200250 + theref$Care_100200 + theref$Care_50100 + theref$Care_50 + theref$PreLtfu_500 + theref$PreLtfu_350500 + theref$PreLtfu_250350 + theref$PreLtfu_200250 + theref$PreLtfu_100200 + theref$PreLtfu_50100 + theref$PreLtfu_50 + theref$Tx_Na_500 + theref$Tx_Na_350500 + theref$Tx_Na_250350 + theref$Tx_Na_200250 + theref$Tx_Na_100200 + theref$Tx_Na_50100 + theref$Tx_Na_50 + theref$Tx_A_500 + theref$Tx_A_350500 + theref$Tx_A_250350 + theref$Tx_A_200250 + theref$Tx_A_100200 + theref$Tx_A_50100 + theref$Tx_A_50 + theref$Ltfu_500 + theref$Ltfu_350500 + theref$Ltfu_250350 + theref$Ltfu_200250 + theref$Ltfu_100200 + theref$Ltfu_50100 + theref$Ltfu_50

plot(round(N, 0), type = 'l', lwd = 2)
# YES!