# Test Area #
rm(list=ls())

setwd("~/git/WhoCascade/Testing/calibration")
library(cascade)

source("model/parameters.R")
source("model/initial.R")
source("model/run.R")

# Fill out info (passed to GetInitial())
user_country = "Kenya"
user_plhiv = 1400000
user_diag = 1109668
user_care = 848018
user_tx = 748000
user_vs = 295000
user_ltfu = 0

p_preArt500 = 0.436389236
p_preArt350500 = 0.321002609
p_preArt250350 = 0.141081564
p_preArt200250 = 0.056611708
p_preArt100200 = 0.035270391
p_preArt50100 = 0.007782541
p_preArt50 = 0.001861952

p_onArt500 = 0.07780654
p_onArt350500 = 0.11304476
p_onArt250350 = 0.238703388
p_onArt200250 = 0.195633724
p_onArt100200 = 0.225103218
p_onArt50100 = 0.093872469
p_onArt50 = 0.0558359



time <- seq(0, 5, 0.02)
p <- GetParameters()
y <- GetInitial(
    user_country = "Kenya",
    user_plhiv = 1400000,
    user_diag = 1109668,
    user_care = 848018,
    user_tx = 748000,
    user_vs = 295000,
    user_ltfu = 0,
    p_preArt500 = 0.436389236,
    p_preArt350500 = 0.321002609,
    p_preArt250350 = 0.141081564,
    p_preArt200250 = 0.056611708,
    p_preArt100200 = 0.035270391,
    p_preArt50100 = 0.007782541,
    p_preArt50 = 0.001861952,
    p_onArt500 = 0.07780654,
    p_onArt350500 = 0.11304476,
    p_onArt250350 = 0.238703388,
    p_onArt200250 = 0.195633724,
    p_onArt100200 = 0.225103218,
    p_onArt50100 = 0.093872469,
    p_onArt50 = 0.0558359
)

time <- seq(0, 10, 0.02)
out <- RunSim(time, y, p)
head(out)

plot(out$N, type = 'l', lwd = 2)

out$N[250]
out$N[251]

# Use result preferably

# Target
t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
t0_results <- c(1,t0_dx,t0_cx,t0_tx,t0_vs)



t0_all = t0_N / t0_N # WTF IS THIS SHIT?

result[[1,"N"]]

out$N[1] # t = 0 @ [1]
comp <- c(1, out$Dx[1], out$Care[1], out$Tx[1], out$Vs[1])
expect_equal(t0_results,comp)

t0_DX <- sum(result$Dx[1], result$Care[1], result$PreLtfu[1], result$ART[1], result$Ltfu[1])
t0_CX <- sum(result$Care[1], result$ART[1])
t0_TX <- result$ART[1]
t0_VS <- result$Vs[1]
t0_ <- result


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

df$def <- factor(df$def, levels=c("% Diagnosed", "% On Treatment", "% Suppressed"))
df


DALY = (
    (rowSums(result[, c("UnDx_500", "Dx_500", "Care_500", "PreLtfu_500", "Tx_Na_500", "Ltfu_500",
        "UnDx_350500", "Dx_350500", "Care_350500", "PreLtfu_350500", "Tx_Na_350500", "Ltfu_350500")]) * 0.078) + # >350, no ART

    (rowSums(result[,c("UnDx_250350", "Dx_250350", "Care_250350", "PreLtfu_250350", "Tx_Na_250350", "Ltfu_250350",
        "UnDx_200250", "Dx_200250", "Care_200250", "PreLtfu_200250", "Tx_Na_200250", "Ltfu_200250")]) * 0.274) + # 200-350, no ART

    (rowSums(result[, c("UnDx_100200", "Dx_100200", "Care_100200", "PreLtfu_100200", "Tx_Na_100200", "Ltfu_100200",
        "UnDx_50100", "Dx_50100", "Care_50100", "PreLtfu_50100", "Tx_Na_50100", "Ltfu_50100",
        "UnDx_50", "Dx_50", "Care_50", "PreLtfu_50", "Tx_Na_50", "Ltfu_50")]) * 0.582) + # <200, no ART

    (rowSums(result[, c("Tx_A_500", "Tx_A_350500", "Tx_A_250350", "Tx_A_200250", "Tx_A_100200", "Tx_A_50100", "Tx_A_50")]) * 0.078) # on ART & VS
)


names(out)
sum(out$Dx_Cost)

out$Dx_Cost / out$time
# BLERGH.


GetParaMatrix()
