############################
# WHO Cascade Work Package #
#  Dummy ODE Solver in R   #
############################

rm(list=ls())
setwd("~/git/WhoCascade/ModelR")

# install_github("cran/deSolve")
# install.packages("deSolve")
require(deSolve)
require(dplyr)
require(ggplot2)
require(scales)
require(gridExtra)
require(googlesheets)

# Set time step
Time <- seq(0,5,0.02)

source("TheModel.R")
source("Parameters.R")
source("Initial.R")

# Googlesheet Spectrum Incidence Values
theTable <- gs_title("SpectrumIncidenceEstimates")
theIncidence <- gs_read(theTable,ws="NewInfections")

# AIDSinfo estimates (2014)
theIncidence$NewInfections2014

# Spectrum estimates (2015)
# theIncidence$IncidenceRate2015

Parameters[["Beta"]]
# [12] = Kenya incidence estimate
theIncidence$NewInfections2014[12]


# Beta <- as.double(theIncidence$NewInfections2014[12] / (((Initial[1] + Initial[5] + Initial[9] + Initial[13] + Initial[21]) * 1.35) + ((Initial[2] + Initial[6] + Initial[10] + Initial[14] + Initial[22]) * 1) + ((Initial[3] + Initial[7] + Initial[11] + Initial[15] + Initial[23]) * 1.64) + ((Initial[4] + Initial[8] + Initial[12] + Initial[16] + Initial[24]) * 5.17) + ((Initial[17] + Initial[18] + Initial[19] + Initial[20]) * 0.1)))
Beta <- 0.0275837
# Beta <- 0

ComplexCascade <- function(t, y, parms) {

    dUnDx_500 <- 0.58 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) - (parms[4] + parms[1] + parms[10] + parms[18]) * y[1]
    dUnDx_350500 <- 0.23 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[1] * y[1] - (parms[4] + parms[2] + parms[11] + parms[18]) * y[2]
    dUnDx_200350 <- 0.16 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[2] * y[2] - (parms[4] + parms[3] + parms[12] + parms[18]) * y[3]
    dUnDx_200 <- 0.03 * (Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))) + parms[3] * y[3] - (parms[4] + parms[13] + parms[18]) * y[4]

    dDx_500 <- + parms[4] * y[1] - (parms[19] + parms[1] + parms[10] + parms[18]) * y[5]
    dDx_350500 <- + parms[4] * y[2] + parms[1] * y[5] - (parms[19] + parms[2] + parms[11] + parms[18]) * y[6]
    dDx_200350 <- + parms[4] * y[3] + parms[2] * y[6] - (parms[19] + parms[3] + parms[12] + parms[18]) * y[7]
    dDx_200 <- + parms[4] * y[4] + parms[3] * y[7] - (parms[19] + parms[13] + parms[18]) * y[8]

    dCare_500 <- + parms[19] * y[5] - (parms[5] + parms[1] + parms[10] + parms[18]) * y[9]
    dCare_350500 <- + parms[19] * y[6] + parms[1] * y[9] - (parms[5] + parms[2] + parms[11] + parms[18]) * y[10]
    dCare_200350 <- + parms[19] * y[7] + parms[2] * y[10] - (parms[5] + parms[3] + parms[12] + parms[18]) * y[11]
    dCare_200 <- + parms[19] * y[8] + parms[3] * y[11] - (parms[5] + parms[13] + parms[18]) * y[12]

    dTx_500 <- + parms[5] * y[9] - ((parms[7] / 2) + parms[6] + parms[10] + parms[18]) * y[13]
    dTx_350500 <- + parms[5] * y[10] + parms[8] * y[15] - ((parms[7] / 2) + parms[6] + parms[11] + parms[18]) * y[14]
    dTx_200350 <- + parms[5] * y[11] + parms[9] * y[16] - ((parms[7] / 2) + parms[8] + parms[6] + parms[12] + parms[18]) * y[15]
    dTx_200 <- + parms[5] * y[12] - ((parms[7] / 2) + parms[9] + parms[6] + parms[13] + parms[18]) * y[16]

    dVs_500 <- + parms[6] * y[13] - ((parms[7] / 2) + parms[14] + parms[18]) * y[17]
    dVs_350500 <- + parms[6] * y[14] + parms[8] * y[19] - ((parms[7] / 2) + parms[15] + parms[18]) * y[18]
    dVs_200350 <- + parms[6] * y[15] + parms[9] * y[20] - ((parms[7] / 2) + parms[8] + parms[16] + parms[18]) * y[19]
    dVs_200 <- + parms[6] * y[16] - ((parms[7] / 2) + parms[9] + parms[17] + parms[18]) * y[20]

    dLtfu_500 <- + (parms[7] / 2) * (y[13] + y[17]) - (parms[1] + parms[10] + parms[18]) * y[21]
    dLtfu_350500 <- + (parms[7] / 2) * (y[14] + y[18]) + parms[1] * y[21] - (parms[2] + parms[11] + parms[18]) * y[22]
    dLtfu_200350 <- + (parms[7] / 2) * (y[15] + y[19]) + parms[2] * y[22] - (parms[3] + parms[12] + parms[18]) * y[23]
    dLtfu_200 <- + (parms[7] / 2) * (y[16] + y[20]) + parms[3] * y[23] - (parms[13] + parms[18]) * y[24]

    dNewInf <- Beta * (((y[1] + y[5] + y[9] + y[13] + y[21]) * 1.35) + ((y[2] + y[6] + y[10] + y[14] + y[22]) * 1) + ((y[3] + y[7] + y[11] + y[15] + y[23]) * 1.64) + ((y[4] + y[8] + y[12] + y[16] + y[24]) * 5.17) + ((y[17] + y[18] + y[19] + y[20]) * 0.1))

    dHivMortality <- parms[10] * (y[1] + y[5] + y[9] + y[13] + y[21]) + parms[11] * (y[2] + y[6] + y[10] + y[14] + y[22]) + parms[12] * (y[3] + y[7] + y[11] + y[15] + y[23]) + parms[13] * (y[4] + y[8] + y[12] + y[16] + y[24]) + parms[14] * y[17] + parms[15] * y[18] + parms[16] * y[19] + parms[17] * y[20]

    dNaturalMortality <- parms[18] * (y[1] + y[2] + y[3] + y[4] + y[5] + y[6] + y[7] + y[8] + y[9] + y[10] + y[11] + y[12] + y[13] + y[14] + y[15] + y[16] + y[17] + y[18] + y[19] + y[20] + y[21] + y[22] + y[23] + y[24])

    list(c(
        dUnDx_500,
        dUnDx_350500,
        dUnDx_200350,
        dUnDx_200,
        dDx_500,
        dDx_350500,
        dDx_200350,
        dDx_200,
        dCare_500,
        dCare_350500,
        dCare_200350,
        dCare_200,
        dTx_500,
        dTx_350500,
        dTx_200350,
        dTx_200,
        dVs_500,
        dVs_350500,
        dVs_200350,
        dVs_200,
        dLtfu_500,
        dLtfu_350500,
        dLtfu_200350,
        dLtfu_200,
        dNewInf,
        dHivMortality,
        dNaturalMortality))
}

#############
# THE MODEL #
out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
out <- tbl_df(data.frame(out))
out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
out <- mutate(out,ART = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
out <- mutate(out,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
out <- mutate(out,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
out <- mutate(out,Care = (Care_500 + Care_350500 + Care_200350 + Care_200) / N)
out <- mutate(out,Tx = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200) / N)
out <- mutate(out,Vs = (Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
out <- mutate(out,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200) / N)
out <- mutate(out,NaturalMortalityProp = NaturalMortality / N)
out <- mutate(out,HivMortalityProp = HivMortality / N)
out <- mutate(out,NewInfProp = NewInf / N)
#############

a <- ggplot(out,aes(x=time,y=UnDx)) +
geom_line() +
theme_classic()

b <- ggplot(out,aes(x=time,y=Dx)) +
geom_line() +
theme_classic()

c <- ggplot(out,aes(x=time,y=Tx)) +
geom_line() +
theme_classic()

d <- ggplot(out,aes(x=time,y=Vs)) +
geom_line() +
theme_classic()

e <- ggplot(out,aes(x=time,y=Ltfu)) +
geom_line() +
theme_classic()

f <- ggplot(out,aes(x=time,y=N)) +
geom_line() +
theme_classic()

g <- ggplot(out,aes(x=time,y=NewInf)) +
geom_line() +
theme_classic()

h <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
geom_line() +
theme_classic()

i <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
geom_line() +
theme_classic()

graphics.off()
quartz.options(w=8,h=5)

grid.arrange(a,b,c,d,e,f,g,h,i,nrow=3,ncol=3)

#################
# 90-90-90 Test #
#################

dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx,Tx,Vs))))
tx = as.double(filter(out,time == 5) %>% select(ART))
vs = as.double(filter(out,time == 5) %>% select(Vs))

results <- c(dx,tx,vs)
definition <- c("% Diagnosed","% On Treatment","% Suppressed")
Scenario <- c("Baseline")
the909090 <- data.frame(definition,results,Scenario)

levels(the909090$definition)
the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed"))

graphics.off()
quartz.options(w=6,h=4)
fill.coll <- brewer.pal(4,"Set1")

o <- ggplot(the909090,aes(definition,results))
o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
o <- o + scale_fill_manual(values=fill.coll)
o <- o + geom_abline(intercept=0.9, slope=0)
o <- o + theme_classic()
o <- o + theme(axis.title=element_blank())
o <- o + theme(axis.text.x=element_text(size=12))
o <- o + theme(axis.text.y=element_text(size=10))
o <- o + theme(legend.position="none")
o

system("mkdir ./plots")
quartz.save(file='./plots/90-90-90.pdf',type='pdf')

###########################################
# Care Cascade Cross Section at t0 and t5 #
###########################################

# denominator here should be all PLHIV. Then 90-90-90 is different.

t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx,Care,Tx,Vs))))
t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx,Vs))))
t0_vs = as.double(filter(out,time == 0) %>% select(Vs))
t0_ltfu = as.double(filter(out,time == 0) %>% select(Ltfu))

t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx,Tx,Vs))))
t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx,Vs))))
t5_vs = as.double(filter(out,time == 5) %>% select(Vs))
t5_ltfu = as.double(filter(out,time == 5) %>% select(Ltfu))

t0_results <- c(t0_dx,t0_tx,t0_vs,t0_ltfu)
t5_results <- c(t5_dx,t5_tx,t5_vs,t5_ltfu)

definition <- c("% Diagnosed","% On Treatment","% Suppressed","% LTFU")
t0 <- data.frame(definition,t0_results)

levels(t0$definition)
t0$definition <- factor(t0$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed","% LTFU"))

graphics.off()
quartz.options(w=6,h=4)
fill.coll <- rev(brewer.pal(9,"Blues")[5:8])

o <- ggplot(t0,aes(definition,t0_results))
o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
o <- o + scale_fill_manual(values=fill.coll)
o <- o + ggtitle("Care Cascade in 2015")
o <- o + theme_classic()
o <- o + theme(axis.title=element_blank())
o <- o + theme(axis.text.x=element_text(size=12))
o <- o + theme(axis.text.y=element_text(size=10))
o <- o + theme(legend.position="none")
o

# quartz.save(file='./plots/Cascade-t0.pdf',type='pdf')

t5 <- data.frame(definition,t5_results)

levels(t5$definition)
t5$definition <- factor(t5$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed","% LTFU"))

graphics.off()
quartz.options(w=6,h=4)
fill.coll <- rev(brewer.pal(9,"Blues")[5:8])

p <- ggplot(t5,aes(definition,t5_results))
p <- p + geom_bar(aes(fill=definition),position='dodge',stat='identity')
p <- p + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
p <- p + scale_fill_manual(values=fill.coll)
p <- p + ggtitle("Care Cascade in 2020")
p <- p + theme_classic()
p <- p + theme(axis.title=element_blank())
p <- p + theme(axis.text.x=element_text(size=12))
p <- p + theme(axis.text.y=element_text(size=10))
p <- p + theme(legend.position="none")
p

# quartz.save(file='./plots/Cascade-t5.pdf',type='pdf')

graphics.off()
quartz.options(w=12,h=4)

grid.arrange(o,p,nrow=1)

quartz.save(file='./plots/Cascade.pdf',type='pdf')

##################
# New Infections #
##################

ggplot(out,aes(x=time,y=HivMortality)) +
geom_line() +
theme_classic()

o <- ggplot(out, aes(x=time,y=NewInf))
o <- o + geom_line(size=0.5)
o <- o + xlab("Year")
o <- o + ylab("New Infections")
o <- o + scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
o <- o + scale_y_continuous(limits=c(0,900), breaks=seq(0,900,100))
o <- o + ggtitle("New Infections between 2015 and 2020")
o <- o + theme_classic()
o <- o + theme(axis.text=element_text(size=10))
o <- o + theme(axis.title=element_text(size=10))
o

p <- ggplot(out, aes(x=time,y=HivMortality))
p <- p + geom_line(size=0.5)
p <- p + xlab("Year")
p <- p + ylab("AIDS deaths")
p <- p + scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
p <- p + scale_y_continuous(limits=c(0,900), breaks=seq(0,900,100))
p <- p + ggtitle("AIDS deaths between 2015 and 2020")
p <- p + theme_classic()
p <- p + theme(axis.text=element_text(size=10))
p <- p + theme(axis.title=element_text(size=10))
p

graphics.off()
quartz.options(w=12,h=4)

grid.arrange(o,p,nrow=1)

quartz.save(file='./plots/NewInf-AidsDeaths.pdf',type='pdf')


p <- ggplot(out, aes(x=time,y=ART)) + 
geom_line() + 
theme_classic() +
theme(axis.text.x=element_text(size=20)) +
theme(axis.text.y=element_text(size=20)) +
theme(axis.title=element_text(size=20)) +
scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1),labels=seq(2015,2020,1))
p