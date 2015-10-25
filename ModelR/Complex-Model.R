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
require(grid)

# Set time step
# Time <- seq(0,5,0.02)

source("TheModel.R")
source("Parameters.R")
source("Initial.R")

# Googlesheet Spectrum Incidence Values
theTable <- gs_title("SpectrumIncidenceEstimates")
theIncidence <- gs_read(theTable,ws="NewInfections")

# # AIDSinfo estimates (2014)
# theIncidence$NewInfections2014

# # Spectrum estimates (2015)
# # theIncidence$IncidenceRate2015

# Parameters[["Beta"]]
# # [12] = Kenya incidence estimate
# theIncidence$NewInfections2014[12]

# Beta Calculation #
# Beta <- 0
# Beta <- 0.0275837
Numerator <- theIncidence$NewInfections2014[12]
Denominator <- as.double(((Initial[["UnDx_500"]] + Initial[["Dx_500"]] + Initial[["Care_500"]] + Initial[["PreLtfu_500"]] + Initial[["Tx_Na_500"]] + Initial[["Ltfu_500"]]) * 1.35) + ((Initial[["UnDx_350500"]] + Initial[["Dx_350500"]] + Initial[["Care_350500"]] + Initial[["PreLtfu_350500"]] + Initial[["Tx_Na_350500"]] + Initial[["Ltfu_350500"]]) * 1) + ((Initial[["UnDx_250350"]] + Initial[["Dx_250350"]] + Initial[["Care_250350"]] + Initial[["PreLtfu_250350"]] + Initial[["Tx_Na_250350"]] + Initial[["Ltfu_250350"]] + Initial[["UnDx_200250"]] + Initial[["Dx_200250"]] + Initial[["Care_200250"]] + Initial[["PreLtfu_200250"]] + Initial[["Tx_Na_200250"]] + Initial[["Ltfu_200250"]]) * 1.64) + ((Initial[["UnDx_100200"]] + Initial[["Dx_100200"]] + Initial[["Care_100200"]] + Initial[["PreLtfu_100200"]] + Initial[["Tx_Na_100200"]] + Initial[["Ltfu_100200"]] + Initial[["UnDx_50100"]] + Initial[["Dx_50100"]] + Initial[["Care_50100"]] + Initial[["PreLtfu_50100"]] + Initial[["Tx_Na_50100"]] + Initial[["Ltfu_50100"]] + Initial[["UnDx_50"]] + Initial[["Dx_50"]] + Initial[["Care_50"]] + Initial[["PreLtfu_50"]] + Initial[["Tx_Na_50"]] + Initial[["Ltfu_50"]]) * 5.17) + ((Initial[["Tx_A_500"]] + Initial[["Tx_A_350500"]] + Initial[["Tx_A_250350"]] + Initial[["Tx_A_200250"]] + Initial[["Tx_A_100200"]] + Initial[["Tx_A_50100"]] + Initial[["Tx_A_50"]]) * 0.1))
Beta <- Numerator / Denominator
print(paste("Beta =",Beta))

#############
# THE MODEL #
Time <- seq(0,5,0.02)
out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
out <- tbl_df(data.frame(out))
out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50 + Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50 + Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50 + Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50)
out <- mutate(out,ART = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
out <- mutate(out,UnDx = (UnDx_500 + UnDx_350500 + UnDx_250350 + UnDx_200250 + UnDx_100200 + UnDx_50100 + UnDx_50) / N)
out <- mutate(out,Dx = (Dx_500 + Dx_350500 + Dx_250350 + Dx_200250 + Dx_100200 + Dx_50100 + Dx_50) / N)
out <- mutate(out,Care = (Care_500 + Care_350500 + Care_250350 + Care_200250 + Care_100200 + Care_50100 + Care_50) / N)
out <- mutate(out,PreLtfu = (PreLtfu_500 + PreLtfu_350500 + PreLtfu_250350 + PreLtfu_200250 + PreLtfu_100200 + PreLtfu_50100 + PreLtfu_50) / N)
out <- mutate(out,Tx = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_250350 + Tx_Na_200250 + Tx_Na_100200 + Tx_Na_50100 + Tx_Na_50 + Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
out <- mutate(out,Vs = (Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) / N)
out <- mutate(out,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_250350 + Ltfu_200250 + Ltfu_100200 + Ltfu_50100 + Ltfu_50) / N)
out <- mutate(out,NaturalMortalityProp = NaturalMortality / N)
out <- mutate(out,HivMortalityProp = HivMortality / N)
out <- mutate(out,NewInfProp = NewInf / N)
out <- mutate(out,TotalCost = Dx_Cost + Linkage_Cost + Annual_Care_Cost + Annual_ART_Cost)
#############

plot(out$N/out$N[1],type='l',lwd=2)
out$N[1]

# Median just gives the median time of the simulation.
median(out$N)
select(filter(out,N > 59.45 & N < 59.47),time)

# Mean calculation.
mean(out$N)
select(filter(out,N > 95060 & N < 96000),time)

# With >1 person.
mean(as.list(select(filter(out,N > 1),N))$N)
as.double(select(filter(out,N > 138120 & N < 138500),time))
MeanSurival <- as.double(select(filter(out,N > 138120 & N < 138140),time))
MeanSurival

ggplot(out,aes(x=time,y=N/out$N[1])) + 
geom_line() + 
theme_classic() +
xlab("Time (years)") +
ylab("Surival (proportion)") +
ggtitle("Pre-ART Survival (mean = 16.5 years)")

#####################
# CD4 DISTRIBUTIONS #
out <- mutate(out,cd4_500 = (UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Tx_A_500 + Ltfu_500) / N)
out <- mutate(out,cd4_350500 = (UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Tx_A_350500 + Ltfu_350500) / N)
out <- mutate(out,cd4_250350 = (UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Tx_A_250350 + Ltfu_250350) / N)
out <- mutate(out,cd4_200250 = (UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Tx_A_200250 + Ltfu_200250) / N)
out <- mutate(out,cd4_100200 = (UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Tx_A_100200 + Ltfu_100200) / N)
out <- mutate(out,cd4_50100 = (UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Tx_A_50100 + Ltfu_50100) / N)
out <- mutate(out,cd4_50 = (UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Tx_A_50 + Ltfu_50) / N)


# CD4 distribution in 2015 (t0)
t0.cd4_500 <- as.double(filter(out,time == 0) %>% select(cd4_500))
t0.cd4_350500 <- as.double(filter(out,time == 0) %>% select(cd4_350500))
t0.cd4_200350 <- as.double(filter(out,time == 0) %>% select(cd4_200350))
t0.cd4_200 <- as.double(filter(out,time == 0) %>% select(cd4_200))

t0.cd4 <- c(t0.cd4_500, t0.cd4_350500, t0.cd4_200350, t0.cd4_200)
t0.names <- c(">500","350-500","200-350","<200")

t0 <- data.frame(t0.names,t0.cd4)

levels(t0$t0.names)
t0$t0.names <- factor(t0$t0.names, levels=c(">500","350-500","200-350","<200"))

t5.cd4_500 <- as.double(filter(out,time == 5) %>% select(cd4_500))
t5.cd4_350500 <- as.double(filter(out,time == 5) %>% select(cd4_350500))
t5.cd4_200350 <- as.double(filter(out,time == 5) %>% select(cd4_200350))
t5.cd4_200 <- as.double(filter(out,time == 5) %>% select(cd4_200))

t5.cd4 <- c(t5.cd4_500, t5.cd4_350500, t5.cd4_200350, t5.cd4_200)
t5.names <- c(">500","350-500","200-350","<200")

t5 <- data.frame(t5.names,t5.cd4)

levels(t5$t5.names)
t5$t5.names <- factor(t5$t5.names, levels=c(">500","350-500","200-350","<200"))

cd4.t0 <- ggplot(t0,aes(x=t0.names,y=t0.cd4)) +
geom_bar(stat='identity') +
theme_classic() +
xlab("CD4 category") +
ylab("Proportion") +
ggtitle("CD4 distribution in 2015")

cd4.t5 <- ggplot(t5,aes(x=t5.names,y=t5.cd4)) +
geom_bar(stat='identity') +
theme_classic() +
xlab("CD4 category") +
ylab("Proportion") +
ggtitle("CD4 distribution in 2020")

grid.arrange(cd4.t0,cd4.t5,ncol=2)


# CD4 distribution in 2020 (t5)
plot(out$cd4_200350)

mean(out$N/out$N[1])
mean(out$N)
names(out)
plot(out$TxInit_Cost)
plot(out$AnnualTxCost)
out$time
length(out$Tx)
(out$AnnualTxCost)

test <- rowSums(select(out,c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200)))

plot(out$time,out$AnnualTxCost,type='l',lwd=2)

1/0.02
# So if an annual cost of $300 then take 1/50 of it
300 / 50



a <- ggplot(out,aes(x=time,y=UnDx)) +
geom_line() +
theme_classic()

b <- ggplot(out,aes(x=time,y=Dx)) +
geom_line() +
theme_classic()

c <- ggplot(out,aes(x=time,y=Care)) +
geom_line() +
theme_classic()

d <- ggplot(out,aes(x=time,y=PreLtfu)) +
geom_line() +
theme_classic()

e <- ggplot(out,aes(x=time,y=Tx)) +
geom_line() +
theme_classic()

f <- ggplot(out,aes(x=time,y=Vs)) +
geom_line() +
theme_classic()

g <- ggplot(out,aes(x=time,y=Ltfu)) +
geom_line() +
theme_classic()

h <- ggplot(out,aes(x=time,y=N)) +
geom_line() +
theme_classic()

i <- ggplot(out,aes(x=time,y=NewInf)) +
geom_line() +
theme_classic()

j <- ggplot(out,aes(x=time,y=NewInfProp)) +
geom_line() +
theme_classic()

k <- ggplot(out,aes(x=time,y=HivMortalityProp)) +
geom_line() +
theme_classic()

l <- ggplot(out,aes(x=time,y=NaturalMortalityProp)) +
geom_line() +
theme_classic()

graphics.off()
quartz.options(w=8,h=5)
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,nrow=4,ncol=3)

# Survival plot

###############
# CareCascade #
###############

t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N
t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
t0_tx = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N

t0_results <- c(t0_dx,t0_cx,t0_tx,t0_vs,t0_ltfu)

definition <- c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU")
t0 <- data.frame(definition,t0_results)

levels(t0$definition)
t0$definition <- factor(t0$definition, levels=c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU"))

fill.coll <- rev(brewer.pal(9,"Blues")[4:8])

o <- ggplot(t0,aes(definition,t0_results))
o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
o <- o + scale_fill_manual(values=fill.coll)
o <- o + ggtitle("Care Cascade in 2015")
o <- o + theme_classic()
o <- o + theme(title=element_text(size=10))
o <- o + theme(axis.title=element_blank())
o <- o + theme(axis.text.x=element_text(size=9))
o <- o + theme(axis.text.y=element_text(size=9))
o <- o + theme(legend.position="none")

graphics.off()
quartz.options(w=5.5,h=3)
o

t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N
t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
t5_tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N
t5_ltfu = as.double(sum(filter(out,time == 5) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N

t5_results <- c(t5_dx,t5_cx,t5_tx,t5_vs,t5_ltfu)

definition <- c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU")
t5 <- data.frame(definition,t5_results)

levels(t5$definition)
t5$definition <- factor(t5$definition, levels=c("% Diagnosed","% In Care","% On Treatment","% Suppressed","% LTFU"))

fill.coll <- rev(brewer.pal(9,"Blues")[4:8])

p <- ggplot(t5,aes(definition,t5_results))
p <- p + geom_bar(aes(fill=definition),position='dodge',stat='identity')
p <- p + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
p <- p + scale_fill_manual(values=fill.coll)
p <- p + ggtitle("Care Cascade in 2020")
p <- p + theme_classic()
p <- p + theme(title=element_text(size=10))
p <- p + theme(axis.title=element_blank())
p <- p + theme(axis.text.x=element_text(size=9))
p <- p + theme(axis.text.y=element_text(size=9))
p <- p + theme(legend.position="none")

graphics.off()
quartz.options(w=5.5,h=3)
p

grid.arrange(o,p,nrow=1,ncol=2)

#################
# PowersCascade #
#################

t0_N = as.double(sum(filter(out,time == 0) %>% select(N)))
t0_undx = as.double(sum(filter(out,time == 0) %>% select(c(UnDx_500,UnDx_350500,UnDx_250350,UnDx_200250,UnDx_100200,UnDx_50100,UnDx_50)))) / t0_N
t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50)))) / t0_N
t0_cx = as.double(sum(filter(out,time == 0) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50)))) / t0_N
t0_preltfu = as.double(sum(filter(out,time == 0) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50)))) / t0_N
t0_tx_na = as.double(sum(filter(out,time == 0) %>% select(c(Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t0_N
t0_vs = as.double(sum(filter(out,time == 0) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t0_N
t0_ltfu = as.double(sum(filter(out,time == 0) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t0_N

tResult <- c(t0_vs,t0_tx_na,t0_cx,t0_dx,t0_undx,t0_preltfu,t0_ltfu,
             t0_vs,t0_tx_na,t0_cx,t0_dx,t0_preltfu,t0_ltfu,
             t0_vs,t0_tx_na,t0_cx,
             t0_vs,t0_tx_na,
             t0_vs,
             t0_preltfu,t0_ltfu)

State <- c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU",
           "% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% pre-ART LTFU","% LTFU",
           "% Suppressed","% On Treatment (non-adherent)","% In Care",
           "% Suppressed","% On Treatment (non-adherent)",
           "% Suppressed",
           "% pre-ART LTFU","% LTFU")

tOrder <- c(rep("All",7),
            rep("Diagnosed",6),
            rep("In Care",3),
            rep("On Treatment",2),
            rep("Virally Suppressed",1),
            rep("LTFU",2))

t0 <- data.frame(State,tResult,tOrder)

levels(t0$tOrder)
t0$tOrder <- factor(t0$tOrder, levels=c("All","Diagnosed","In Care","On Treatment","Virally Suppressed","LTFU"))

levels(t0$State)
t0$State <- factor(t0$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

cols <- brewer.pal(9,"Set1")
power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

o <- ggplot(t0,aes(x=tOrder,y=tResult,fill=State))
o <- o + geom_bar(stat='identity')
o <- o + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)
o <- o + scale_fill_manual(values=power.col)
o <- o + ggtitle("Care Cascade in 2015")
o <- o + theme_classic()
o <- o + theme(title=element_text(size=10))
o <- o + theme(axis.title=element_blank())
o <- o + theme(axis.text.x=element_text(size=9))
o <- o + theme(axis.text.y=element_text(size=8))
o <- o + theme(legend.text=element_text(size=7))
o <- o + theme(legend.key.size=unit(3,"mm"))

graphics.off()
quartz.options(w=9,h=4)
o

t5_N = as.double(sum(filter(out,time == 5) %>% select(N)))
t5_undx = as.double(sum(filter(out,time == 5) %>% select(c(UnDx_500,UnDx_350500,UnDx_250350,UnDx_200250,UnDx_100200,UnDx_50100,UnDx_50)))) / t5_N
t5_dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50)))) / t5_N
t5_cx = as.double(sum(filter(out,time == 5) %>% select(c(Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50)))) / t5_N
t5_preltfu = as.double(sum(filter(out,time == 5) %>% select(c(PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50)))) / t5_N
t5_tx_na = as.double(sum(filter(out,time == 5) %>% select(c(Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50)))) / t5_N
t5_vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50)))) / t5_N
t5_ltfu = as.double(sum(filter(out,time == 5) %>% select(c(Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50)))) / t5_N


tResult <- c(t5_vs,t5_tx_na,t5_cx,t5_dx,t5_undx,t5_preltfu,t5_ltfu,
             t5_vs,t5_tx_na,t5_cx,t5_dx,t5_preltfu,t5_ltfu,
             t5_vs,t5_tx_na,t5_cx,
             t5_vs,t5_tx_na,
             t5_vs,
             t5_preltfu,t5_ltfu)

State <- c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU",
           "% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% pre-ART LTFU","% LTFU",
           "% Suppressed","% On Treatment (non-adherent)","% In Care",
           "% Suppressed","% On Treatment (non-adherent)",
           "% Suppressed",
           "% pre-ART LTFU","% LTFU")

tOrder <- c(rep("All",7),
            rep("Diagnosed",6),
            rep("In Care",3),
            rep("On Treatment",2),
            rep("Virally Suppressed",1),
            rep("LTFU",2))

t5 <- data.frame(State,tResult,tOrder)

levels(t5$tOrder)
t5$tOrder <- factor(t5$tOrder, levels=c("All","Diagnosed","In Care","On Treatment","Virally Suppressed","LTFU"))

levels(t5$State)
t5$State <- factor(t5$State, levels=c("% Suppressed","% On Treatment (non-adherent)","% In Care","% Diagnosed","% Undiagnosed","% pre-ART LTFU","% LTFU"))

power.col <- c(cols[3],cols[2],cols[4],cols[5],cols[1],cols[9],cols[8])

p <- ggplot(t5,aes(x=tOrder,y=tResult,fill=State))
p <- p + geom_bar(stat='identity')
p <- p + scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)
p <- p + scale_fill_manual(values=power.col)
p <- p + ggtitle("Care Cascade in 2020")
p <- p + theme_classic()
p <- p + theme(title=element_text(size=10))
p <- p + theme(axis.title=element_blank())
p <- p + theme(axis.text.x=element_text(size=9))
p <- p + theme(axis.text.y=element_text(size=8))
p <- p + theme(legend.text=element_text(size=7))
p <- p + theme(legend.key.size=unit(3,"mm"))

graphics.off()
quartz.options(w=9,h=4)
p

#################
# 90-90-90 PLOT #
#################

Plot909090 <- function() {
    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    # dx / PLHIV
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
    # tx / dx
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
    # vs / tx
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx

    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed\nof all PLHIV","% On Treatment\nof all diagnosed","% Suppressed\nof all on ART")
    Scenario <- c("Baseline")
    the909090 <- data.frame(definition,results,Scenario)

    levels(the909090$definition)
    the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed\nof all PLHIV","% On Treatment\nof all diagnosed","% Suppressed\nof all on ART"))

    fill.coll <- brewer.pal(4,"Set1")

    o <- ggplot(the909090,aes(definition,results))
    o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
    o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
    o <- o + scale_fill_manual(values=fill.coll)
    o <- o + geom_abline(intercept=0.9, slope=0)
    o <- o + theme_classic()
    o <- o + theme(title=element_text(size=10))
    o <- o + theme(axis.title=element_blank())
    o <- o + theme(axis.text.x=element_text(size=8))
    o <- o + theme(axis.text.y=element_text(size=9))
    o <- o + theme(legend.position="none")
    print(o)
}

graphics.off()
quartz.options(w=4,h=3)
Plot909090()
# grid.arrange(o,p,nrow=1,ncol=2)

###########################
# 90-90-90 PLOT Version 2 #
###########################

Plot909090v2 <- function() {
    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))

    p_dx <- dx / PLHIV
    p_tx <- tx / PLHIV
    p_vs <- vs / PLHIV

    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed\nof all PLHIV","% On Treatment\nof all PLHIV","% Suppressed\nof all PLHIV")
    Scenario <- c("Baseline")
    the909090 <- data.frame(definition,results,Scenario)

    levels(the909090$definition)
    the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed\nof all PLHIV","% On Treatment\nof all PLHIV","% Suppressed\nof all PLHIV"))

    fill.coll <- brewer.pal(4,"Set1")

    o <- ggplot(the909090,aes(definition,results))
    o <- o + geom_bar(aes(fill=definition),position='dodge',stat='identity')
    o <- o + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1),labels=percent)
    o <- o + scale_fill_manual(values=fill.coll)
    o <- o + geom_segment(aes(x = 0.5, y = 0.9, xend = 1.5, yend = 0.9))
    o <- o + geom_segment(aes(x = 1.5, y = 0.9^2, xend = 2.5, yend = 0.9^2))
    o <- o + geom_segment(aes(x = 2.5, y = 0.9^3, xend = 3.5, yend = 0.9^3))
    o <- o + theme_classic()
    o <- o + theme(title=element_text(size=10))
    o <- o + theme(axis.title=element_blank())
    o <- o + theme(axis.text.x=element_text(size=8))
    o <- o + theme(axis.text.y=element_text(size=9))
    o <- o + theme(legend.position="none")
    print(o)
}

graphics.off()
quartz.options(w=4,h=3)
Plot909090v2()

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