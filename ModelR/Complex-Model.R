############################
# WHO Cascade Work Package #
#  Dummy ODE Solver in R   #
############################

rm(list=ls())
setwd("~/git/WhoCascade/ModelR")

# install_github("cran/deSolve")
# install.packages("deSolve")
library(deSolve)
require(dplyr)
require(ggplot2)
require(scales)
require(gridExtra)


# Set time step
Time <- seq(0,5,0.02)

source("TheModel.R")
source("Parameters.R")
source("Initial.R")

#############
# THE MODEL #
out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
out <- tbl_df(data.frame(out))
out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
out <- mutate(out,ART = (Tx_500 + Tx_350500 + Tx_200350 + Tx_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
out <- mutate(out,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
out <- mutate(out,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
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

t0_dx = as.double(sum(filter(out,time == 0) %>% select(c(Dx,Tx,Vs))))
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