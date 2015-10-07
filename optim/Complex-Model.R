############################
# WHO Cascade Work Package #
#  Dummy ODE Solver in R   #
############################

rm(list=ls())
setwd("~/git/WhoCascade/optim")

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

# Just using a boring beta value for now
Beta <- 0.0275837
# Beta <- 0

# --------- #
# THE MODEL #
# --------- #

# Need a f(x) that is able to produce the estimate that we need.
# Start with just the first 90. (then scale up to get all three.)
# Start with just varying ONE parameter.

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

dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx,Tx,Vs))))
tx = as.double(filter(out,time == 5) %>% select(ART))
vs = as.double(filter(out,time == 5) %>% select(Vs))

results <- c(dx,tx,vs)
definition <- c("% Diagnosed","% On Treatment","% Suppressed")
the909090 <- data.frame(definition,results)

# Optimise this.
the909090$results[1]

# Cost testing.

ggplot(out,aes(x=time,y=Dx_Cost)) + geom_line() +
geom_line(aes(x=time,y=Care_Cost),color="red") +
geom_line(aes(x=time,y=Tx_Cost),color="green") +
geom_line(aes(x=time,y=Retention_Cost),color="blue")

out$Dx_Cost
out$Care_Cost
out$Tx_Cost
out$Retention_Cost

# But if we are minimising it then we need for it to equal zero?
# Minimise the squared difference between value and 0.9.

target <- 0.9

(target - the909090$results[1])^2

# ---- #
# TEST #
# ---- #

# First the first 90.

findFirst90 <- function(target, par) {
    
    print(paste("par =",par))

    Parameters <- c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = par,
        Gamma = 0.5,
        Theta = 2,
        Omega = 0.01,
        Delta_1 = 1.1491019,
        Delta_2 = 2.5468165,
        Alpha_1 = 0.0043812,
        Alpha_2 = 0.0179791,
        Alpha_3 = 0.0664348,
        Alpha_4 = 0.1289688,
        Tau_1 = 0.0041621,
        Tau_2 = 0.0170798,
        Tau_3 = 0.0631120,
        Tau_4 = 0.1225184,
        Mu = 0.0374,
        Epsilon = 0.5
    )

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


    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)
    out <- sum((target - the909090$results)^2)
    # out <- (target - the909090$results[1])^2
    return(out)
}

theResult <- optim(par = 0, findFirst90, target = 0.9, lower = 0, upper = 10)
theResult

# ----------------- #
# FIND ALL 90-90-90 #
# ----------------- #

find909090 <- function(target, par) {
    
    print(paste("par =",par))

    Parameters <- c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = par[1],
        Gamma = par[2],
        Theta = 2,
        Omega = par[4],
        Delta_1 = 1.1491019,
        Delta_2 = 2.5468165,
        Alpha_1 = 0.0043812,
        Alpha_2 = 0.0179791,
        Alpha_3 = 0.0664348,
        Alpha_4 = 0.1289688,
        Tau_1 = 0.0041621,
        Tau_2 = 0.0170798,
        Tau_3 = 0.0631120,
        Tau_4 = 0.1225184,
        Mu = 0.0374,
        Epsilon = par[3]
    )

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


    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)
    out <- sum((target - the909090$results)^2)
    # out <- (target - the909090$results[1])^2
    return(out)
}

theResult <- optim(par = c(0,0,0,0), find909090, target = 0.9, lower = 0, upper = 10)
theResult$par

# ToDo:
# Get a decent search algorithm going - DONE.
# Add upper / lower bounds - DONE.
# Specify different bounds for each par[]. - DONE.
# After optimisation, re-run simulation so that we can visualise the results immediately.

testResult <- optim(par = c(0,0,0,0), find909090, target = 0.9, lower = 0.01, upper = 10, method = 'L-BFGS-B')
testResult$par
theResult$par
testResult2 <- optim(par = c(0,0,0,0), find909090, target = 0.9, lower = c(0.01,0.01,0.01,0.05), upper = c(5,5,5,5), method = 'L-BFGS-B')
testResult2$par

# Answer #
theResult
# ------ #

# Alternative method, using {FME}.

require(FME)

find909090_alt <- function(target, p) {
    
    print(paste("p =",p))

    Parameters <- c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = p[1],
        Gamma = p[2],
        Theta = p[3],
        Omega = p[4],
        Delta_1 = 1.1491019,
        Delta_2 = 2.5468165,
        Alpha_1 = 0.0043812,
        Alpha_2 = 0.0179791,
        Alpha_3 = 0.0664348,
        Alpha_4 = 0.1289688,
        Tau_1 = 0.0041621,
        Tau_2 = 0.0170798,
        Tau_3 = 0.0631120,
        Tau_4 = 0.1225184,
        Mu = 0.0374,
        Epsilon = p[5]
    )

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


    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)
    out <- sum((target - the909090$results)^2)
    # out <- (target - the909090$results[1])^2
    return(out)
}

test <- modFit(find909090_alt,p = c(0,0,0,0,0),target = 0.9, lower = rep(0,5), upper = rep(10,5), method = "Port")
theResult$par
    # Parameters <- c(
    #     Nu_1 = 0.2139008,
    #     Nu_2 = 0.3379898,
    #     Nu_3 = 0.2744363,
    #     Rho = 0.280838,
    #     Gamma = 0.5,
    #     Theta = 2,
    #     Omega = 0.01,
    #     Delta_1 = 1.1491019,
    #     Delta_2 = 2.5468165,
    #     Alpha_1 = 0.0043812,
    #     Alpha_2 = 0.0179791,
    #     Alpha_3 = 0.0664348,
    #     Alpha_4 = 0.1289688,
    #     Tau_1 = 0.0041621,
    #     Tau_2 = 0.0170798,
    #     Tau_3 = 0.0631120,
    #     Tau_4 = 0.1225184,
    #     Mu = 0.0374,
    #     Epsilon = 0.5
    # )

    Parameters <- c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = 0.2651495,
        Gamma = 0.6180978,
        Theta = 0.7914529,
        Omega = 0,
        Delta_1 = 1.1491019,
        Delta_2 = 2.5468165,
        Alpha_1 = 0.0043812,
        Alpha_2 = 0.0179791,
        Alpha_3 = 0.0664348,
        Alpha_4 = 0.1289688,
        Tau_1 = 0.0041621,
        Tau_2 = 0.0170798,
        Tau_3 = 0.0631120,
        Tau_4 = 0.1225184,
        Mu = 0.0374,
        Epsilon = 1.4354894
    )


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


    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_500,Tx_350500,Tx_200350,Tx_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)

levels(the909090$definition)
the909090$definition <- factor(the909090$definition, levels=c("% Diagnosed","% On Treatment","% Suppressed"))
the909090

sum((target - the909090$results)^2)

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