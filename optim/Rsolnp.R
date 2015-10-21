############################
# WHO Cascade Work Package #
#  Dummy ODE Solver in R   #
############################

rm(list=ls())
setwd("~/git/WhoCascade/optim")

# install.packages("Rsolnp")
# install_github("cran/deSolve")
# install.packages("deSolve")
require(deSolve)
require(dplyr)
require(ggplot2)
require(scales)
require(gridExtra)
require(googlesheets)
require(Rsolnp)

# load model files
source("TheModel.R")
source("Parameters.R")
source("Initial.R")

# Just using a boring beta value for now
Beta <- 0.0275837
# Beta <- 0

# ------------------------- #
# THE OPTIMISATION FUNCTION #
# ------------------------- #

FindInitialCost <- function() {

    Parameters <- c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.163484,
        Rho = 0.205,
        Epsilon = 16.949,
        Kappa = 1.079,
        Gamma = 2.556,
        Eta = 0.476,
        Phi = 3.628,
        Psi = 0.431,
        Theta = 2.28,
        Omega = 0.033,
        p = 0.95,
        s_1 = 0.25,
        s_2 = 0.75,
        s_3 = 1,
        s_4 = 2,
        Sigma_a = 0.5,
        Sigma_b = 0.5,
        Delta_1 = 1.58084765,
        Delta_2 = 3.50371789,
        Alpha_1 = 0.00411,
        Alpha_2 = 0.01167,
        Alpha_3 = 0.01289,
        Alpha_4 = 0.385832,
        Tau_1 = 0.04013346,
        Tau_2 = 0.05431511,
        Tau_3 = 0.15692556,
        Tau_4 = 0.23814569,
        Mu = 0.0374,
        Dx_unitCost = 10,
        Care_unitCost = 12,
        TxInit_unitCost = 28,
        Retention_unitCost = 367,
        AnnualTx_unitCost = 367
    )

    Time <- seq(0,5,0.02)
    out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
    out <- tbl_df(data.frame(out))
    out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
    out <- mutate(out,TotalCost = Dx_Cost + Care_Cost + TxInit_Cost + Retention_Cost + AnnualTxCost)
    cost <- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    return(dollar(cost))
}

FindInitialCost()

# ----- #
# solnp #
# ----- # 

# Specify your function
Find909090 <- function(target, par) {

    Parameters <- c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.163484,
        Rho = par[["Rho"]],
        Epsilon = par[["Epsilon"]],
        Kappa = par[["Kappa"]],
        Gamma = par[["Gamma"]],
        Eta = par[["Eta"]],
        Phi = par[["Phi"]],
        Psi = par[["Psi"]],
        Theta = 2.28,
        Omega = par[["Omega"]],
        p = 0.95,
        s_1 = 0.25,
        s_2 = 0.75,
        s_3 = 1,
        s_4 = 2,
        Sigma_a = par[["Sigma_a"]],
        Sigma_b = 0.5,
        Delta_1 = 1.58084765,
        Delta_2 = 3.50371789,
        Alpha_1 = 0.00411,
        Alpha_2 = 0.01167,
        Alpha_3 = 0.01289,
        Alpha_4 = 0.385832,
        Tau_1 = 0.04013346,
        Tau_2 = 0.05431511,
        Tau_3 = 0.15692556,
        Tau_4 = 0.23814569,
        Mu = 0.0374,
        Dx_unitCost = 10,
        Care_unitCost = 12,
        TxInit_unitCost = 28,
        Retention_unitCost = 367,
        AnnualTx_unitCost = 367
    )

    Time <- seq(0,5,0.02)
    out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
    out <- tbl_df(data.frame(out))
    out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
    out <- mutate(out,TotalCost = Dx_Cost + Care_Cost + TxInit_Cost + Retention_Cost + AnnualTxCost)
    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,PreLtfu_500,PreLtfu_350500,PreLtfu_200350,PreLtfu_200,Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)

    # Outputs to return
    cost <<- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    output <- 1/3 * sum((target - the909090$results)^2)
    print(paste("Error =",output,"Cost =",dollar(cost)))
    return(output)
}

# Specify the equality function.
ForCost <- function(pars,target) {
    return(cost[1])
}

# The optimiser - minimises by default (that is cool).
Budget = 14e+6

solnp(pars = c(
        Rho = 0.205,
        Gamma = 2.556,
        Epsilon = 16.949,
        Omega = 0.033,
        Kappa = 1.079,
        Sigma_a = 0.5,
        Phi = 3.628,
        Psi = 0.431,
        Eta = 0.476), # Starting values
    Find909090, # Function to optimise
    eqfun=ForCost, # Equality function
    eqB=Budget,   # The equality constraint
    LB = c(
        Rho = 0.01,
        Gamma = 0.01,
        Epsilon = 0.01,
        Omega = 0.01,
        Kappa = 0.01,
        Sigma_a = 0.01,
        Phi = 0.01,
        Psi = 0.01,
        Eta = 0.01), # Lower bound for parameters i.e. greater than zero
    UB = c(
        Rho = 20,
        Gamma = 20,
        Epsilon = 20,
        Omega = 20,
        Kappa = 20,
        Sigma_a = 20,
        Phi = 20,
        Psi = 20,
        Eta = 20), # Upper bound for parameters (I just chose 100 randomly)
        target = 0.9) # Additional arguments (should be given to both main and equality f(x) regardless of whether they are used by both or not).


##############################

# Perhaps an intervention just involves constraining the bounds of a parameter in a certain way? (but tightly-ish.)

# e.g. a testing intervention.
# Rho = 0.205 at baseline.
# So maximum is 0.205 (4.87yrs)
# min = 0.41

par <- data.frame(
    min = c(Rho = 0.41),
    max = c(Rho = 0.205))

params <- Latinhyper(par,100)


# Test loop.

for(i in 1:length(params)) {

    target <- 0.9

    Parameters <- c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.163484,
        Rho = params[[i,"Rho"]],
        Epsilon = 16.949,
        Kappa = 1.079,
        Gamma = 2.556,
        Eta = 0.476,
        Phi = 3.628,
        Psi = 0.431,
        Theta = 2.28,
        Omega = 0.033,
        p = 0.95,
        s_1 = 0.25,
        s_2 = 0.75,
        s_3 = 1,
        s_4 = 2,
        Sigma_a = 0.5,
        Sigma_b = 0.5,
        Delta_1 = 1.58084765,
        Delta_2 = 3.50371789,
        Alpha_1 = 0.00411,
        Alpha_2 = 0.01167,
        Alpha_3 = 0.01289,
        Alpha_4 = 0.385832,
        Tau_1 = 0.04013346,
        Tau_2 = 0.05431511,
        Tau_3 = 0.15692556,
        Tau_4 = 0.23814569,
        Mu = 0.0374,
        Dx_unitCost = 10,
        Care_unitCost = 12,
        TxInit_unitCost = 28,
        Retention_unitCost = 367,
        AnnualTx_unitCost = 367
    )

    Time <- seq(0,5,0.02)
    out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
    out <- tbl_df(data.frame(out))
    out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
    out <- mutate(out,TotalCost = Dx_Cost + Care_Cost + TxInit_Cost + Retention_Cost + AnnualTxCost)
    PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,PreLtfu_500,PreLtfu_350500,PreLtfu_200350,PreLtfu_200,Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
    tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
    vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
    p_dx <- dx / PLHIV
    p_tx <- tx / dx
    p_vs <- vs / tx
    results <- c(p_dx,p_tx,p_vs)
    definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    the909090 <- data.frame(definition,results)

    # Cost
    cost <- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    # print(paste("cost = ",dollar(cost)))

    # Returning outputs
    output <- 1/3 * sum((target - the909090$results)^2)
    # output <- sum((target - the909090$results)^2)
    print(paste("error =",output,"cost =",dollar(cost)))

}

# I should be optimising 90-90-90... but how to include the cost component too?

# Right now it is just reducing cost NOT focusing on getting to 90-90-90.

res$par <- c(
 Rho = 0.2050000,
 Gamma = 2.6340549,
 Epsilon = 16.9475865,
 Omega = 0.0100000,
 Kappa = 0.9487229,
 Sigma_a = 0.7561403,
 Phi = 3.6366206,
 Psi = 0.5768007,
 Eta = 0.8563607
 )


# Parameters <- c(
#     Nu_1 = 0.193634,
#     Nu_2 = 0.321304,
#     Nu_3 = 0.163484,
#     Rho = res$par[["Rho"]],
#     Epsilon = res$par[["Epsilon"]],
#     Kappa = res$par[["Kappa"]],
#     Gamma = res$par[["Gamma"]],
#     Eta = res$par[["Eta"]],
#     Phi = res$par[["Phi"]],
#     Psi = res$par[["Psi"]],
#     Theta = 2.28,
#     Omega = res$par[["Omega"]],
#     p = 0.95,
#     s_1 = 0.25,
#     s_2 = 0.75,
#     s_3 = 1,
#     s_4 = 2,
#     Sigma_a = res$par[["Sigma_a"]],
#     Sigma_b = 0.5,
#     Delta_1 = 1.58084765,
#     Delta_2 = 3.50371789,
#     Alpha_1 = 0.00411,
#     Alpha_2 = 0.01167,
#     Alpha_3 = 0.01289,
#     Alpha_4 = 0.385832,
#     Tau_1 = 0.04013346,
#     Tau_2 = 0.05431511,
#     Tau_3 = 0.15692556,
#     Tau_4 = 0.23814569,
#     Mu = 0.0374,
#     Dx_unitCost = 10,
#     Care_unitCost = 12,
#     TxInit_unitCost = 28,
#     Retention_unitCost = 367,
#     AnnualTx_unitCost = 367
# )


Parameters <- c(
    Nu_1 = 0.193634,
    Nu_2 = 0.321304,
    Nu_3 = 0.163484,
    Rho = 0.2050000,
    Epsilon = 16.9475865,
    Kappa = 0.9487229,
    Gamma = 2.6340549,
    Eta = 0.8563607,
    Phi = 3.6366206,
    Psi = 0.5768007,
    Theta = 2.28,
    Omega = 0.0100000,
    p = 0.95,
    s_1 = 0.25,
    s_2 = 0.75,
    s_3 = 1,
    s_4 = 2,
    Sigma_a = 0.7561403,
    Sigma_b = 0.5,
    Delta_1 = 1.58084765,
    Delta_2 = 3.50371789,
    Alpha_1 = 0.00411,
    Alpha_2 = 0.01167,
    Alpha_3 = 0.01289,
    Alpha_4 = 0.385832,
    Tau_1 = 0.04013346,
    Tau_2 = 0.05431511,
    Tau_3 = 0.15692556,
    Tau_4 = 0.23814569,
    Mu = 0.0374,
    Dx_unitCost = 10,
    Care_unitCost = 12,
    TxInit_unitCost = 28,
    Retention_unitCost = 367,
    AnnualTx_unitCost = 367
)

Time <- seq(0,5,0.02)
out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
out <- tbl_df(data.frame(out))
out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
out <- mutate(out,TotalCost = Dx_Cost + Care_Cost + TxInit_Cost + Retention_Cost + AnnualTxCost)
PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_200350,Dx_200,Care_500,Care_350500,Care_200350,Care_200,PreLtfu_500,PreLtfu_350500,PreLtfu_200350,PreLtfu_200,Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200,Ltfu_500,Ltfu_350500,Ltfu_200350,Ltfu_200))))
tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_200350,Tx_A_200,Tx_Na_500,Tx_Na_350500,Tx_Na_200350,Tx_Na_200,Vs_500,Vs_350500,Vs_200350,Vs_200))))
vs = as.double(sum(filter(out,time == 5) %>% select(c(Vs_500,Vs_350500,Vs_200350,Vs_200))))
p_dx <- dx / PLHIV
p_tx <- tx / dx
p_vs <- vs / tx
results <- c(p_dx,p_tx,p_vs)
definition <- c("% Diagnosed","% On Treatment","% Suppressed")
the909090 <- data.frame(definition,results)

# graphics.off()
# quartz.options(w=6,h=4)
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