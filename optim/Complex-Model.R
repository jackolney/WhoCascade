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

# Time <- seq(0,5,0.02)
# out <- ode(times=Time, y=Initial, func=ComplexCascade, parms=Parameters)
# out <- tbl_df(data.frame(out))
# out <- mutate(out,N = UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200 + Dx_500 + Dx_350500 + Dx_200350 + Dx_200 + Care_500 + Care_350500 + Care_200350 + Care_200 + PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200 + Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200 + Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200)
# out <- mutate(out,ART = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
# out <- mutate(out,UnDx = (UnDx_500 + UnDx_350500 + UnDx_200350 + UnDx_200) / N)
# out <- mutate(out,Dx = (Dx_500 + Dx_350500 + Dx_200350 + Dx_200) / N)
# out <- mutate(out,Care = (Care_500 + Care_350500 + Care_200350 + Care_200) / N)
# out <- mutate(out,PreLtfu = (PreLtfu_500 + PreLtfu_350500 + PreLtfu_200350 + PreLtfu_200) / N)
# out <- mutate(out,Tx = (Tx_Na_500 + Tx_Na_350500 + Tx_Na_200350 + Tx_Na_200 + Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200) / N)
# out <- mutate(out,Adherence = (Tx_A_500 + Tx_A_350500 + Tx_A_200350 + Tx_A_200 + Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
# out <- mutate(out,Vs = (Vs_500 + Vs_350500 + Vs_200350 + Vs_200) / N)
# out <- mutate(out,Ltfu = (Ltfu_500 + Ltfu_350500 + Ltfu_200350 + Ltfu_200) / N)
# out <- mutate(out,NaturalMortalityProp = NaturalMortality / N)
# out <- mutate(out,HivMortalityProp = HivMortality / N)
# out <- mutate(out,NewInfProp = NewInf / N)


find909090 <- function(target, par) {

    # print(paste("par =",par))

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

    # Cost
    cost <- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    # print(paste("cost = ",dollar(cost)))

    # Returning outputs
    output <- 1/3 * sum((target - the909090$results)^2)
    # output <- sum((target - the909090$results)^2)
    print(paste("error =",output,"cost =",dollar(cost)))
    return(output)
}

# willThisWork <- optim(par = c(0,0,0,0), find909090, target = 0.9, lower = c(0.01,0.01,0.01,0.05), upper = c(5,5,5,5), method = 'L-BFGS-B')
# willThisWork$par

# ModFit method
require(FME)
res <- modFit(find909090,
    p = c(
        Rho = 0.205,
        Gamma = 2.556,
        Epsilon = 16.949,
        Omega = 0.033,
        Kappa = 1.079,
        Sigma_a = 0.5,
        Phi = 3.628,
        Psi = 0.431,
        Eta = 0.476),
    target = 0.9,
    lower = c(
        Rho = 0.01,
        Gamma = 0.01,
        Epsilon = 0.01,
        Omega = 0.01,
        Kappa = 0.01,
        Sigma_a = 0.01,
        Phi = 0.01,
        Psi = 0.01,
        Eta = 0.01),
    upper = c(
        Rho = 20,
        Gamma = 20,
        Epsilon = 20,
        Omega = 20,
        Kappa = 20,
        Sigma_a = 20,
        Phi = 20,
        Psi = 20,
        Eta = 20),
    method = 'L-BFGS-B')
res

# Optimisation method which takes a matrix? Containing randomised parameter values? Or let the optim() method walk through them?

# LHC sampling parameter values
require(FME)
parRange <- data.frame(
    min = c(
        Rho = 0.01,
        Gamma = 0.01,
        Epsilon = 0.01,
        Omega = 0.01,
        Kappa = 0.01,
        Sigma_a = 0.01,
        Phi = 0.01,
        Psi = 0.01,
        Eta = 0.01),
    max = c(
        Rho = 20,
        Gamma = 20,
        Epsilon = 20,
        Omega = 20,
        Kappa = 20,
        Sigma_a = 20,
        Phi = 20,
        Psi = 20,
        Eta = 20))

a <- Latinhyper(parRange,100)

# For some interventions
parRange <- data.frame(
    off = c(
        Testing = 0,
        Linkage = 0
        ),
    on = c(
        Testing = 1,
        Linakge = 1
        )
    )

# The number of combinations of 9 parameters that need testing?
factorial(9)

# Extract a value from LHC sample
a[[1,"Rho"]]

# What are we trying to do?
# -> For a given set of changes to the system (interventions), what is the impact / cost?
# -> Help us minimise error towards 90-90-90 for the least money?
# -> For a given amount of money, how can we maximise health benefit?
# -> For a given health benefit, how cheap can it be?


##############################
install.packages("Rsolnp")
require(Rsolnp)

#specify your function
# find909090() -> This is the function to optimise... i.e. aiming towards 90-90-90
find909090 <- function(target, par) {

    # print(paste("par =",par))

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

    # Cost
    cost <- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    print(paste("cost = ",cost))

    # Returning outputs
    output <- 1/3 * sum((target - the909090$results)^2)
    # output <- sum((target - the909090$results)^2)
    # print(paste("error =",output,"cost =",dollar(cost)))
    return(output)
}

#specify the equality function. The number 15 (to which the function is equal)
#is specified as an additional argument
# This f(x) then should be the cost function. i.e. we set a cost?
ForCost <- function(cost,target) {
    print(cost)
    cost[1]
}

#the optimiser - minimises by default (that is cool).
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
    find909090, # Function to optimise
    eqfun=ForCost, # Equality function
    eqB=100e+6,   # The equality constraint
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

# AIM = Minimise cost AND get to 90-90-90.
# so smallest totalCost


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
    # out <- sum((target - the909090$results)^2)
    out <- (target - the909090$results[1])^2
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