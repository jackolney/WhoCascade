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
require(pryr)

# Set time step
# Time <- seq(0,5,0.02)

# Googlesheet Spectrum Incidence Values
theTable <- gs_title("SpectrumIncidenceEstimates")
theIncidence <- gs_read(theTable,ws="NewInfections")

# Googlesheet CD4 distributions
theCD4 <- gs_read(theTable,ws="CD4-Distribution")
SelectedCountry <- "Kenya"
as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.500))

prop_preART_500 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.500))
prop_preART_350500 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.350500))
prop_preART_250350 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.250350))
prop_preART_200250 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.200250))
prop_preART_100200 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.100200))
prop_preART_50100 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.50100))
prop_preART_50 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.Off.ART.50))

prop_onART_500 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.500))
prop_onART_350500 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.350500))
prop_onART_250350 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.250350))
prop_onART_200250 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.200250))
prop_onART_100200 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.100200))
prop_onART_50100 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.50100))
prop_onART_50 <<- as.double(filter(theCD4,Country == SelectedCountry) %>% select(prop.On.ART.50))

source("TheModel.R")
source("Parameters.R")
source("Initial.R")

# Beta Calculation #
# Beta <- 0
# Beta <- 0.0275837
Numerator <- theIncidence$NewInfections2014[12]
Denominator <- as.double(((Initial[["UnDx_500"]] + Initial[["Dx_500"]] + Initial[["Care_500"]] + Initial[["PreLtfu_500"]] + Initial[["Tx_Na_500"]] + Initial[["Ltfu_500"]]) * 1.35) + ((Initial[["UnDx_350500"]] + Initial[["Dx_350500"]] + Initial[["Care_350500"]] + Initial[["PreLtfu_350500"]] + Initial[["Tx_Na_350500"]] + Initial[["Ltfu_350500"]]) * 1) + ((Initial[["UnDx_250350"]] + Initial[["Dx_250350"]] + Initial[["Care_250350"]] + Initial[["PreLtfu_250350"]] + Initial[["Tx_Na_250350"]] + Initial[["Ltfu_250350"]] + Initial[["UnDx_200250"]] + Initial[["Dx_200250"]] + Initial[["Care_200250"]] + Initial[["PreLtfu_200250"]] + Initial[["Tx_Na_200250"]] + Initial[["Ltfu_200250"]]) * 1.64) + ((Initial[["UnDx_100200"]] + Initial[["Dx_100200"]] + Initial[["Care_100200"]] + Initial[["PreLtfu_100200"]] + Initial[["Tx_Na_100200"]] + Initial[["Ltfu_100200"]] + Initial[["UnDx_50100"]] + Initial[["Dx_50100"]] + Initial[["Care_50100"]] + Initial[["PreLtfu_50100"]] + Initial[["Tx_Na_50100"]] + Initial[["Ltfu_50100"]] + Initial[["UnDx_50"]] + Initial[["Dx_50"]] + Initial[["Care_50"]] + Initial[["PreLtfu_50"]] + Initial[["Tx_Na_50"]] + Initial[["Ltfu_50"]]) * 5.17) + ((Initial[["Tx_A_500"]] + Initial[["Tx_A_350500"]] + Initial[["Tx_A_250350"]] + Initial[["Tx_A_200250"]] + Initial[["Tx_A_100200"]] + Initial[["Tx_A_50100"]] + Initial[["Tx_A_50"]]) * 0.1))
Beta <- Numerator / Denominator
print(paste("Beta =",Beta))


# Create matrix of interventions and parameter values
# Starting with just one intervention (rho)
# Write function to take parameter inputs and values, returns result matrix.
# Save matrix in accessible list.
# Expand.
# Create a matrix / dataframe that has, 4096 rows, with 6 columns specifying the values for each parameter (reshape::melt()?)

ParameterMatrix <- matrix(0,4,6)

Rho.Range <- seq(from = 0.205,to = 20,length.out = 4)
Epsilon.Range <- seq(from = 16.949,to = 30,length.out = 4)
Kappa.Range <- seq(from = 0.01,to = 1.079,length.out = 4)
Gamma.Range <- seq(from = 2.556,to = 20,length.out = 4)
Sigma.Range <- seq(from = 0,to = 5,length.out = 4)
Omega.Range <- seq(from = 0.01,to = 0.033,length.out = 4)

# Epsilon.Range <- 16.949
# Kappa.Range <- 0.01
# Gamma.Range <- 2.556
# Sigma.Range <- 0
# Omega.Range <- 0.01

ParameterMatrix[,1] <- Rho.Range
ParameterMatrix[,2] <- Epsilon.Range
ParameterMatrix[,3] <- Kappa.Range
ParameterMatrix[,4] <- Gamma.Range
ParameterMatrix[,5] <- Sigma.Range
ParameterMatrix[,6] <- Omega.Range

colnames(ParameterMatrix) <- c("Rho","Epsilon","Kappa","Gamma","Sigma","Omega")
rownames(ParameterMatrix) <- paste("p",seq(1,dim(ParameterMatrix)[1],1),sep='')
ParameterMatrix

# Use expand.grid() to list all possible combinations of interventions
ParInput <- expand.grid(
    Rho = Rho.Range,
    Epsilon = Epsilon.Range,
    Kappa = Kappa.Range,
    Gamma = Gamma.Range,
    Sigma = Sigma.Range,
    Omega = Omega.Range
    )

# ParInput
head(ParInput)
dim(ParInput)

# ----- #
# STUFF #
# ----- #

# Specify your function
RunSimulation <- function(par,target) {

    Parameters <- c(
        Nu_1 = 0.193634,
        Nu_2 = 0.321304,
        Nu_3 = 0.328285,
        Nu_4 = 0.497247,
        Nu_5 = 0.559090,
        Nu_6 = 0.846406,
        Rho = par[["Rho"]],
        Epsilon = par[["Epsilon"]],
        Kappa = par[["Kappa"]],
        Gamma = par[["Gamma"]],
        Theta = 1.511,
        Omega = par[["Omega"]],
        p = 0.95,
        s_1 = 0.1,
        s_2 = 0.2,
        s_3 = 0.3,
        s_4 = 0.4,
        s_5 = 1,
        s_6 = 2,
        s_7 = 3,
        Sigma = par[["Sigma"]],
        Delta_1 = 1.58084765,
        Delta_2 = 3.50371789,
        Delta_3 = 3.50371789,
        Delta_4 = 3.50371789,
        Delta_5 = 3.50371789,
        Alpha_1 = 0.004110,
        Alpha_2 = 0.011670,
        Alpha_3 = 0.009385,
        Alpha_4 = 0.016394,
        Alpha_5 = 0.027656,
        Alpha_6 = 0.047877,
        Alpha_7 = 1.081964,
        Tau_1 = 0.003905,
        Tau_2 = 0.011087,
        Tau_3 = 0.008916,
        Tau_4 = 0.015574,
        Tau_5 = 0.026273,
        Tau_6 = 0.045482,
        Tau_7 = 1.02785,
        Mu = 0.0374,
        ART_500 = 1,
        ART_350 = 1,
        ART_200 = 1,
        Dx_unitCost = 10,
        Linkage_unitCost = 40,
        Annual_Care_unitCost = 40,
        Annual_ART_unitCost = 367
    )

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
    out <- mutate(out,DALY = (((UnDx_500 + Dx_500 + Care_500 + PreLtfu_500 + Tx_Na_500 + Ltfu_500 + UnDx_350500 + Dx_350500 + Care_350500 + PreLtfu_350500 + Tx_Na_350500 + Ltfu_350500) * 0.078) +  # >350, no ART
                              ((UnDx_250350 + Dx_250350 + Care_250350 + PreLtfu_250350 + Tx_Na_250350 + Ltfu_250350 + UnDx_200250 + Dx_200250 + Care_200250 + PreLtfu_200250 + Tx_Na_200250 + Ltfu_200250) * 0.274) +  # 200-350, no ART
                              ((UnDx_100200 + Dx_100200 + Care_100200 + PreLtfu_100200 + Tx_Na_100200 + Ltfu_100200 + UnDx_50100 + Dx_50100 + Care_50100 + PreLtfu_50100 + Tx_Na_50100 + Ltfu_50100 + UnDx_50 + Dx_50 + Care_50 + PreLtfu_50 + Tx_Na_50 + Ltfu_50) * 0.582) + # <200, no ART
                              ((Tx_A_500 + Tx_A_350500 + Tx_A_250350 + Tx_A_200250 + Tx_A_100200 + Tx_A_50100 + Tx_A_50) * 0.078))) # on ART and virally suppressed
    # PLHIV = as.double(sum(filter(out,time == 5) %>% select(N)))
    # dx = as.double(sum(filter(out,time == 5) %>% select(c(Dx_500,Dx_350500,Dx_250350,Dx_200250,Dx_100200,Dx_50100,Dx_50,Care_500,Care_350500,Care_250350,Care_200250,Care_100200,Care_50100,Care_50,PreLtfu_500,PreLtfu_350500,PreLtfu_250350,PreLtfu_200250,PreLtfu_100200,PreLtfu_50100,PreLtfu_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50,Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Ltfu_500,Ltfu_350500,Ltfu_250350,Ltfu_200250,Ltfu_100200,Ltfu_50100,Ltfu_50))))
    # tx = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50,Tx_Na_500,Tx_Na_350500,Tx_Na_250350,Tx_Na_200250,Tx_Na_100200,Tx_Na_50100,Tx_Na_50))))
    # vs = as.double(sum(filter(out,time == 5) %>% select(c(Tx_A_500,Tx_A_350500,Tx_A_250350,Tx_A_200250,Tx_A_100200,Tx_A_50100,Tx_A_50))))
    # p_dx <- dx / PLHIV
    # p_tx <- tx / dx
    # p_vs <- vs / tx
    # results <- c(p_dx,p_tx,p_vs)
    # definition <- c("% Diagnosed","% On Treatment","% Suppressed")
    # the909090 <- data.frame(definition,results)

    # # Outputs to return
    # cost <<- as.double(sum(filter(out,time == 5) %>% select(TotalCost)))
    # output <- 1/3 * sum((target - the909090$results)^2)
    # print(paste("Error =",output,"Cost =",dollar(cost)))
    return(out)
}

Start.Time <- proc.time()
theList <- list()
for(i in 1:dim(ParInput)[1]) {
    print(paste("Run",i))
    theList[[rownames(ParInput)[i]]] <- RunSimulation(ParInput[i,],1)
}
Time.Elapsed <- proc.time() - Start.Time
Time.Elapsed
object_size(theList)


Calc_Cost <- function(outFile) {
    theCost <- as.double(filter(outFile,time == 5) %>% select(TotalCost))
    return(theCost)
}

Calc_DALY <- function(outFile) {
    theDALY <- select(outFile,DALY) %>% sum()
    return(theDALY)
}

Calc_BaselineDALY <- function() {
    BaselinePar <- c(
        Rho = 0.205,
        Epsilon = 16.949,
        Kappa = 1.079,
        Gamma = 2.556,
        Sigma = 0,
        Omega = 0.033
        )
    Baseline <- RunSimulation(BaselinePar,1)
    BaselineDALY <- Calc_DALY(Baseline)
    return(BaselineDALY)
}

theBaselineDALY <- Calc_BaselineDALY()

Calc_DALYsAverted <- function(outFile,BaselineDALY) {
    theDALY <- select(outFile,DALY) %>% sum()
    return(BaselineDALY - theDALY)
}

ResultImpact <- c()
ResultCost <- c()
for(i in 1:length(theList)) {
    print(i)
    ResultImpact[i] <- Calc_DALYsAverted(theList[[i]],theBaselineDALY)
    ResultCost[i] <- Calc_Cost(theList[[i]])
}

ResultNames <- paste("p",seq(1,dim(ParInput)[1],1),sep='')

Result <- data.frame(ResultNames,ResultImpact,ResultCost)

ggplot(Result,aes(x=ResultImpact,y=ResultCost)) +
geom_point(aes(color=ResultNames)) +
theme_classic() +
theme(legend.position="none") + 
ggtitle("All 4,096 Results (2nd, November 2015)")

# Save all those files.
# dir("~/Google\ Drive/DIDE/HIVMC/WhoCascade/Model/29th-October")
# save.image("~/Google\ Drive/DIDE/HIVMC/WhoCascade/Model/29th-October/sessionData.Rdata")


