library(shiny)
library(ggplot2)
library(dplyr)
library(deSolve)

function(input, output) {
  
  Parameters <- reactive({c(
        Nu_1 = 0.2139008,
        Nu_2 = 0.3379898,
        Nu_3 = 0.2744363,
        Rho = 0.5,
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
        Beta = 0.0275837
    )})

  Initial <- reactive({c(
        UnDx_500 = 1e+4 * 0.2 * 0.58,
        UnDx_350500 = 1e+4 * 0.2 * 0.23,
        UnDx_200350 = 1e+4 * 0.2 * 0.16,
        UnDx_200 = 1e+4 * 0.2 * 0.03,

        Dx_500 = 1e+4 * 0.43 * 0.58,
        Dx_350500 = 1e+4 * 0.43 * 0.23,
        Dx_200350 = 1e+4 * 0.43 * 0.16,
        Dx_200 = 1e+4 * 0.43 * 0.03,

        Tx_500 = 1e+4 * 0.2 * 0.58,
        Tx_350500 = 1e+4 * 0.2 * 0.23,
        Tx_200350 = 1e+4 * 0.2 * 0.16,
        Tx_200 = 1e+4 * 0.2 * 0.03,

        Vs_500 = 1e+4 * 0.17 * 0.58,
        Vs_350500 = 1e+4 * 0.17 * 0.23,
        Vs_200350 = 1e+4 * 0.17 * 0.16,
        Vs_200 = 1e+4 * 0.17 * 0.03,

        Ltfu_500 = 0,
        Ltfu_350500 = 0,
        Ltfu_200350 = 0,
        Ltfu_200 = 0,

        NewInf = 0,

        HivMortality = 0,

        NaturalMortality = 0
    )})

  output$plot <- renderPlot({

    ComplexCascade <- function(t, y, parms) {

        dUnDx_500 <- 0.58 * (parms[19] * (((y[1] + y[5] + y[9] + y[17]) * 1.35) + ((y[2] + y[6] + y[10] + y[18]) * 1) + ((y[3] + y[7] + y[11] + y[19]) * 1.64) + ((y[4] + y[8] + y[12] + y[20]) * 5.17) + ((y[13] + y[14] + y[15] + y[16]) * 0.1))) - (parms[4] + parms[1] + parms[10] + parms[18]) * y[1]
        dUnDx_350500 <- 0.23 * (parms[19] * (((y[1] + y[5] + y[9] + y[17]) * 1.35) + ((y[2] + y[6] + y[10] + y[18]) * 1) + ((y[3] + y[7] + y[11] + y[19]) * 1.64) + ((y[4] + y[8] + y[12] + y[20]) * 5.17) + ((y[13] + y[14] + y[15] + y[16]) * 0.1))) + parms[1] * y[1] - (parms[4] + parms[2] + parms[11] + parms[18]) * y[2]
        dUnDx_200350 <- 0.16 * (parms[19] * (((y[1] + y[5] + y[9] + y[17]) * 1.35) + ((y[2] + y[6] + y[10] + y[18]) * 1) + ((y[3] + y[7] + y[11] + y[19]) * 1.64) + ((y[4] + y[8] + y[12] + y[20]) * 5.17) + ((y[13] + y[14] + y[15] + y[16]) * 0.1))) + parms[2] * y[2] - (parms[4] + parms[3] + parms[12] + parms[18]) * y[3]
        dUnDx_200 <- 0.03 * (parms[19] * (((y[1] + y[5] + y[9] + y[17]) * 1.35) + ((y[2] + y[6] + y[10] + y[18]) * 1) + ((y[3] + y[7] + y[11] + y[19]) * 1.64) + ((y[4] + y[8] + y[12] + y[20]) * 5.17) + ((y[13] + y[14] + y[15] + y[16]) * 0.1))) + parms[3] * y[3] - (parms[4] + parms[13] + parms[18]) * y[4]

        dDx_500 <- + parms[4] * y[1] - (parms[5] + parms[1] + parms[10] + parms[18]) * y[5]
        dDx_350500 <- + parms[4] * y[2] + parms[1] * y[5] - (parms[5] + parms[2] + parms[11] + parms[18]) * y[6]
        dDx_200350 <- + parms[4] * y[3] + parms[2] * y[6] - (parms[5] + parms[3] + parms[12] + parms[18]) * y[7]
        dDx_200 <- + parms[4] * y[4] + parms[3] * y[7] - (parms[5] + parms[13] + parms[18]) * y[8]

        dTx_500 <- + parms[5] * y[5] - ((parms[7] / 2) + parms[6] + parms[10] + parms[18]) * y[9]
        dTx_350500 <- + parms[5] * y[6] + parms[8] * y[11] - ((parms[7] / 2) + parms[6] + parms[11] + parms[18]) * y[10]
        dTx_200350 <- + parms[5] * y[7] + parms[9] * y[12] - ((parms[7] / 2) + parms[8] + parms[6] + parms[12] + parms[18]) * y[11]
        dTx_200 <- + parms[5] * y[8] - ((parms[7] / 2) + parms[9] + parms[6] + parms[13] + parms[18]) * y[12]

        dVs_500 <- + parms[6] * y[9] - ((parms[7] / 2) + parms[14] + parms[18]) * y[13]
        dVs_350500 <- + parms[6] * y[10] + parms[8] * y[15] - ((parms[7] / 2) + parms[15] + parms[18]) * y[14]
        dVs_200350 <- + parms[6] * y[11] + parms[9] * y[16] - ((parms[7] / 2) + parms[8] + parms[16] + parms[18]) * y[15]
        dVs_200 <- + parms[6] * y[12] - ((parms[7] / 2) + parms[9] + parms[17] + parms[18]) * y[16]

        dLtfu_500 <- + (parms[7] / 2) * (y[9] + y[13]) - (parms[1] + parms[10] + parms[18]) * y[17]
        dLtfu_350500 <- + (parms[7] / 2) * (y[10] + y[14]) + parms[1] * y[17] - (parms[2] + parms[11] + parms[18]) * y[18]
        dLtfu_200350 <- + (parms[7] / 2) * (y[11] + y[15]) + parms[2] * y[18] - (parms[3] + parms[12] + parms[18]) * y[19]
        dLtfu_200 <- + (parms[7] / 2) * (y[12] + y[16]) + parms[3] * y[19] - (parms[13] + parms[18]) * y[20]

        dNewInf <- parms[19] * (((y[1] + y[5] + y[9] + y[17]) * 1.35) + ((y[2] + y[6] + y[10] + y[18]) * 1) + ((y[3] + y[7] + y[11] + y[19]) * 1.64) + ((y[4] + y[8] + y[12] + y[20]) * 5.17) + ((y[13] + y[14] + y[15] + y[16]) * 0.1))

        dHivMortality <- parms[10] * (y[1] + y[5] + y[9] + y[17]) + parms[11] * (y[2] + y[6] + y[10] + y[18]) + parms[12] * (y[3] + y[7] + y[11] + y[19]) + parms[13] * (y[4] + y[8] + y[12] + y[20]) + parms[14] * y[13] + parms[15] * y[14] + parms[16] * y[15] + parms[17] * y[16]

        dNaturalMortality <- parms[18] * (y[1] + y[2] + y[3] + y[4] + y[5] + y[6] + y[7] + y[8] + y[9] + y[10] + y[11] + y[12] + y[13] + y[14] + y[15] + y[16] + y[17] + y[18] + y[19] + y[20])

        list(c(
            dUnDx_500,
            dUnDx_350500,
            dUnDx_200350,
            dUnDx_200,
            dDx_500,
            dDx_350500,
            dDx_200350,
            dDx_200,
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

    Time <- seq(0,5,0.02)

    out <- data.frame(ode(times=Time, y=Initial(), func=ComplexCascade, parms=Parameters()))

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

    p <- ggplot(out, aes_string(x="time",y=input$y)) + geom_line()

    # aes_string was used before, I presume to get a string from input$something
    # ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_line()

    # if (input$color != 'None')
    #   p <- p + aes_string(color=input$color)

    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .')
    #   p <- p + facet_grid(facets)

    # if (input$jitter)
    #   p <- p + geom_jitter()
    # if (input$smooth)
    #   p <- p + geom_smooth()

    print(p)

  }, height=700)

}