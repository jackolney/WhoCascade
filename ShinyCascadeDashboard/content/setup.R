CountryList <- c(
    "Brazil",
    "Cambodia",
    "Cameroon",
    "China",
    "Cote d'Ivoire",
    "DRC",
    "Ethiopia",
    "Haiti",
    "India",
    "Indonesia",
    "Jamaica",
    "Kenya",
    "Malawi",
    "Mozambique",
    "Myanmar",
    'Nigeria',
    "Pakistan",
    "Philippines",
    "South Africa",
    "South Sudan",
    "Tanzania",
    "Thailand",
    "Uganda",
    "Ukraine",
    "Vietnam",
    "Zambia",
    "Zimbabwe"
    )

Tab_Setup <- tabItem(tabName = "setup",
    column(width = 8,
        box(width = NULL,
            status = "primary",
            h1("Model Setup"),
            shinyjs::useShinyjs(),
            id = "setup-panel",
            helpText("Select country and fill in boxes to specify the initial values of the model. 
                The boxes correlate with indicators in the Consolidated Information Guidelines (shown below), with numbers in brackets corresponding to numbers on the indicator figure."),
            img(src="WHOGuidelinesCascade.png", height = '100%', width = '100%'),
            br(), br(),
            wellPanel(
                selectInput("userCountry","Country:",CountryList,selected="Brazil")
                ),
            wellPanel(
                h4("ART Initiation Threshold"),
                helpText("Please check the box corresponding to the correct treatment threshold. Boxes are reactive and nearby boxes will adjust to the selection made. Please note that unchecking all boxes will result in ART being witheld for all individuals."),
                checkboxInput("userART_All","Immediate ART",value=TRUE),
                checkboxInput("userART_500","CD4 <500",value=TRUE),
                checkboxInput("userART_350","CD4 <350",value=TRUE),
                checkboxInput("userART_200","CD4 <200",value=TRUE)
                ),
            wellPanel(
                numericInput("userPLHIV","Number of PLHIV (1):",1e+6,min=0)
                ),
            wellPanel(
                numericInput("userDx","Number of PLHIV who have been diagnosed (4):",0,min=0)
                ),
            wellPanel(
                numericInput("userCare","Number of PLHIV in HIV care (including ART) (5):",0,min=0)
                ),
            wellPanel(
                numericInput("userTx","Number of PLHIV in HIV care and on ART (6):",0,min=0)
                ),
            wellPanel(
                numericInput("userVs","Number of PLHIV in HIV care, on ART and virally suppressed (8):",0,min=0)
                ),
            wellPanel(
                numericInput("userLtfu","Number of PLHIV who dropped out of ART care:",0,min=0)
                ),
            em(h5("If this value is known please enter it, otherwise leave it blank:")),
            wellPanel(
                numericInput("userRetArt12mths","Percentage of PLHIV retained and surviving on ART 12 months after initiation (7):",0,min=0,max=1,step=0.01)
                )
        )
    ),
    column(width = 4,
        box(width = NULL,
            status = "warning",
            solidHeader = TRUE,
            title = "Quick Start",
            h4("Help Panel"),
            helpText("Please fill in all boxes with relevant data, then hit 'SAVE' and wait for the confirmation below. 
                Hit 'RESET' to reset all values to zero, and hit 'DEMO' for a random set of values to be generated. 
                Unchecking the 'HIV incidence' checkbox prevents any new infections occurring in the model."),
            checkboxInput("incidenceInput","HIV Incidence",value=TRUE),
            bsButton("saveInput",label="SAVE",style="success"),
            p(" "),
            bsButton("resetInput",label="RESET",style="danger"),
            p(" "),
            bsButton("demoInput",label="DEMO",style="primary"),
            bsTooltip(id = "demoInput", title = "Populate model with best estimates from Kenya.", placement = "left", trigger = "hover"),
            p(" "),
            helpText("Console output:"),
            textOutput('saveText'),
            textOutput('warningText'),
            textOutput('warningCD4Text')
        )
    )
)

    # tabPanel("Setup",
    #     # h1("Model Setup"),
    #     sidebarLayout(position="right",
    #         sidebarPanel(
    #             # h4("Help Panel"),
    #             # helpText("Please fill in all boxes with relevant data, then hit 'SAVE' and wait for the confirmation below. 
    #             #     Hit 'RESET' to reset all values to zero, and hit 'DEMO' for a random set of values to be generated. 
    #             #     Unchecking the 'HIV incidence' checkbox prevents any new infections occurring in the model."),
    #             # checkboxInput("incidenceInput","HIV Incidence",value=TRUE),
    #             # bsButton("saveInput",label="SAVE",style="success"),
    #             # p(" "),
    #             # bsButton("resetInput",label="RESET",style="danger"),
    #             # p(" "),
    #             # bsButton("demoInput",label="DEMO",style="primary"),
    #             # bsTooltip(id = "demoInput", title = "Populate model with best estimates from Kenya.", placement = "left", trigger = "hover"),
    #             # p(" "),
    #             # helpText("Console output:"),
    #             # textOutput('saveText'),
    #             # textOutput('warningText'),
    #             # textOutput('warningCD4Text')
    #             ),
    #         mainPanel(
    #             # shinyjs::useShinyjs(),
    #             # id = "setup-panel",
    #             # helpText("Select country and fill in boxes to specify the initial values of the model. 
    #             #     The boxes correlate with indicators in the Consolidated Information Guidelines (shown below), with numbers in brackets corresponding to numbers on the indicator figure."),
    #             # img(src="WHOGuidelinesCascade.png", height = '100%', width = '100%'),
    #             # br(), br(),
    #             # wellPanel(
    #             #     selectInput("userCountry","Country:",CountryList,selected="Brazil")
    #             #     ),
    #             # wellPanel(
    #             #     h4("ART Initiation Threshold"),
    #             #     helpText("Please check the box corresponding to the correct treatment threshold. Boxes are reactive and nearby boxes will adjust to the selection made. Please note that unchecking all boxes will result in ART being witheld for all individuals."),
    #             #     checkboxInput("userART_All","Immediate ART",value=TRUE),
    #             #     checkboxInput("userART_500","CD4 <500",value=TRUE),
    #             #     checkboxInput("userART_350","CD4 <350",value=TRUE),
    #             #     checkboxInput("userART_200","CD4 <200",value=TRUE)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userPLHIV","Number of PLHIV (1):",1e+6,min=0)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userDx","Number of PLHIV who have been diagnosed (4):",0,min=0)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userCare","Number of PLHIV in HIV care (including ART) (5):",0,min=0)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userTx","Number of PLHIV in HIV care and on ART (6):",0,min=0)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userVs","Number of PLHIV in HIV care, on ART and virally suppressed (8):",0,min=0)
    #             #     ),
    #             # wellPanel(
    #             #     numericInput("userLtfu","Number of PLHIV who dropped out of ART care:",0,min=0)
    #             #     ),
    #             # em(h5("If this value is known please enter it, otherwise leave it blank:")),
    #             # wellPanel(
    #             #     numericInput("userRetArt12mths","Percentage of PLHIV retained and surviving on ART 12 months after initiation (7):",0,min=0,max=1,step=0.01)
    #             #     )
    #             )
    #         )
    #     ),