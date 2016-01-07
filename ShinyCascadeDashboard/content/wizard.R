Tab_Result_Test <- tabItem(tabName = "test",
    column(width = 8,
        fluidRow(
            tabBox(
                width = NULL,
                height = "500px",
                title = "Big Plot",
                tabPanel("Tab1", "Some shit"),
                tabPanel("Tab2", "Some more shit")
            )
        ),
        fluidRow(
            tabBox(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", 
                title = "First tabBox",
                height = "300px",
                width = 6,
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
            ),
            tabBox(
                id = "tabset2",
                title = tagList(shiny::icon("gear"), "tabBox status"),
                height = "300px",
                width = 6,
                selected = "Tab1",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2")
            )
        ),
        valueBox(90, "Diagnosed", color = "red", icon = icon("medkit", lib = "font-awesome")),
        valueBox(90, "On Treatment", color = "yellow", icon = icon("medkit", lib = "font-awesome")),
        valueBox(90, "Virally Suppressed", color = "green", icon = icon("medkit", lib = "font-awesome"))
    ),
    column(width = 4,
        wellPanel(
            sliderInput('userTest1','Unit cost of diagnosing a patient (USD):',min=0,max=100,value=10,step=1)
            ),
        wellPanel(
            sliderInput('userTest2','Unit cost of linking a patient to care (USD):',min=0,max=100,value=40,step=1)
            ),
        wellPanel(
            sliderInput('userTest3','Annual cost of keeping a patient in pre-ART care (USD):',min=0,max=100,value=40,step=1)
            ),
        wellPanel(
            sliderInput('userTest4','Annual cost of ART (USD):',min=0,max=500,value=367,step=1)
            ),
        bsButton(inputId = "testStart", label = "Start", style = "success", size = "large", icon = icon("play", lib = "font-awesome")),
        bsButton(inputId = "testStop", label = "Stop", style = "danger", size = "large", icon = icon("stop", lib = "font-awesome"))
    )
)