library(dygraphs) 

shinyUI(navbarPage("Twitter activity and TF-IDF",

  tabPanel("Activity",
    dygraphOutput('frequencyHists')
  ),

  tabPanel("TimeSeries",
    sidebarLayout(
      sidebarPanel(
        uiOutput("place"),
        submitButton("Update View")
      ),
      mainPanel(
        plotOutput("timeseries")
      )
    )
  ),

  tabPanel("TF-IDF",
    sidebarLayout(
      sidebarPanel(
        uiOutput("place_"),
#        conditionalPanel(condition="input.conditionedPanels==4",
          textInput("queryWord", "Find association with:", value = "", width = NULL),
          sliderInput('corr', 'Association threshold ',value = 0.1,   min = 0, max = 0.3, step = 0.001),
          submitButton("Update View")
#        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("TopWords",     verbatimTextOutput("terms"), value=3), 
          tabPanel("Associations", verbatimTextOutput("text"),  value=4),
          id="conditionedPanels"
         )
      )
    )
  )
))
