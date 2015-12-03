library(rCharts) 
library(dygraphs) 
require(rjson)

shinyUI(fluidPage(
  titlePanel("Twitter activity and TF-IDF"),

  sidebarLayout(
    sidebarPanel(
      selectInput("place", "Location:", choices = c(
"California"="California",
"New York"="NY",
"London"="London",
"Pairs"="Pairs",
"Moskow"="Moskow",
"St.Peterburg"="St.Peterburg",
"Kiev"="Kiev",
"Ukraine"="Ukraine",
"Ekatirinburg"="Ekatirinburg",
"Novosibirsk"="Novosibirsk",
"Vladivostok"="Vladivostok"
)),
#      uiOutput("PlaceName"),
      textInput("queryWord", "Find association with:", value = "", width = NULL),
      sliderInput('corr', 'Association threshold ',value = 0.1,   min = 0, max = 1, step = 0.001,)
    ),
    mainPanel(
      p('Stacked activity, sampled from various places:'),
      dygraphOutput('frequencyHists'),
#      p("Standard time series decomposition:"),
#      plotOutput('timeseries'),
      p("Most frequent words:"),
      verbatimTextOutput("terms"),
      p("Associations:"),
      verbatimTextOutput("text")
    )
  )
))
