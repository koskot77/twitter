library(UsingR)
library(ggplot2)
library(rCharts)

library(dygraphs)
library(timeSeries)
require(tm)

input <- "freq.RData"

require("R.utils")
if( F ){
if( !file.exists(input) || as.double( Sys.time() - file.info(input)$mtime , units="days") > 2 ){
    print("Downloading new data")
    inputGz <- paste(input,".gz",sep='') 
    download.file( paste("http://cern.ch/kkotov/",inputGz,sep=''), destfile=inputGz )
    unlink(input)
    gunzip(inputGz)
}
}

load(input)

places <- c()

for(file in list.files(path="./",pattern="*.RData")){
    if( length(grep("termMatrix",file)) != 0  ){
#        load(file)
        file   <- gsub(".RData",    "",file)
        file   <- gsub("termMatrix","",file)
        places <- append(places,file)
    }
}

shinyServer(
  function(input, output) {

#    output$PlaceName <- renderUI({
#        selectInput("PlaceName", "Select place", choices = places[ seq(length(places),1) ])
#    })

#    output$FrequentWord <- renderUI({
#        if( is.null(input$place) ) frequentTerms <- c()
#        else load(paste("termMatrix",input$place,".RData",sep=''))
#        selectInput("FrequentWord",  "Select frequent word", choices = frequentTerms)
#    })

    output$frequencyHists <- renderDygraph({
        dygraph(freq) %>%
            dyOptions(stackedGraph = TRUE) %>%
            dyAxis("y", label = "number of tweets") %>%
            dyAxis("x", label = "time (UTC+0?)") %>%
            dyRangeSelector(height = 20)
    })

    output$timeseries <- renderPlot({
        if( is.null(input$place) ) return(NULL)
        plot( decompose( ts(freq[,input$place],frequency=1440) ) )
    })

    output$terms <- renderPrint({
        if( is.null(input$place) ) return(NULL)
        else load(paste("termMatrix",input$place,".RData",sep=''))
        frequentTerms
    })

    output$text<- renderPrint({
        if( is.null(input$place) || is.null(input$queryWord) ) return(NULL)
        else load(paste("termMatrix",input$place,".RData",sep=''))
        # remove all terms that occur only once
        dtmLite <- removeSparseTerms(dtm, 1-1.01/dtm$nrow)
        findAssocs( dtmLite, input$queryWord, input$corr ) 
    })

  }
)
