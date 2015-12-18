library(dygraphs)
library(timeSeries)
require(tm)

input <- 'rdata.zip'

if( F ){
if( !file.exists(input) || !file.exists("freq.RData") || as.double( Sys.time() - file.info(input)$mtime , units="days") > 1 ){
    print("Downloading new data")
    download.file( paste("http://cern.ch/kkotov/",input,sep=''), destfile=input )
    unlink(c("termMatrix*","freq.RData"))
    unzip(input)
}
}

load("freq.RData")

places <- c()
days   <- data.frame( place=character(), lag=numeric() )

for(file in list.files(path="./",pattern="*.RData")){

    if( length(grep("termMatrix",file)) != 0  ){

        file <- gsub(".RData",    "",file)
        file <- gsub("termMatrix","",file)

        if( length( grep("Lag",file) ) == 0 ){
            places <- append(places,file)
        } else {
            place <- gsub("(.*?)Lag(\\d+)", "\\1", file, perl=TRUE)
            lag   <- gsub("(.*?)Lag(\\d+)", "\\2", file, perl=TRUE)
            days  <- rbind(days, data.frame( place = place, lag = lag ) )
        }
    }
}

shinyServer(
  function(input, output) {

    output$place_ <- renderUI({
        selectInput("place_", "Select place", choices = places)
    })

    output$place <- renderUI({
        selectInput("place", "Select place", choices = places)
    })

    output$lag <- renderUI({
        # each place have same number of lags
        choices <- c('Yesterday'='1','Two days ago'='2','Three days ago'='3','Four days ago'='4','Five days ago'='5','Six days ago'='6','Seven days ago'='7')
        selectInput("lag", "Select time period", choices = c('Whole period'='0', choices[ days[days$place==places[1],"lag"] ]) )
    })


    output$frequencyHists <- renderDygraph({
        dygraph(freq) %>%
            dyOptions(stackedGraph = TRUE) %>%
            dyAxis("y", label = "number of tweets") %>%
            dyAxis("x", label = "time (UTC+00)") %>%
            dyRangeSelector(height = 20)
    })

    output$timeseries <- renderPlot({
        if( is.null(input$place_) ) return(NULL)
        plot( decompose( ts(freq[,input$place_],frequency=1440) ) )
    })

    output$terms <- renderPrint({
        if( is.null(input$place) || is.null(input$lag) ) return(NULL)
        else load(paste("termMatrix", input$place, if( input$lag==0 ) "" else paste("Lag",input$lag,sep=''), ".RData",sep=''))
        frequentTerms
    })

    output$text<- renderPrint({
        if( is.null(input$place) || is.null(input$queryWord) ) return(NULL)
        else load(paste("termMatrix",input$place, if( input$lag==0 ) "" else paste("Lag",input$lag,sep=''), ".RData",sep=''))
        # remove all terms that occur only twice
        dtmLite <- removeSparseTerms(dtm, 1-2.01/dtm$nrow)
        findAssocs( dtmLite, input$queryWord, input$corr ) 
    })

  }
)
