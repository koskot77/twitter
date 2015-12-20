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

nPeriods <- round( as.numeric( difftime(end(freq), start(freq), units='day') ) )
places   <- c()
days     <- data.frame( place=character(), lag=numeric() )

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
        choices <- c('One day before'='1','Two days before'='2','Three days before'='3','Four days before'='4','Five days before'='5','Six days before'='6','Seven days before'='7')
        selectInput("lag", "Select time period", choices = c('Whole period'='0', choices[1:nPeriods]) )
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
        output <- "Terms (with their relative document frequency) from the top TF-IDF list: "
        output <- append(output, frequentTerms)
        return(output)
    })

    output$text<- renderPrint({
        if( is.null(input$place) || is.null(input$queryWord) ) return(NULL)
        else load(paste("termMatrix",input$place, if( input$lag==0 ) "" else paste("Lag",input$lag,sep=''), ".RData",sep=''))

        queryWord <- tolower(input$queryWord)

        output <- paste("Term '",queryWord,"' is not found in the corpus",sep='')

        if( queryWord %in% colnames(dtm) ){
            output <- paste("Term '",queryWord,"' is found ",slam::col_sums( dtm[,queryWord]>0 )," times in the corpus, associations:",sep='')
            # remove all terms that occur only once or twice so at to speed up the findAssocs function
            dtmLite <- removeSparseTerms(dtm, 1-2.01/dtm$nrow)
            output <- append(output, findAssocs( dtmLite, queryWord, input$corr ) )
        }

        return(output)
    })

  }
)

