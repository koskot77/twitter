library(dygraphs)
library(timeSeries)
require(tm)
require(parallel)

update <- T

processList <- c()

if( update ){
# Unfortunately, I don't have a shiny server on my own so I cannot update data sets localy as it goes; I have to dowload big chunks of data every time
if( !file.exists("freq.RData") || as.double( Sys.time() - file.info("freq.RData")$mtime , units="days") > 1 ){

    print("Downloading new freq.RData")
    unlink(c("termMatrix*","freq.RData"))
    download.file("http://cern.ch/kkotov/twitter/freq.bin", destfile='freq.RData' )

    load("freq.RData")

    # download the global period term matrices sequentially, wait until done
    for( place in colnames(freq) ){
        download.file( paste("http://cern.ch/kkotov/twitter/","termMatrix",place,".bin",sep=''), destfile=paste("termMatrix",place,".RData",sep='') )
    }

    nPeriods <- round( as.numeric( difftime(end(freq), start(freq), units='day') ) )

    fileList <- c()

    # download other term matricies in background
    for( place in colnames(freq) ){
        for( day in 1:nPeriods){

            file <- paste("termMatrix",place,"Lag",day,sep='')

            p <- parallel:::mcfork()
            if( inherits(p, "masterProcess") ){
                download.file( paste("http://cern.ch/kkotov/twitter/",file,".bin",sep=''), destfile=paste(file,".RData",sep='') )
                parallel:::mcexit(,paste(file," done (",Sys.getpid(),")",sep=''))
            }
            processList <- append(processList,list(p))
            fileList    <- append(fileList,file)

        }
    }
    names(processList) <- fileList
} else {
    load("freq.RData")
    nPeriods <- round( as.numeric( difftime(end(freq), start(freq), units='day') ) )
}
}

lookupPlaces <- function(){

  places <- c()

  for(file in list.files(path="./",pattern="*.RData")){

    if( length(grep("termMatrix",file)) != 0  ){

        file <- gsub(".RData",    "",file)
        file <- gsub("termMatrix","",file)

        if( length( grep("Lag",file) ) == 0 ){
            places <- append(places,file)
        }
    }

  }
  return(places)
}

lookupDays <- function(){

  days   <- data.frame( place=character(), lag=numeric() )

  for(file in list.files(path="./",pattern="*.RData")){

    if( length(grep("termMatrix",file)) != 0  ){

        file <- gsub(".RData",    "",file)
        file <- gsub("termMatrix","",file)

        if( length( grep("Lag",file) ) != 0 ){
            place <- gsub("(.*?)Lag(\\d+)", "\\1", file, perl=TRUE)
            lag   <- gsub("(.*?)Lag(\\d+)", "\\2", file, perl=TRUE)
            days  <- rbind(days, data.frame( place = place, lag = lag ) )
        }
    }

  }
  return(days)
}



shinyServer(
  function(input, output) {

    output$place_ <- renderUI({
        places <- lookupPlaces()
        selectInput("place_", "Select place", choices = places)
    })

    output$place <- renderUI({
        places <- lookupPlaces()
        selectInput("place", "Select place", choices = places)
    })

    output$lag <- renderUI({
        # each place have same number of lags
        choices <- c('One day before'='1','Two days before'='2','Three days before'='3','Four days before'='4','Five days before'='5','Six days before'='6','Seven days before'='7')
        selectInput("lag", "Select time period", choices = c('Whole period'='0', choices[1:nPeriods] )) # [ days[days$place==places[1],"lag"] ]) )
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
        else {
            if( input$lag==0 ){
                load( paste("termMatrix",input$place,".RData",sep='') ) 
            } else { # chech if downloading is still in progress
                file <- paste("termMatrix",input$place, if( input$lag==0 ) "" else paste("Lag",input$lag,sep=''),sep='')

                if( file %in% names(processList) ){

                    if( parallel:::selectChildren( processList[[file]] ) == T ){
                        return( paste("Downloading ",file,"has not yet finished") )
                    } else {
                        unserialize( parallel:::readChild( unlist(processList[[file]]) ) )
                        processList <- processList[ -which( attr(processList,"names") == file ) ]
                    }
                }
                load( paste(file,".RData",sep='') )
            }
        }
        output <- "Terms (with their relative document frequency) from the top TF-IDF list: "
        output <- append(output, frequentTerms)
        output
    })

    output$text<- renderPrint({
        if( is.null(input$place) || is.null(input$queryWord) || is.null(input$lag) ) return(NULL)
        else {
            if( input$lag==0 ){
                load( paste("termMatrix",input$place,".RData",sep='') ) 
            } else { # chech if downloading is still in progress
                file <- paste("termMatrix",input$place, if( input$lag==0 ) "" else paste("Lag",input$lag,sep=''),sep='')

                if( file %in% names(fileList) ){

                    if( parallel:::selectChildren( processList[[file]] ) == T ){
                        return( paste("Downloading ",file,"has not yet finished") )
                    } else {
                        unserialize( parallel:::readChild( unlist(processList[[file]]) ) )
                        processList <- processList[ -which( attr(processList,"names") == file ) ]
                    }
                }
                load( paste(file,".RData",sep='') )
            }
        }

        queryWord <- tolower(input$queryWord)

        output <- paste("Term '",queryWord,"' is not found in the corpus",sep='')

        if( queryWord %in% colnames(dtm) ){
            output <- paste("Term '",queryWord,"' is found ",slam::col_sums( dtm[,queryWord]>0 )," times in the corpus, associations:",sep='')
            # remove all terms that occur only once or twice so at to speed up the findAssocs function
            dtmLite <- removeSparseTerms(dtm, 1-2.01/dtm$nrow)
            output <- append(output, findAssocs( dtmLite, queryWord, input$corr ) )
        }

        output
      })

  }
)
