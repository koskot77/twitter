library(timeSeries)
require(tm)

all            <- read.csv(file="all.txt", header=T, sep=',', encoding = "UTF-8", stringsAsFactors = F)
all$timeDate   <- timeDate(   all$DateTime, format="%Y-%m-%d %H:%M:%S", zone = "GMT", FinCenter = "GMT")
by             <- timeSequence(from = start(all$timeDate), to = end(all$timeDate), by = "min")
# Run One-HOT encoding while converting data to the timeSeries
freq           <- aggregate( timeSeries( model.matrix(~PlaceName-1,all), all$timeDate), by, sum)
colnames(freq) <- gsub("PlaceName","",colnames(freq))

save(freq, file = "freq.RData")

for(place in unique(all$PlaceName)){

    print(place)

    filteredForPlace <- subset(all, PlaceName==place)

    # define global time period
    periods <- data.frame( start(freq), end(freq) )
    #  and partition this time period by days from the current moment and going back
    days <- seq( from = end(freq), to = start(freq), by = -24*60*60 )
    periods <- rbind( periods,
                      data.frame( start = days[1:length(days)-1],
                                  end   = days[2:length(days)  ]
                                )
                    )
    colnames(periods) <- c('start','end')
    # construct global and daily term matricies
    for( p in as.integer(rownames(periods)) ){

        if( p == 1 ){
            print("  Global time period")
            # do nothing here, dataset is already given for a global time period
            filtered <- filteredForPlace
        } else {
            filtered <- subset(filteredForPlace, timeDate > timeDate(periods[p,2]) & timeDate <= timeDate(periods[p,1]) )
            print( paste("  Lag",p-1) )
        }

        # limit the size of input data so as not to run out of RAM
        if( dim(filtered)[1] > 100000 ){
            filtered <- filtered[ sample(dim(filtered)[1], 100000, replace = FALSE), ]
        }

        filtered$Text <- sub("@\\w+","",      filtered$Text,perl = TRUE)
        filtered$Text <- sub("(:\\w+:)+","",  filtered$Text,perl = TRUE)
        filtered$Text <- sub("http[^\\s]*","",filtered$Text,perl = TRUE)

        lang <- "eng"

        if( place %in% c('Moscow','St.Peterburg','Kiev','Ukraine','Novosibirsk','Ekatirinburg','Vladivostok') ){
            lang <- "rus"
        }
        if( place %in% c('Paris') ){
            lang <- "fre"
        }

        corp <- Corpus(VectorSource(filtered$Text),readerControl = list(language = lang))
        corp <- tm_map(corp, stripWhitespace)
        corp <- tm_map(corp, removePunctuation)
        corp <- tm_map(corp, content_transformer(tolower))
        corp <- tm_map(corp, removeWords, stopwords("russian"))
        corp <- tm_map(corp, removeWords, stopwords("french"))
        corp <- tm_map(corp, removeWords, stopwords("english"))

        dtm <- DocumentTermMatrix(corp, control = list( weighting = function(x) weightTfIdf(x,normalize = FALSE), stopwords = TRUE) )
        # remove all terms that occur only once or twice
        dtmLite <- removeSparseTerms(dtm, 1-2.01/dtm$nrow)

        # 100 most frequent (well, actually IF-IDF relevant) terms and their document frequencies
        frequentTerms <- NULL
        if( dim(dtm)[2]>100 ){ # require at least 100 words
             # frequent terms
             frequentTerms <- findFreqTerms(dtm, quantile(slam::col_sums(dtm), probs = 1 - 100./dim(dtm)[2]))
             # and their document frequencies
             frequentTerms <- sort( slam::col_sums( dtm[,frequentTerms]>0 )/dim(dtm)[1], decreasing = T )
        }

        if( p == 1 ){ # global period
#            save( frequentTerms, dtm, dtmLite, corp, file = paste("termMatrix",place,".RData",sep='') )
            save( frequentTerms, dtm, dtmLite, file = paste("termMatrix",place,".RData",sep='') )
        } else { # daily
#            save( frequentTerms, dtm, dtmLite, corp, file = paste("termMatrix",place,"Lag",p-1,".RData",sep='') )
            save( frequentTerms, dtm, dtmLite, file = paste("termMatrix",place,"Lag",p-1,".RData",sep='') )
        }
        rm( frequentTerms, dtm, dtmLite, corp )
    }
}
