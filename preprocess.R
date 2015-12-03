library(dygraphs)
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

        filtered <- subset(all, PlaceName==place)

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
        # 100 most frequent terms
        frequentTerms <- findFreqTerms(dtm, quantile(slam::col_sums(dtm), probs = 1 - 100./dim(dtm)[2]))

        save( frequentTerms, dtm, corp, file = paste("termMatrix",place,".RData",sep='') )
        rm( frequentTerms, dtm, corp )
}
