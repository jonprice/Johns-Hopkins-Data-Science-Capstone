setwd("~/src/coursera/johnhopkinsdatasciencecapstone")
  
sampleFile <- function(fileLines, percent, name) {
  subset <- sample(fileLines,   size = length(fileLines) * percent)
  
  dir.create(paste("samples" , percent, sep="/"), recursive = TRUE)
  file.create(paste("samples" , percent , name, sep="/"), recursive = TRUE) 
  con <- file(paste("samples" , percent , name, sep="/"), "w") 
  
  writeLines(subset, con)
  close(con)
}


loadFile <- function(fileName) {
  con <- file(fileName, "r") 
  lines <- readLines(con)
  close(con)
  lines
}


lines <- loadFile("final/en_US/en_US.twitter.txt")
sampleFile(lines, 0.01, "twitter.txt")

lines <- loadFile("final/en_US/en_US.blogs.txt")
sampleFile(lines, 0.01, "blogs.txt")


lines <- loadFile("final/en_US/en_US.news.txt")
sampleFile(lines, 0.01, "news.txt")







library(tm)   
library(ggplot2)   
library(reshape2)


docs <- Corpus(DirSource("samples/0.01/"))   



summary(docs)   
inspect(docs[0])





BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

TrigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
FourgramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}

dtm <- DocumentTermMatrix(docs,  control = list(removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE)) 
dtmBigram <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer,removePunctuation = TRUE,
                                                     removeNumbers = TRUE,
                                                     stopwords = TRUE) )
dtmTrigram <- DocumentTermMatrix(docs, control = list(tokenize = TrigramTokenizer,removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      stopwords = TRUE))

dtmFourgram <- DocumentTermMatrix(docs, control = list(tokenize = FourgramTokenizer,removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      stopwords = TRUE))


plotWordFreq(dtm)
plotWordFreq(dtmBigram)
plotWordFreq(dtmTrigram)
plotWordFreq(dtmFourgram)

plotWordFreq <- function(dtm){
  df <- as.data.frame(as.matrix(dtm))
  # and transpose for plotting
  df <- data.frame(t(df))
  
  newdf <- df[order(-df$blogs.txt),] 
  newdf <- newdf[1:20,]
  
  newdf[ "word" ] <- rownames(newdf)
  newdfLong <- melt( newdf, id.vars="word",  variable_name="doc" )
  
  p <- ggplot(newdfLong, aes(x = word,  y = value, fill=doc))    
  p <- p + geom_bar(stat="identity") 
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p   
}


getPercentsRows <- function(dtm) {
  df <- as.data.frame(as.matrix(dtm))
  # and transpose for plotting
  df <- data.frame(t(df))
  df$totals =df$blogs.txt + df$news.txt + df$twitter.txt
  df <- df[order(-df$totals),]
  df$cumsum <- cumsum(df$totals)
  
  results <- c(getRow(df, 0.1), 
            getRow(df, 0.2),
            getRow(df, 0.3),
            getRow(df, 0.4),
            getRow(df, 0.5),
            getRow(df, 0.6),
            getRow(df, 0.7),
            getRow(df, 0.8),
            getRow(df, 0.9),
            getRow(df, 1.0))
   return (results)
}

getRow <- function(df, percent) {
  sumTotal <- sum(df$totals)
  cutoff <-  sumTotal * percent

  for (i in 1:nrow(df)){
    if (df$cumsum[i] >= cutoff){
      return (i)
    }
  }
}
getPercentsRows(dtmFourgram)
getRow(df, 0.1)

head(df)



dtmBigram <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer,removePunctuation = TRUE,
                                                     removeNumbers = TRUE,
                                                     stopwords = TRUE) )

getCoverage(dtmBigram, 0.5)

