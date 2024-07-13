custom.stopwords <- c(stopwords("english"),'&gt;',"like","can","make","just","say","one","will",
                      "think","know","also","get", "want", "see","use","take","happen", "way",
                      "give","try","now","thing", "comment", "year","say", "GMT","“","’"
                      ,"they", "are", "into","the", "have", "question", "coldplay",
                      "point","base","rule","bot","post","allow",
                      "automatically", "gt;") #Words to remove

# An helper function to print corpus rows
print.corpus <- function(corpus, start=1, end=1){
  for(i in start:end){
    print(corpus[[i]][1])
  }
}

# An helper function to remove a given pattern from a text
removePattern <- function(x, pattern) gsub(x, pattern = pattern, replacement = "")

urlPattern <- "http\\S*" # URL pattern

removeURL <- function (x) removePattern(x, urlPattern) #Removes all URLs starts with http

# An helper function to clear corpus data
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus,content_transformer(replace_emoji))  
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

clean.corpus.snt <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- remove_non_english(corpus) # Removes non-enlglish words
  
  return(corpus)
}

#'@description Vector clean function. Uses the `clean.vec` clean function defined in the beginning
clean.corpus.all<-function(corpus){
  corpus<-clean.corpus(corpus)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}