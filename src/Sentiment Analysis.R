# ------------------ Lesson 1 -----------------------
setwd(paste0(getwd(), '/src'))
#Set Options
options(stringsAsFactors=FALSE) #don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')
set.seed(123)

source("./helpers.R")

# ------------ Sentiment Analysis --------------
library(stringi)
library(stringr) 
library(tm)
library(textclean)
library(dplyr)
library(sentimentr)
library(wordcloud)
library(emoji)

# read csv
ipc.df<-read.csv('./files/pse_isr_reddit_comments_min.csv', encoding = 'UTF-8') #read the csv file
head(ipc.df) #print 6 first rows
nrow(ipc.df) #number of rows

ipc.corpus<-VCorpus(VectorSource(ipc.df$self_text))

hash_sentiment_with_emojies <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                                  x = data.frame(
                                                                    words = tolower(lexicon::emojis_sentiment$id),
                                                                    polarity = lexicon::emojis_sentiment$sentiment,
                                                                    stringsAsFactors = FALSE
                                                                  )
)

replace_emoji_underscore<-function(vec){
  replace_emoji(vec,emoji_dt = hash_emoji_underscore)
}

remove_symbols <- function(text_corpus, symbols_to_remove = c("“", "”", "'", "’", "$", "%", "@", "&")) {
  clean_corpus <- gsub(paste0("[", paste0(symbols_to_remove, collapse = ""), "]"), "", text_corpus, perl = TRUE)
  
  return(clean_corpus)
}

replace_emojies_identifier <- function(txt){
  replace_emoji(txt, emoji_dt = lexicon::hash_emojis_identifier)
}

clean.corpus.snt <- function(corpus){
  corpus <- tm_map(ipc.corpus, content_transformer(remove_symbols))
  corpus <- tm_map(corpus, content_transformer(replace_emojies_identifier))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- remove_non_english(corpus) # Removes non-enlglish words
  
  return(corpus)
}

lexicon::emojis_sentiment[lexicon::emojis_sentiment$name=="face with tears of joy",]

ipc.corpus.cln.sen<-clean.corpus.snt(ipc.corpus)
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpus.cln.sen, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)


ipc.corpus.sntr<-sentiment_by(ipc.corpus.cln.sen.df$text, polarity_dt = hash_sentiment_with_emojies) #Calculate the average polarity of each comment

ipc.df$sentiemnt<-ipc.corpus.sntr$ave_sentiment
# vec <- stri_trans_general(sapply(ipc.corpus, as.character),"Latin-ASCII")
# ipc.corpus.cln.sen.df <- data.frame(text=vec, 
#                                     originalText=ipc.df$self_text,
#                                     stringsAsFactors=F)


lexicon::emojis_sentiment

hash_sentiment_with_emojies <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                                  x = data.frame(
                                                                    words = lexicon::emojis_sentiment$id,
                                                                    polarity = lexicon::emojis_sentiment$sentiment,
                                                                    stringsAsFactors = FALSE
                                                                  )
)

hash_sentiment_with_emojies[hash_sentiment_with_emojies$x == "lexiconwcaiviebiytolowkanmb"]

ipc.corpus.sntr<-sentiment_by(unlist(ipc.corpus.cln.sen.df$text), polarity_dt = hash_sentiment_with_emojies) #Calculate the average polarity of each comment



# ------------- Part 3 - Sentiment Analysis -------------

ipc.corpus.sntr.df<-data.frame(sentiment=ipc.corpus.sntr$ave_sentiment,content=ipc.df$self_text,
                               filtered_content=ipc.corpus.cln.sen.df$text)

lexicon::hash_sentiment_jockers_rinker[c("skull")]
lexicon::hash_sentiment_jockers_rinker[c("starve","hoard","shoot", "aid","food")]
lexicon::hash_sentiment_jockers_rinker[c("tears")]
lexicon::hash_sentiment_huliu[c("only")]


ipc.df$sentiment<-ipc.corpus.sntr$ave_sentiment

table(sign(ipc.df$sentiment))

ipc.pos<-ipc.corpus.sntr.df[ipc.corpus.sntr.df$sentiment > 0, "filtered_content"] # Positive comments
ipc.neg<-ipc.corpus.sntr.df[ipc.corpus.sntr.df$sentiment < 0, "filtered_content"] # Negative comments

ipc.pos.vec<-paste(ipc.pos, collapse=" ") # Concatenate all positive comments
ipc.neg.vec<-paste(ipc.neg, collapse=" ") # Concatenate all negative comments

ipc.all<-c(ipc.pos.vec,ipc.neg.vec) # Creates a vector with positive and negative comments

# Create the corpus (vector source) with two documents
ipc.all.all.corpus <- VCorpus(VectorSource(ipc.all))
ipc.all.all.corpus<-clean.corpus.all(ipc.all.all.corpus)
ipc.all.all.corpus
# Create the Term Document Matrix - each column is a document
tdm.all <- TermDocumentMatrix(ipc.all.all.corpus)
inspect(tdm.all[1:10,1:2])
tdm.all.m <- as.matrix(tdm.all) # Convert to a matrix
colnames(tdm.all.m) = c("Positive", "Negative") # Change column names
tdm.all.m[3480:3490,] # Inspect rows 3480 to 3490

display.brewer.all()
pal <- brewer.pal(9, "Purples") # Choose color palette
pal <- pal[-(1:4)] # Remove the 4 lightest colors

####construct a comparison cloud
comparison.cloud(tdm.all.m, max.words = 50, random.order=FALSE,title.size = 1, #random.order=FALSE keeps the list ordered by frequency
                 colors=brewer.pal(ncol(tdm.all.m), "Dark2")) #number of colors just like the number of different documents we compare

# Need to remove a lot of stopwords that somehow doesn't removed
commonality.cloud(tdm.all.m, max.words = 200, random.order=FALSE,colors=pal) 

