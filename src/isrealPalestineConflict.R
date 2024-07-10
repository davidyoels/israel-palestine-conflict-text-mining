wd <- paste0(getwd(),"/src")
setwd(wd)

# load libraries
library(tm)
library(stringi)
library(textstem)
library(textclean)
library(dplyr)
library(stringr)
library(sentimentr)

source('./helpers.R')

# ------------- Part 1 - Load File to DataFrame -------------

options(stringsAsFactors=FALSE) #Don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')

ipc <- read.csv('files/pse_isr_reddit_comments_min.csv', encoding = 'UTF-8') #Read Sentiment180_2000 csv file.
ipc.df <- data.frame(doc_id=seq(1:nrow(ipc)),text=ipc$self_text) #Convert "text" column to data frame 

ipc.df[1,]

replace_emoji(ipc.df[23,]$text)

corpus <- VCorpus(DataframeSource(ipc.df)) #Creates a corpus with the data frame created before

custom.stopwords <- c(stopwords("english"),'&gt;') #Words to remove

# ------------- Part 2 - Clean Data -------------

corpus.cleaned <- clean.corpus(corpus) #Create a cleaned corpus using clean.corpus helper function

# corpus.cleaned <- tm_map(corpus, replace_emoji)

print.corpus(corpus.cleaned, 1,1)
print.corpus(corpus.cleaned, 23,23)

hash_sentiment_with_emojies <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                                  x = data.frame(
                                                                    words = tolower(lexicon::emojis_sentiment$name),
                                                                    polarity = lexicon::emojis_sentiment$sentiment,
                                                                    stringsAsFactors = FALSE
                                                                  )
)

replace_emoji("😂")

emojis_sentiment[lexicon::emojis_sentiment$name=="face with tears of joy",]

hash_sentiment_with_emojies[hash_sentiment_with_emojies$x == "face with tears of joy"]

corpus.cleaned.sen<-clean.corpus.snt(corpus)
corpus_contents <- lapply(corpus.cleaned.sen, as.character)
corpus.cleaned.sntr<-sentiment_by(unlist(corpus_contents), polarity_dt = hash_sentiment_with_emojies) #Calculate the average polarity of each comment

# ------------- Part 3 - Sentiment Analysis -------------

corpus.df.sntr<-data.frame(sentiment=corpus.cleaned.sntr$ave_sentiment,content=ipc$self_text,
                           filtered_content=unlist(corpus_contents))

lexicon::hash_sentiment_jockers_rinker[c("skull")]
lexicon::hash_sentiment_jockers_rinker[c("starve","hoard","shoot", "aid","food")]
lexicon::hash_sentiment_jockers_rinker[c("tears")]
lexicon::hash_sentiment_huliu[c("only")]

ipc$sentiment<-corpus.cleaned.sntr$ave_sentiment
library(emoji)

ipc.pos<-ipc[ipc$sentiment > 0, "self_text"] # Positive comments
ipc.neg<-ipc[ipc$sentiment < 0, "self_text"] # Negative comments

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

set.seed(1)
####construct a comparison cloud
comparison.cloud(tdm.all.m, max.words = 50, random.order=FALSE,title.size = 1, #random.order=FALSE keeps the list ordered by frequency
                 colors=brewer.pal(ncol(tdm.all.m), "Dark2")) #number of colors just like the number of different documents we compare


#------------------ Part 4 - Data Visualization ------------------
library(qdap)
library(ggplot2)
library(ggthemes) 

#create a Term Document Matrix
tdm<-TermDocumentMatrix(corpus.cleaned)

inspect(tdm[1:10, 1:5]) #inspect the result, 10 first terms, 5 documents

#convert to a matrix
tdm.reddit.m<-as.matrix(tdm) 

#summarize the rows (number of appearances of each term)
term.freq<-rowSums(tdm.reddit.m) 
term.freq[1:10] # term frequency of the first 10 terms

#create a dataframe with two columns, word and frequency
freq.df<-data.frame(word=names(term.freq),frequency=term.freq) 

#sort by frequency
freq.df<-freq.df[order(freq.df[,2], decreasing=T),] 
head(freq.df,10)

############### create a word frequency bar plot
#convert the unique words from a string to a factor with unique levels
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

#defining the levels will keep the original order
ggplot(freq.df[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity", fill='darkred') + 
  coord_flip() + theme_gdocs() + geom_text(aes(label=frequency), colour="white",hjust=1.25,size=5.0)  #add the numbers on the bars


############################## create a dendrogram - terms will be closer if they appear together in many documents 
#how many terms do we have in tdm? how many documents?
tdm
inspect(tdm[1,1:10])

#remove sparse terms. include all terms with 97.5% or less zeros (at least in 34 documents)
tdm2 <- removeSparseTerms(tdm, sparse =0.975) #42 terms were left
nrow(tdm2)
as.matrix(tdm2[1:42,1:20])

#dist - computes the distance between the term vectorshttp://127.0.0.1:41601/graphics/plot_zoom_png?width=1536&height=807
#hclust assumes each term is its own cluster and then iteratively attempts to join the two most similar clusters
hc <- hclust(dist(tdm2, method = "euclidean"))
#plot the dendrogram
#remove the y axis and add a title
plot(hc,yaxt='n',main = '@DeltaAssist Dendrogram') 


############### create a word cloud 
library(wordcloud)
head(freq.df)
set.seed(1)

display.brewer.all()#display color sets

wordcloud(freq.df$word,freq.df$frequency, max.words = 50,random.order = FALSE, colors=brewer.pal(8,"Dark2"), rot.per = 0.2, scale=c(3.5,0.25)) # color set, and 90 degree proportion 

# ----------- Part 5 - Topic Modeling ---------------
library(ldatuning)
library(LDAvis)
library(servr)
library(topicmodels)

dtm <- DocumentTermMatrix(corpus)


#------------- Question 2 ---------------
##run LDA with 3 topics
lda <- LDA(dtm, k = 3, control=list(seed=10,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic <- terms(lda, 10)
termsByTopic

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 1, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", #VEM/Gibbs
  control = list(seed = 10,alpha=0.1),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)



























replace_emoji("💀")


replace_emoji("😂😂")

nrc_sentiments <- get_sentiments("nrc")
dfasd<-sentimentr_data("hash_sentiment_emojis")

lexicon::hash_emojis_identifier[x='<U+0001F480>',]

lexicon::hash_emojis

ls("package:lexicon")

head(hash_sentiment_emojis)
ls("package:sentimentr")





