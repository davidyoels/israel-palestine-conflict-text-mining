# ------------------ Lesson 1 -----------------------
setwd(paste0(getwd(), '/src'))
#Set Options
options(stringsAsFactors=FALSE) #don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')
set.seed(123)

#'@description Load libraries
library(stringi)
library(stringr) 
library(tm)
library(stringi)
library(textstem)
library(ggplot2)
library(ggthemes)
library(textclean)
library(dplyr)
library(sentimentr)
library(topicmodels)
library(wordcloud)
library(ldatuning)

# read csv
ipc.df<-read.csv('./files/pse_isr_reddit_comments_min.csv', encoding = 'UTF-8') #read the csv file
head(ipc.df) #print 6 first rows
nrow(ipc.df) #number of rows

df<-data.frame(text = ipc.df$self_text,nchar = nchar(ipc.df$self_text)) ##create new data frame
# mean(nchar(text.df$self_text)) #mean of all text rows

### Document Term Matrix
# Create corpus - vector source
ipc.corpus <- VCorpus(VectorSource(ipc.df$self_text))

removePattern <- function(x, pattern) gsub(x, pattern = pattern, replacement = "")
urlPattern <- "http\\S*" # URL pattern
removeURL <- function (x) removePattern(x, urlPattern) # Removes all URLs starts with http

custom.stopwords <- c(stopwords("english"),'&gt;',"like","can","make","just","say","one","will",
                      "think","know","also","get", "want", "see","use","take","happen", "way",
                      "give","try","now","thing", "comment", "year","say", "GMT",
                      "they", "are", "into","the", "have", "question", "automatically", "gt;") #Words to remove

clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

print.corpus <- function(corpus, start=1, end=1){
  for(i in start:end){
    print(corpus[[i]][1])
  }
}

ipc.corpus.cln<-clean.corpus(ipc.corpus)
ipc.corpus.cln.df <- data.frame(text=unlist(sapply(ipc.corpus.cln, `[`, "content")), 
                                       stringsAsFactors=F)

print.corpus(ipc.corpus.cln,1,3)

ipc.dtm <- DocumentTermMatrix(ipc.corpus.cln) # DTM
ipc.dtm.m <- as.matrix(ipc.dtm)
inspect(ipc.dtm[1:10, 1:50]) #inspect the result, 10 first terms, 50 documents

ipc.tdm <- TermDocumentMatrix(ipc.corpus.cln) # TDM
ipc.tdm.m <- as.matrix(ipc.tdm)
inspect(ipc.tdm[1:10, 1:30]) #inspect the result, 10 first terms, 30 documents

# ---------------- Lesson 3 ---------------
#summarize the rows (number of appearances of each term)
term.freq<-rowSums(ipc.tdm.m)
term.freq[1:10] # term frequency of the first 10 terms

#create a dataframe with two columns, word and frequency
freq.df<-data.frame(word=names(term.freq),frequency=term.freq) 

#sort by frequency
freq.df<-freq.df[order(freq.df[,2], decreasing=T),] 
head(freq.df,10)

######################Visualizations##############################

############### create a word frequency bar plot
#convert the unique words from a string to a factor with unique levels
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

freq.df<-freq.df[order(freq.df$frequency, decreasing = TRUE),]

#defining the levels will keep the original order
ggplot(freq.df[1:30,], aes(x=word,y=frequency)) + geom_bar(stat="identity", fill='darkred') + 
  coord_flip() + theme_gdocs() + geom_text(aes(label=frequency), colour="white",hjust=1.25,size=5.0)  #add the numbers on the bars

############################## create a dendrogram - terms will be closer if they appear together in many documents 
#how many terms do we have in tdm? how many documents?
ipc.tdm
inspect(ipc.tdm[1:10,1:10])

#remove sparse terms. include all terms with 97.5% or less zeros (at least in 34 documents)
ipc.tdm2 <- removeSparseTerms(ipc.tdm, sparse =0.975) #42 terms were left
nrow(ipc.tdm2)
as.matrix(ipc.tdm2[1:42,1:20])

#dist - computes the distance between the term vectors
#hclust assumes each term is its own cluster and then iteratively attempts to join the two most similar clusters
hc <- hclust(dist(ipc.tdm2, method = "euclidean", ))
#plot the dendrogram
#remove the y axis and add a title
plot(hc,yaxt='n',main = '@DeltaAssist Dendrogram') 
# cutree(hc, k = 3)

############### create a word cloud 
head(freq.df)
display.brewer.all()#display color sets
wordcloud(freq.df$word,freq.df$frequency, max.words = 50,random.order = FALSE,
          colors=brewer.pal(8,"Dark2"), rot.per = 0.2, scale=c(3.5,0.25)) # color set, and 90 degree proportion 


clean.corpus.snt <- function(corpus){
  # corpus <- tm_map(corpus, content_transformer(replace_emoji)) 
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- remove_non_english(corpus) # Removes non-enlglish words
  
  return(corpus)
}

hash_sentiment_with_emojies <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                                  x = data.frame(
                                                                    words = tolower(lexicon::emojis_sentiment$name),
                                                                    polarity = lexicon::emojis_sentiment$sentiment,
                                                                    stringsAsFactors = FALSE
                                                                  )
)

replace_emoji("😂")

lexicon::emojis_sentiment[lexicon::emojis_sentiment$name=="face with tears of joy",]

hash_sentiment_with_emojies[hash_sentiment_with_emojies$x == "face with tears of joy"]

ipc.corpus.cln.sen<-clean.corpus.snt(ipc.corpus)
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpus.cln.sen, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                                         stringsAsFactors=F)

ipc.corpus.sntr<-sentiment_by(unlist(corpus_contents), polarity_dt = hash_sentiment_with_emojies) #Calculate the average polarity of each comment

# ------------- Part 3 - Sentiment Analysis -------------

ipc.corpus.sntr.df<-data.frame(sentiment=ipc.corpus.sntr$ave_sentiment,content=ipc.df$self_text,
                           filtered_content=unlist(corpus_contents))

lexicon::hash_sentiment_jockers_rinker[c("skull")]
lexicon::hash_sentiment_jockers_rinker[c("starve","hoard","shoot", "aid","food")]
lexicon::hash_sentiment_jockers_rinker[c("tears")]
lexicon::hash_sentiment_huliu[c("only")]

library(emoji)

ipc.df$sentiment<-corpus.cleaned.sntr$ave_sentiment


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


# -------------- NEED TO CHECK THIS CODE -------------------
####construct a Polarized Tag Plot - we would like to see the words with the highest difference in frequency
library(plotrix)
#only terms that appear in both documents
common.words <- subset(tdm.all.m, tdm.all.m[,1] > 0 & tdm.all.m[,2] > 0) #578 terms (out of 4049)
tail(common.words)
difference <- abs(common.words[,1] - common.words[,2]) #find the difference between the frequencies
# add a new column that contains the difference
common.words <- cbind(common.words, difference)
common.words[1:20,1:3]

#order by the third column in decreasing order
common.words <- common.words[order(common.words[,3],decreasing = TRUE), ]
head(common.words)
#select the first 25 term values
top25.df <- data.frame(x = common.words[1:25, 1],y = common.words[1:25, 2],
                       labels = rownames(common.words[1:25, ])) #TODO: CHECK
head(top25.df)
#create the plot
# x contains the amazon frequency, y the delta frequency
pyramid.plot(top25.df$x, top25.df$y, labels = top25.df$labels,
             gap = 30, top.labels = c("Amazon", "Words", "Delta"),
             main = "Words in Common", unit = NULL)
# -------------- NEED TO CHECK THIS CODE -------------------

extract_sentiment_terms(ipc.corpus.sntr.df$filtered_content)

########################adding words to the polarity lexicon ###########################################


#valence shifters
lexicon::hash_valence_shifters[y == 1] #negators = 1
lexicon::hash_valence_shifters[y == 2] #amplifiers [intensifiers] = 2
lexicon::hash_valence_shifters[y == 3] #de-amplifiers [downtoners] = 3

# emotion 
emotion<-emotion(ipc.corpus.sntr.df$filtered_content)
plot(emotion(text))

# emotion terms
extract_emotion_terms(ipc.corpus.sntr.df$filtered_content, emotion_dt = lexicon::hash_nrc_emotions, un.as.negation = TRUE)

###################################Airbnb case study#####################################################

#create a histogram and a density plot to see the polarity distribution

ggplot(ipc.corpus.sntr, aes(x=ave_sentiment, y=..density..))+ theme_gdocs() + #..density..is a computed variable, used to calculate the density of x, part of ggplot2.
  geom_histogram(binwidth=.1,fill="darkred",colour="grey60", size=.2) + geom_density(size=.75)

#the center is 0
#scaling the polarity score vector moves the average to zero
#add the polarity to the dataset

ipc.df$sentiment_scaled<-scale(ipc.corpus.sntr$ave_sentiment) 


# How many positive and negative reviews
table(sign(ipc.df$sentiment_scaled)) #scaled #sign returns -1,0 or 1
table(sign(ipc.df$sentiment)) #not scaled

write.csv(ipc.df,"ipc.df with polarity - compare scale and sentimentr.csv")


###########advanced - highlight sentences within elements by sentiment polarity
# -------------------- NEED TO CHECK THIS CODE ---------------------
library(magrittr)
library(dplyr)
library(data.table)
bos.airbnb.table <- as.data.table(ipc.df)

ipc.df[1:20,]

bos.airbnb.table %>% select(comment_id,self_text) %>%
  filter(comment_id %in% sample(unique(comment_id), 50)) %>% #choose 50 reviews
  mutate(review = get_sentences(self_text)) %$%  # mutate() adds new variables and preserves existing ones
  sentiment_by(review, comment_id) %>%  #calculate sentiment for each comment, keeps review_id column
  highlight()
# -------------------- NEED TO CHECK THIS CODE ---------------------


# --------------- Lesson 5 -----------------

#clean.data function
clean.data.lda <- function(text){
  myCorpus <- VCorpus(VectorSource(text))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))# convert to lower case
  myCorpus <- tm_map(myCorpus, removeWords, custom.stopwords) #remove stopwords
  myCorpus <- tm_map(myCorpus, removeNumbers)# remove numbers
  myCorpus <- tm_map(myCorpus, removePunctuation)# remove punctuation
  myCorpus <- tm_map(myCorpus, stripWhitespace)# remove extra whitespace
  #myCorpus <- tm_map(myCorpus, content_transformer(lemmatize_strings)) 
  myCorpus <- tm_map(myCorpus, stemDocument)
  
  return(myCorpus)
}

ipc.lda.corpus.cln <- clean.data.lda(ipc.df$self_text)

print.corpus(ipc.lda.corpus.cln, 1, 5)

# find top terms
# TDM, *without* tfXidf weighting
#tdm <- TermDocumentMatrix(tweetCorpus)
#inspect(tdm[1:15, 1:15])

# view top frequent terms
#term.freq <- rowSums(as.matrix(tdm)) #summarize each row (number of appearances of each term)
#top.terms <- term.freq[order(term.freq, decreasing = T)][1:100]
#top.terms[1:10]


## topic modeling
# DTM, *without* tfXidf weighting
ipc.lda.dtm <- DocumentTermMatrix(ipc.lda.corpus.cln)
# we can remove sparse terms
# ipc.lda.dtm <- removeSparseTerms(ipc.lda.dtm, .979)
warnings()

inspect(ipc.lda.dtm)

ui = unique(ipc.lda.dtm$i)
text_dtm.new = ipc.lda.dtm[ui,]

result <- FindTopicsNumber(
  text_dtm.new,
  topics = seq(from = 1, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", #VEM/Gibbs
  control = list(seed = 10,alpha=0.1),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

##run LDA with the recommended number of topics
lda <- LDA(text_dtm.new, k = 3, control=list(seed=10,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic <- terms(lda, 10)
termsByTopic

posterior(lda)$topics[1:10,] #topic distribution for each document (ten first documents)
posterior(lda)$terms[,1:10] # term distribution among topics
head(ipc.df$self_text, 5)
#check for lda_VEM, lda_Gibbs

topics <- topics(lda)
topics  #assignment of term to a topic
table(topics) #number of terms in each topic
library(dplyr)
topics.labeled <- recode(topics, '1'='Topic 1','2'='Topic 2','3'='Topic 3')
table(topics.labeled)

## plot topic over time
#library(lubridate)
#tweets$created_at_date <- as.Date(dmy_hm(tweets$created_at,tz=Sys.timezone())) #convert character to Date
#topics.by.date <- data.frame(date=tweets$created_at_date, topic=factor(topics.labeled))

# ----------- NEED TO CHECK THIS CODE -----------------
topics.by.date <- data.frame(date=as.Date(ipc.df[1:989,]$created_time), topic=factor(topics.labeled))
#density plot
ggplot(topics.by.date, aes(date, fill = topic)) + 
  geom_density(alpha = 0.3) # alpha is a transparency param

# bar plot
ggplot(topics.by.date, aes(date, fill = topic)) + 
  geom_bar(position = "stack")+ # position = "stack" - atop one another 
  scale_x_date(date_breaks = "days") + #add breaks by day 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#create a data frame with tweet, date and topic
library(qdap)
cleaned.df <- as.data.frame(tweetCorpus)

tweets.with.topic <- data.frame(text = tweets$text, created = as.character(tweets$created_at), 
                                topic = as.character(topics.by.date$topic),
                                cleand_text = cleaned.df$text)
write.csv(tweets.with.topic, 'tweets_with_topic_pop_0523.csv')
# ----------- NEED TO CHECK THIS CODE -----------------





###############advanced
###############interactive visualization
library(LDAvis)
library(servr)
####################################################################################
# Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
# for use with LDAvis

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
###############################
# Apply the function
Jason <- topicmodels2LDAvis(lda)
serVis(Jason)
# click on each topic to see top terms and frequency

lda <- LDA(text_dtm.new, k = 6, control=list(seed=10,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic <- terms(lda, 10)
termsByTopic

Jason2 <- topicmodels2LDAvis(lda)
serVis(Jason2)


















###################################################################################
#build a model and apply on new data
dtm.train <- dtm[1:(dtm$nrow-5),]
dtm.valid <- dtm[(dtm$nrow-4):dtm$nrow,]
lda.train <- LDA(dtm.train, control = list(alpha = 0.1,seed=10), k = 5)
lda.inf <- posterior(lda.train, dtm.valid) #compute posterior probabilities for new data
lda.inf


#Text mining in R
#part 4.2 code Cluster analysis
setwd("E:/Text mining course/4")

##########################################################################################
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL','C')

library(skmeans)
library(tm)
library(stringr)

# a function to clean the corpus
clean.data <-function(text){
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'), "customer","service","customers","calls"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}

resumeDf<-read.csv('Resumes.csv', header = T)
resumeDf$text <- iconv(resumeDf$text, "latin1", "ASCII", sub=" ")
resumeDf$text <- str_replace_all(resumeDf$text, "[[:punct:]]", " ") #replace punctuation marks with space
#create the corpus and clean it
resumeCorpus <- clean.data(resumeDf$text)

for(i in 1:5){
  print(resumeCorpus[[i]][1])
}

#create a DTM
resumeDTM <- DocumentTermMatrix(resumeCorpus,control = list(weighting = weightTfIdf))
inspect(round(resumeDTM,4)[1:5,1:5])
#resumeDTM <- removeSparseTerms(resumeDTM, .98) # 0.98 - the maximal allowed sparsity per term.

#cluster to 3 groups - K-Means
set.seed(1)
resumeClusters<-kmeans(resumeDTM,3)
resumeClusters$cluster
#partialCenters <- resumeClusters$centers[1:3,1:12]
#c1 <- which(resumeClusters$centers[1,]>0) #center 1 terms

# plotting the total number of documents in each cluster
barplot(resumeClusters$size, main='k-means') 

##Spherical K-Means Clustering
#the distance calculation is based on cosine similarity

#hard clustering
set.seed(1)
resumeClusters <- skmeans(resumeDTM, 3)
barplot(table(resumeClusters$cluster), main='Spherical k-means')

resumeClusters$cluster
resumeDf$cluster <- resumeClusters$cluster
#centers.s <- resumeClusters$prototypes

#fuzzy/soft clustering. The m parameter is the fuzziness of the clusters.
#resumeClustersFuzzy <- skmeans(resumeDTM, 3, m = 1.2)
#resumeClustersFuzzy$membership

## analyze clusters (run for each clustering algorithm separately)
#aggregate the original text by cluster
dataForAnalysis <- aggregate(resumeDf$text~resumeClusters$cluster, 
                             FUN = paste, 
                             collapse = " ")
#clean the data
dataForAnalysisCorpus <- clean.data(dataForAnalysis[,2])

#create TDM
dataForAnalysisTdm <- TermDocumentMatrix(dataForAnalysisCorpus, 
                                         control=list(weighting=weightTfIdf))
dataForAnalysisTdm.m <- as.matrix(dataForAnalysisTdm)

#inspect the DTM
dataForAnalysisTdm.m[1:10,1:3]
#change column names
colnames(dataForAnalysisTdm.m) <- c("Cluster 1", "Cluster 2", "Cluster 3")

#comparison cloud
#windows()
comparison.cloud(dataForAnalysisTdm.m, 
                 max.words = 50, 
                 random.order = FALSE, 
                 colors = c('red','blue', 'green'), title.size=1)


#top terms
data.frame(cluster1 = names(sort(dataForAnalysisTdm.m[,1],decreasing=T)[1:6]), 
           cluster2 = names(sort(dataForAnalysisTdm.m[,2],decreasing=T)[1:6]), 
           cluster3 = names(sort(dataForAnalysisTdm.m[,3],decreasing=T)[1:6]))


#write the result to csv
resumeDf$cluster <- resumeClusters$cluster
write.csv(resumeDf, "Resume with cluster 2021.csv")

########advanced
##comparison cloud based on cl_prototypes (cluster centers)
library(clue)
s.clus.proto <- t(cl_prototypes(resumeClusters))
comparison.cloud(s.clus.proto, 
                 max.words = 50, 
                 random.order = FALSE, 
                 colors = c('red','blue', 'green'), title.size=1)

#top terms based on cl_prototypes
data.frame(cluster1 = names(sort(s.clus.proto[,1],decreasing=T)[1:10]), 
           cluster2 = names(sort(s.clus.proto[,2],decreasing=T)[1:10]), 
           cluster3 = names(sort(s.clus.proto[,3],decreasing=T)[1:10]))





library(text2vec) 
library(tm) 
library(qdap) #convert corpus to data.frame
library(ggplot2)
vignette(text2vec)
## read data
resumeDf <- read.csv("Resumes.csv")
resumeDf <- read.csv("test.csv") #small example


# a function to clean the corpus
clean.data <-function(text){
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'), "customer","service","customers","calls"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}
resumeCorpus <- clean.data(resumeDf$text)

# create word tokens and vocabulary
processedDocuments <- as.data.frame(resumeCorpus)
it <- itoken(processedDocuments$text, progressbar = FALSE)
#v <- create_vocabulary(it)  #run for small example
v <- create_vocabulary(it) %>% 
  prune_vocabulary(doc_proportion_max = 0.1, #maximum proportion of documents which should contain term
                   term_count_min = 5)

vectorizer <- vocab_vectorizer(v)


# Term-co-occurence matrix  
set.seed(1)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L) 
tcm[1:10,1:4]
#as.matrix(tcm) #run only for small example

# fit text2vec model
glove <- GlobalVectors$new(rank = 2, x_max = 10) #length 2 vectors 
#fit Glove model to input matrix
word_vectors_main  <- glove$fit_transform(tcm, n_iter = 20)
word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context)
plot.data <- data.frame(word_vectors)
ggplot(plot.data, 
       aes(x = X1, y = X2)) +
  geom_text(
    label=rownames(plot.data), 
    nudge_x = 0.01, nudge_y = 0.01
  )

## Wikipedia example
## from: https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html
library(data.table)
library(lsa) # for cosine

wiki <- readLines("wikipedia.dat", n = 1, warn = FALSE)

tokens <- space_tokenizer(wiki) #faster but less general
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove <- GlobalVectors$new(rank = 50, x_max = 10) #vector length = 50
word_vectors_main <- glove$fit_transform(tcm, n_iter = 50)
word_vectors_context <- glove$components
word_vectors <- word_vectors_context + t(word_vectors_main)
# paris - france + germany
berlin <- word_vectors[,"paris", drop = FALSE] - #drop = FALSE, keep the dimensions of the array, don't convert to vector
  word_vectors[,"france", drop = FALSE] + 
  word_vectors[,"germany", drop = FALSE]
cos_sim <- apply(word_vectors, 2, cosine, berlin) #2-apply on columns
head(sort(cos_sim, decreasing = TRUE), 10)

word_vectors[1:50,1:10]#several word vectors 






#Text mining in R
#part 5.1 code Document Classification
setwd("E:/Text mining course/5")

##########################################################################################
library(tm) #preprocessing, term document matrix, corpus
library(Matrix) #provides memory efficient methods for manipulating sparse matrices.
library(glmnet) #regression model
library(caret) #data partitioning, confusion matrix
library(pROC) #ROC curves
library(ggthemes) #visualization
library(ggplot2) #visualization

options(stringsAsFactors = F)

#cleaning function
headline.clean <-function(text){
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  
  return(corpus)
}

####################################################################################

#Partition the data
#The createDataPartition function selects random number of rows based on the dependent variable distribution.
headlines<-read.csv('all_3k_headlines.csv',encoding="UTF-8")

headlines$headline <- iconv(headlines$headline, "UTF-8", "ASCII", sub="") #convert UTF-8 to ASCII

set.seed(1)
train<-createDataPartition(headlines$y,p=0.6,list = F) # p -> the percentage of data that goes to training. #list = F -> the result will be in a matrix and not a list
train.headlines<-headlines[train,]
test.headlines<-headlines[-train,]

#clean the training set
clean.train<-headline.clean(train.headlines$headline) #clean the training set
for(i in 1:5){
  print(clean.train[[i]][1])
}

#create a DTM
train.dtm <- DocumentTermMatrix(clean.train,control = list(weighting = weightTfIdf)) 
train.dtm
inspect(train.dtm)

train.matrix<-as.matrix(train.dtm)
#change the object into a sparse matrix (memory efficient)
train.matrix<-Matrix(train.matrix, sparse = T) #sparse = T will automatically become true if more than half the values are 0
dim(train.matrix)

#build the model
#Note that the results of cv.glmnet are random, since the folds are selected at random. 
#Users can reduce this randomness by running cv.glmnet many times, and averaging the error curves.
set.seed(1001)
cv<-cv.glmnet(train.matrix, y=as.factor(train.headlines$y), 
              alpha=0.1, 
              family='binomial', 
              nfolds = 10, 
              intercept = F, 
              type.measure = 'class')
###################
#alpha=1 is the default. alpha can be between 0 to 1. alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#alpha = 1 means the model will be simpler, more generic.
#family - use 'binomial' when y is binary. Use 'multinomial' when there are multiple classes.
#nfolds - performs cross validation when constructing a glmnet model, number of folds - 10, folds are chosen randomly
#type.measure - loss to use for cross-validation. "class" applies to binomial and multinomial logistic regression only and gives misclassification error.
#intercept = F, the intercept will be set to zero.
###################
plot(cv)
log(cv$lambda.1se)
log(cv$lambda.min)

#apply the model to the training set
#preds<-predict(cv,train.matrix,type='class',s=cv$lambda.1se)
preds<-predict(cv,train.matrix,type='response',s=cv$lambda.1se) #for auc we need probability
head(preds,10) #1 = clickbait

###################
#type='class', the return information will be 1 or 0. type="response" will return probabilities.
#can change s to be cv$lambda.min to use a more complex model that minimizes the misclassification rate.
###################

#Evaluate the model
#create a roc curve
train.auc<-roc(train.headlines$y,as.numeric(preds))
train.auc
plot(train.auc)

#create the confusion matrix
confusion.train <- confusionMatrix(as.factor(ifelse(preds>=0.5,1,0)),as.factor(train.headlines$y))
confusion.train

#Test the model on new data
#clean the test set
clean.test<-headline.clean(test.headlines$headline)

#create the DTM
#match the matrix to contain same words as the training matrix
test.dtm <- DocumentTermMatrix(clean.test, control = list(dictionary=Terms(train.dtm), weighting = weightTfIdf))
test.dtm
test.matrix<-as.matrix(test.dtm)
test.matrix<-Matrix(test.matrix)

#predict on the test data
preds.test<-predict(cv,test.matrix,type='response',s=cv$lambda.1se)
head(preds.test)

#create a data frame that contains the doc row and the prediction
#headline.preds<-data.frame(doc_row = rownames(test.headlines),class=preds.test[,1])
#head(headline.preds)

#calculate AUC
test.auc<-roc(test.headlines$y,as.numeric(preds.test))
test.auc
plot(test.auc)

#compare train and test
plot(train.auc,col="blue",main="RED = test, BLUE = train", adj=0) #adj=0, title aligned left
plot(test.auc, add=TRUE, col="red", lty=2)#lty=2, line is dashed

#confusion matrix
confusion.test <- confusionMatrix(as.factor(ifelse(preds.test>=0.5,1,0)),as.factor(test.headlines$y))
confusion.test


#######################################################################################
#finding the most impactful words
#model coefficients
glmnet.coef<-as.matrix(coef(cv, s='lambda.min'))

#add column names (words,glmnet_coefficients)
glmnet.coef<-data.frame(words=row.names(glmnet.coef),glmnet_coefficients=glmnet.coef[,1]) 

#sort in decreasing order
glmnet.coef<-glmnet.coef[order(glmnet.coef$glmnet_coefficients, decreasing = T),]

#convert words to factor (for ggplot)
glmnet.coef$words<-factor(glmnet.coef$words,levels=unique(glmnet.coef$words)) 

#1st and 3rd quartiles are 0 because the lasso regression forced many of the term coefficients to 0.
summary(glmnet.coef$glmnet_coefficients) 

#number of positive coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients>0)) 
#number of negative coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients<0)) 
#number of 0 coefficients
length(subset(glmnet.coef$glmnet_coefficients,glmnet.coef$glmnet_coefficients==0))

#create the density plot for word coefficients
ggplot(glmnet.coef,aes(x=glmnet_coefficients)) +
  geom_line(stat = 'density', color='darkred',size=1) + 
  theme_gdocs()

#inspect the meaningful terms
top.coef<-rbind(head(glmnet.coef,10),tail(glmnet.coef,10))
top.coef$impact<-ifelse(top.coef$glmnet_coefficients>0,"Positive","Negative")#positive=clickbait
top.coef

ggplot(top.coef, aes(x=glmnet_coefficients, y=words)) + 
  geom_segment(aes(yend=words), xend=0, colour="grey50")+
  geom_point(size=3, aes(colour=impact)) + theme_few()
#geom_segment draws a straight line between points (x, y) and (xend, yend)

























