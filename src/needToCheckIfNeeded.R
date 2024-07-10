paste(c('a', 'b', 'cd'), collapse='|')

DTM<-DocumentTermMatrix(corpus, control=list(weighting=weightTf)) # weightTf
as.matrix(DTM)

# weightBin
DTM<-DocumentTermMatrix(corpus, control=list(weighting=weightBin)) # weightBin
inspect(DTM)

# Sparsity
DTM <- DocumentTermMatrix(corpus, control=list(wordLengths=c(1, Inf),
                                               bounds = list(global = c(2,Inf)))) #Terms that appear in less than two document or more than Inf are discarded
inspect(DTM)
DTM <- DocumentTermMatrix(corpus)
inspect(DTM) #  1 -> 12, 0 -> 16. sparsity = 16/(12+16)
as.matrix(DTM)
# What is the sparsity of the term "many"?
# Remove sparse terms from a document-term matrix.
smallDtm <- removeSparseTerms(DTM, .7) # 0.7 - the maximal allowed sparsity per term.
inspect(smallDtm) 


##Bi-grams (tm)
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2)) #define the function BigramTokenizer
DTM_BGram <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer)) #BigramTokenizer function is passed as a parameter
inspect(DTM_BGram)


########advanced
# stem_completion - complete all stems to valid words and create a new corpus
# corpus is the corpus after stemming
# copy_corpus is the corpus before stemming
stem_completion_result <- tm_map(corpus, 
                                 content_transformer(function(x, d)
                                   paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], d), 
                                         collapse = ' ')), d = copy_corpus)

#1 - default (hash_lemmas)
lemmatize_strings(txt)
#2 - hunspell
lemma_dictionary <- make_lemma_dictionary(txt, engine = 'hunspell')
lemmatize_strings(txt, dictionary = lemma_dictionary)

# This example somtimes shows why we need to use stemming sometimes
vector1 <- c("complicatedly", "complicated", "complication")
lemmatize_words <- lemmatize_words(vector1)
lemmatize_words


## Hunspell dictionary - advanced
lemma_dictionary <- make_lemma_dictionary(vector3, engine = 'hunspell')
lemmatize_words <- lemmatize_words(vector3,dictionary = lemma_dictionary)
lemmatize_words

corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts


#------------- VIS ----------
############## word associations - which words appear together with the word 'apologies'
associations<-findAssocs(bos.airbnb.tdm, 'hamas', 0.11) #find the associations for the word 'apologies'. 0.11 is the association threshold
associations<-as.data.frame(associations) 
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels=associations$terms) #convert to factor


################## scatter plot that shows the association level between the word 'apologies' and the other words
ggplot(associations, aes(y=terms)) + 
  geom_point(aes(x=apologies), data=associations, size=5) + 
  theme_gdocs() + geom_text(aes(x=apologies,label=apologies), colour="darkred",hjust=-.25,size=5) 

