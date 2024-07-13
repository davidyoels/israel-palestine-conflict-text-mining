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


replace_emoji("I'm")

vec <- stri_trans_general(sapply(ipc.corpus, as.character),"Latin-ASCII")
ipc.corpus.cln.sen.df <- data.frame(text=vec, 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

ipc.corpu.new <- VCorpus(VectorSource(vec))

ipc.corpu.new <- tm_map(ipc.corpu.new, content_transformer(replace_emoji))
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

ipc.corpu.new <- tm_map(ipc.corpu.new, content_transformer(tolower)) #lower all texts
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)


tag_emoji_text<- function(vec){
  lexicon::emojis_sentiment
  vec<-toupper(vec)
  vec.sep<-strsplit(vec, split = " ")
  vec.all<-paste0(unlist(vec.sep), collapse = '_')
  vec.all<-paste0("EMOJI_", vec.all)
  
  return(vec.all)
}


tag_emoji_text(vec)

preprocess_corpus <- function(vec) {
  # corpus <- tm_map(corpus, content_transformer(convert_emojis))
  vec <- replace_emoji(vec)
  vec <- tag_emoji_text(vec)
  # corpus <- tm_map(corpus, content_transformer(remove_stopwords))
  # corpus <- tm_map(corpus, content_transformer(untag_emoji_text))
  return(vec)
}


text <- "I am so happy today! 😂"

# Text to find and its replacement
text_list <- c("I am so happy today! 😂", "I am so happy today! 😂😂")
text_list <- replace_emoji(text_list)

tagged<-tag_emoji_text(text_list)

# Replace text
text <- gsub(find_text, replace_text, text)

vec<-c("Why coldplay have to go indonesia. 😂")
vec <- replace_emoji(vec)
vec <- tag_emoji_text(vec)
corpus.preprocess<-preprocess_corpus(vec)

corpus.preprocess

lexicon::emojis_sentiment


vec1<-c("face with tears of joy")

check_lexicon_presence <- function(sentence, lexicon) {
  matches <- lexicon[str_detect(sentence, fixed(lexicon))]
  return(length(matches) > 0)
}

text <- check_lexicon_presence(vec1, hash_sentiment_with_emojies)

matches <- hash_sentiment_with_emojies[str_detect(vec1, fixed(hash_sentiment_with_emojies))]

# Step-by-step processing
text <- convert_emojis(text)
text <- tag_emoji_text(text)
text <- remove_stopwords(text)
text <- untag_emoji_text(text)







ipc.corpu.new <- tm_map(ipc.corpu.new, content_transformer(removeURL)) # Remove all the characters after the http
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

# Need to do something with the 😂
replace_emoji("😂")
txt<-c("I love this! 😂 😂 😂")
emoji::emoji_extract_all(txt)

emojis_location<-emoji::emoji_locate_all(txt)[[1]]
emojis_location
substr(txt,emojis_location[emojis_location[1],"start"], emojis_location[emojis_location[1],"end"])

gsub()



emoji::emoji_extract_all(txt)
emoji::emoji_replace_all("asd 😂 😂",emoji::emoji_extract_all(txt))
(txt)


ipc.corpu.new <- tm_map(ipc.corpu.new, removeWords, custom.stopwords)
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

ipc.corpu.new <- tm_map(ipc.corpu.new, removeNumbers)
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

ipc.corpu.new <- tm_map(ipc.corpu.new, content_transformer(lemmatize_strings))
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)

ipc.corpu.new <- tm_map(ipc.corpu.new, stripWhitespace)
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(ipc.corpu.new, `[`, "content")), 
                                    originalText=ipc.df$self_text,
                                    stringsAsFactors=F)





# Install and load the necessary packages
library(textclean)
library(sentimentr)

# Example vector with sentences containing emojis
sentences <- c("I love this! 😊", "This is terrible 😢")

# Replace emojis with text equivalents
sentences_clean <- replace_emoji(sentences)

# Combine the sentiment lexicons
# Note: Replace 'hash_sentiment_jockers_rinker' and 'emojis_sentiment' with your actual lexicon objects
combined_lexicon <- lexicon::hash_sentiment_jockers_rinker
combined_lexicon <- rbind(combined_lexicon, emojis_sentiment)

# Use sentimentby function to calculate sentiment for each sentence
sentiment_scores <- sentiment_by(sentences_clean, lexicon = combined_lexicon)

# View the sentiment scores
print(sentiment_scores)
























library(emoji)

replace_emoji1 <- function(text) {
  emojize <- function(match) {
    # Get the emoji description
    print(match)
    description <- emoji::emoji_name(match)
    print(description)
    # Remove the colons, replace spaces with underscores, and convert to uppercase
    description <- toupper(gsub(" ", "_", gsub(":", "", description)))
    return(description)
  }
  
  # Find all emojis in the text
  emojis <- emoji::emoji_extract(text)
  
  # Replace each emoji with its description
  for (emj in emojis$emoji) {
    text <- gsub(emj, emojize(emj), text, fixed = TRUE)
  }
  
  return(text)
}

library(emoji)
library(stringr)

replace_emoji2 <- function(text) {
  emojize <- function(match) {
    print(match)
    # Get the emoji description
    description <- emoji::emoji_name(match)
    print(description)
    # Remove the colons, replace spaces with underscores, and convert to uppercase
    description <- toupper(gsub(" ", "_", gsub(":", "", description)))
    return(description)
  }
  
  # Find all emojis in the text
  emojis <- emoji::emoji_extract(text)$emoji
  
  # Replace each emoji with its description
  for (emj in emojis) {
    text <- str_replace_all(text, fixed(emj), emojize(emj))
  }
  
  return(text)
}

# Example usage
text <- "This is so funny 😂!"
emojize <- function(match) {
  # Get the emoji description
  description <- emoji::emoji_name(match)
  # Remove the colons, replace spaces with underscores, and convert to uppercase
  description <- toupper(gsub(" ", "_", gsub(":", "", description)))
  return(description)
}

# Find all emojis in the text
emojis <- emoji::emoji_extract(text)

seq <- c(1:10)

for (i in emojis){
  print(i)
}

# Replace each emoji with its description

# Get the emoji description

# Remove the colons, replace spaces with underscores, and convert to uppercase

for (emj in emojis) {
  print(emj)
  description <- emoji::emoji_name("😂")
  emojize <- toupper(gsub(" ", "_", gsub(":", "", description)))
  
  text <- str_replace_all(text, fixed(emj), emojize)
}

head(emoji::emoji_name)

emoji::emoji_name["tears"]

replace_emoji("😂")

install.packages("emo")

library(emo)

# install.packages("emojifont")
library(emojifont)

emoji_names <- emo::ji_name("😊")
cat(emoji_names)
# install.packages("devtools")


# Convert emoji to description with underscores
emoji_desc <- emoji::emoji_name("😊")
cat(emoji_desc)

head(emoji::emoji_name)
class(emoji::emoji_name)

generate_hash_emojis<-function(){
  hash_emojis<-data.frame(x=emoji::emoji_name)
  hash_emojis<-data.frame(x=hash_emojis$x,y=rownames(hash_emojis))
  
  char_vector <- apply(hash_emojis, 1, function(row) paste(row, collapse = " "))
  # Print the character vector
  return(char_vector)
}

lexicon::hash_emoticons
class(lexicon::hash_emojis)

lexicon_hash_emojis = generate_hash_emojis()

text <- c("This is so funny 😂 😂!", "This is so funny 😂!")

hash_emojis[hash_emojis$emoji == "😂","description"]
emoji::emoji_name["health_worker"]
text

das<-lexicon::hash_emojis


replace_emoji(text,emoji_dt = hash_emoji_underscore)
# -------------- Working emojis ----------------------

ipc.corpu.new <- tm_map(ipc.corpu.new, removeWords, custom.stopwords)

hash_emoji_underscore<-lexicon::hash_emojis
hash_emoji_underscore$y<-gsub(" ", "_", hash_emoji_underscore$y)

replace_emoji_underscore<-function(vec){
  replace_emoji(vec,emoji_dt = hash_emoji_underscore)
}

custom.stopwords <- c(stopwords("english"),'&gt;',"like","can","make","just","say","one","will",
                      "think","know","also","get", "want", "see","use","take","happen", "way",
                      "give","try","now","thing", "comment", "year","say", "GMT",
                      "they", "are", "into","the", "have", "question", "automatically", "gt;") #Words to remove

clean.corpus.snt <- function(corpus){
  # corpus <- tm_map(corpus, content_transformer(replace_emoji_underscore))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
  corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  corpus <- tm_map(corpus, stripWhitespace)
  # corpus <- remove_non_english(corpus) # Removes non-enlglish words
  
  return(corpus)
}

txt<-c("I love this! 😂 😂 😂 🧑‍⚕️", "I love this! 😂 🤝 😂 😂", "I love this! 😂 😂 😂")
corpus<-VCorpus(VectorSource(txt))


lexicon::emojis_sentiment
lexicon::hash_emojis_identifier
lexicon::hash_emojis




corpus <- tm_map(corpus, content_transformer(replace_emoji_underscore))
ipc.corpus.cln.sen.df <- data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                                    originalText = txt,
                                    stringsAsFactors=F)

corpus <- tm_map(corpus, removeWords, custom.stopwords)
corpus <- tm_map(corpus, content_transformer(tolower)) #lower all texts
corpus <- tm_map(corpus, content_transformer(removeURL)) # Remove all the characters after the http
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
corpus <- tm_map(corpus, stripWhitespace)








