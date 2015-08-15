###############################################################################
# processing
# author: khmelkoff
###############################################################################

### Load library
library(NLP)
library(openNLP)
library(tm)
library(RWeka)
library(parallel)
library(foreach)
library(doParallel)

### Load data
load("Data/blogs_repl.RData")
blogs_data <- unlist(blogs_repl$data)
load("Data/twitter_repl.RData")
twitter_data <- unlist(twitter_repl$data)
load("Data/news_repl.RData")
news_data <- unlist(news_repl$data)

con <- file("Data/suffix.txt") 
suffix <- readLines(con, n=-1, skipNul = TRUE) # Read all line of text 
close(con)

### Cleaning & Filtering
cleaning <- function(data) {
    for (i in 1:length(data)) {
        if(i %% 10000 == 0) print(paste(i, "docs processed"))
        x <- data[i]
        # to lower case ????
        x <- tolower(x)
        
        # for twitter tags
        x <- gsub("#[[:alpha:]]+", "twitter-tag", x)
        
        # "\u0432\u0402\u2122" = '
        x <- gsub("\u0432\u0402\u2122", "'", x)
        
        # Multiple letters replacement
        x <- gsub("(.)\\1{2,}", "\\1", x)
        
        # Only words and puncts
        x <- gsub("[^A-Za-z',\\?!:;\\.-]", " ", x)
        x <- gsub("--", " ", x) ##!!!!!!!!
        
        # Mark suffixes
        slen <- length(suffix)
        for (j in 1:slen) {
            suf <- unlist(strsplit(suffix[j], split="'"))
            x <- gsub(suffix[j], paste0(suf[1], "_", suf[2]),x)
        }
        
        
        
        
        # Kill extra
        x <- gsub("( )\\1{1,}", " ", x)
        x <- gsub("^\\s+|\\s+$", "", x)
        x <- gsub("-\\s", " ", x)
        x <- gsub("\\s-", " ", x)
        x <- gsub(" \\.", ".", x)
        x <- gsub("\\s,", ",", x)
        x <- gsub("\\s\\.", ".", x)
        x <- gsub("\\s:", ":", x)
        x <- gsub("\\s;", ";", x)
        x <- gsub("\\s!", "!", x)
        x <- gsub("\\s\\?", "\\?", x)
        x <- gsub("\\.-", "\\. ", x)
        x <- gsub("\\?-", "\\? ", x)
        x <- gsub("!-", "! ", x)
        x <- gsub(":-", ": ", x)
        x <- gsub(";-", "; ", x)
        x <- gsub(",-", ", ", x)
        x <- gsub("-\\.", "\\.", x)
        x <- gsub("-\\?", "\\?", x)
        x <- gsub("-!", "!", x)
        x <- gsub("-:", ":", x)
        x <- gsub("-;", ";", x)
        x <- gsub("-,", ",", x)
        x <- gsub("^-", "", x)
        x <- gsub("-$", "", x)
        x <- gsub("\\\\", " ", x)
        x <- gsub("'-", " ", x)
        
        data[i] <- x
    }
    return(data)
}


print("Blogs data cleaning")
t_start <- Sys.time()
cleaned_blogs_data <- cleaning(blogs_data)
t_end <- Sys.time()
t_end - t_start

print("Twitter data cleaning")
cleaned_twitter_data <- cleaning(twitter_data)

print("News data cleaning")
cleaned_news_data <- cleaning(news_data)

#### Save job
save(cleaned_blogs_data, file="Data/cleaned_blogs_data.RData")
save(cleaned_twitter_data, file="Data/cleaned_twitter_data.RData")
save(cleaned_news_data, file="Data/cleaned_news_data.RData")

### Sampling ##################################################################
## 75% of the sample size
load("Data/cleaned_blogs_data.RData")
load("Data/cleaned_twitter_data.RData")
load("Data/cleaned_news_data.RData")

all_data <- c(cleaned_blogs_data,
              cleaned_twitter_data,
              cleaned_news_data)

save(all_data, file="Data/all_data.RData")
rm(cleaned_blogs_data)
rm(cleaned_twitter_data)
rm(cleaned_news_data)

set.seed(1313) ## set the seed to make your partition reproductible
sample_size <- floor(0.33 * length(all_data))
all_data <- sample(all_data, sample_size, replace=FALSE)

train_size <- floor(0.75 * length(all_data))

train_ind <- sample(seq_len(length(all_data)), size = train_size)

train <- all_data[train_ind]
test <- all_data[-train_ind]

save(train, file="Data/train.RData")
save(test, file="Data/test.RData")

rm(all_data)
rm(test)
rm(train_ind)

### Sentense tokenizer ########################################################
sent_tokenizer <- function(data) {
    # parallizer
    cl <- makeCluster(4)
    registerDoParallel(cl)
    data <- data[grep("[[:alpha:]]+", data)]
    data <- data[nchar(data)>2]
    x <- foreach(i=1:length(data), .packages=c('NLP', 'openNLP'),.verbose = T) %dopar% {
        
        big_string <- as.String(data[i])
        # sent_token_annotator
        # print(paste(i,substr(big_string,1,30)))
        sent_token_annotator <- Maxent_Sent_Token_Annotator(model="en-sent.bin")
        sent_boundaries <- annotate(big_string, sent_token_annotator)
        big_string[sent_boundaries]
        
    }
    stopCluster(cl)    
    return(unlist(x))
}

t_start <- Sys.time()
train <- sent_tokenizer(train) ### train!
t_end <- Sys.time()
t_end - t_start
save(train, file="Data/train.RData")

gc()

load("Data/test.RData")
test <- sent_tokenizer(test) ### test!
save(test, file="Data/test.RData")


### big n-gramm tokenizer #####################################################
#### Unigram
UnigramTokenizer <- function(x) WordTokenizer(x)

print("Unigram tokenisation")
txt_vector <- VectorSource(train) # vectorize
txt_corpus <- Corpus(txt_vector, # creat corpus
                     readerControl = list(language = "en"))

rm(txt_vector)

t_start <- Sys.time()
train_1bow <- DocumentTermMatrix(txt_corpus, # creat bag of words
                                 control=list(wordLengths=c(1,Inf),stemming=FALSE, tokenize = UnigramTokenizer))
t_end <- Sys.time()
t_end - t_start

save(train_1bow, file="Data/train_1bow.RData")
rm(txt_corpus)
#rm(train_1bow)

#### Bigram
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

print("Bigram tokenization")
txt_vector <- VectorSource(train) # vectorize
txt_corpus <- Corpus(txt_vector, # creat corpus
                     readerControl = list(language = "en"))

rm(txt_vector)
gc()

t_start <- Sys.time()
train_2bow <- DocumentTermMatrix(txt_corpus, # creat bag of words
                                 control=list(stemming=FALSE, tokenize = BigramTokenizer))
t_end <- Sys.time()
t_end - t_start

save(train_2bow, file="Data/train_2bow.RData")
rm(txt_corpus)
#rm(train_2bow)

#### Trigram
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

print("Trigram tokenization")
txt_vector <- VectorSource(train) # vectorize
txt_corpus <- Corpus(txt_vector, # creat corpus
                     readerControl = list(language = "en"))

rm(txt_vector)

t_start <- Sys.time()
train_3bow <- DocumentTermMatrix(txt_corpus, # creat bag of words
                                 control=list(stemming=FALSE, tokenize = TrigramTokenizer))
t_end <- Sys.time()
t_end - t_start

save(train_3bow, file="Data/train_3bow.RData")
rm(txt_corpus)
#rm(train_3bow)

#### 4-gram
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

print("4-gram tokenization")
txt_vector <- VectorSource(train) # vectorize
txt_corpus <- Corpus(txt_vector, # creat corpus
                     readerControl = list(language = "en"))

rm(txt_vector)

t_start <- Sys.time()
train_4bow <- DocumentTermMatrix(txt_corpus, # creat bag of words
                                 control=list(stemming=FALSE, tokenize = BigramTokenizer))
t_end <- Sys.time()
t_end - t_start

save(train_4bow, file="Data/train_4bow.RData")
rm(txt_corpus)
#rm(train_4bow)

##### Aggregation (Freq calculation) ##########################################
library(data.table)

freq <- function(bow) {
    dictionary <- data.table(word=bow$dimnames$Terms)
    term_count <- data.table(id=bow$j, count=bow$v)
    # Aggregation    
    term_count <- term_count[,sum(count),by=id]
    setnames(term_count, "V1","count")
    # Bad word cleaning
    dictionary <- dictionary[term_count$id,]
    term_count <- term_count[,word:=dictionary$word]
    term_count <- term_count[-grep("bad-word|web-address|mail-address|twitter-tag", term_count$word)] 
    
    # Sorting
    term_count <- term_count[order(count, decreasing = TRUE)]
    return(term_count)
}

one_freq <- list(un=0, bn=0, tn=0, fn=0)

#### unigram freq
unigram_freq <- freq(train_1bow)
#unigram_freq[, count2 := length(unique(id)),by=count]
one_freq$un <- sum(unigram_freq$count==1)
unigram_freq <- unigram_freq[unigram_freq$count!=1,]
save(unigram_freq, file="Data/unigram_freq.RData")

#### bigram freq
bigram_freq <- freq(train_2bow)
#bigram_freq[, count2 := length(unique(id)),by=count]
one_freq$bn <- sum(bigram_freq$count==1)
bigram_freq <- bigram_freq[bigram_freq$count!=1,]
save(bigram_freq, file="Data/bigram_freq.RData")

#### trigram freq
trigram_freq <- freq(train_3bow)
#trigram_freq[, count2 := length(unique(id)),by=count]
one_freq$tn <- sum(trigram_freq$count==1)
trigram_freq <- trigram_freq[trigram_freq$count!=1,]
save(trigram_freq, file="Data/trigram_freq.RData")

#### 4-gram freq
foregram_freq <- freq(train_4bow)
#foregram_freq[, count2 := length(unique(id)),by=count]
one_freq$fn <- sum(foregram_freq$count==1)
foregram_freq <- foregram_freq[foregram_freq$count!=1,]
save(foregram_freq, file="Data/foregram_freq.RData")
save(one_freq, file="Data/one_freq.RData")