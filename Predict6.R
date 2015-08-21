###############################################################################
# Next word predictive model script
# ver 6.0 (PKN back_off)
# author: khmelkoff
###############################################################################

### Load the data
load("Data/unigram.RData")
load("Data/bigram.RData")
load("Data/trigram.RData")
load("Data/foregram.RData")

library(data.table)
library(parallel)
library(foreach)
library(doParallel)

load("Data/PKN2.RData")
bigram[,pkn:=PKN2]

load("Data/PKN3.RData")
trigram[,pkn:=PKN3]

load("Data/PKN4.RData")
foregram[,pkn:=PKN4]

add_words <- c("and", "in", "the", "to", "is")
most_freq <- data.table(id=1:5, word=add_words, back_id=0, 
                        layer=1, count=100, back_word="", pkn=1)

# sum_unigram_count <- 24576915
# most_freq <-unigram[order(-count),][1:5] # unigram 5 most freq
# most_freq[,pkn:=count/sum_unigram_count]
# most_freq[,layer:=1]
# most_freq[,back_id:=0]
# most_freq[,back_word:=""]
#most_freq[,pwb:=0]

## clear and preprocess query
clear <- function(query) {
    query <- unlist(strsplit(query, split=" "))
    return(query)
}

## predict next word
predict <- function(query) {
    
    query <- clear(query)
    size <- length(query)
    
    if (size==1) {
        
        #t_start <- Sys.time() # diagnostic
        #print("predict by bigram")
        
        w1 <- query
        
        res1 <- unigram[word==w1,] # search in unigrams
        if (nrow(res1)==0) {
            w1 <- "<unk>"
            res1 <- unigram[word==w1,]
        }
        
        id1 <- res1$id
        res2 <- bigram[back_id==id1,] # search in bigrams
        res2 <- res2[word!="<unk>",]
        
        res2[,layer:=2]
        res2[,back_word:=w1]

        res2 <- res2[order(-pkn),]
        
        nres <- nrow(res2) # if nrow res2 < 5 add most_freq
        if (nres < 5) {
             # most_freq[,count:=0.4*count]
            res2 <- rbind(res2, most_freq)
        }
        
        #t_end <- Sys.time() # diagnostic
        #print(paste("layer2:", t_end - t_start))

        return(res2)
        
    } # end size 1
    
    if (size==2) { # predict by trigram #######################################
        
        #t_start <- Sys.time() # diagnostic
        #print("predict by bigram & trigram")
        
        w1 <- query[(size-1)]
        w2 <- query[size]
        
        # search the first word in unigrams
        res11 <- unigram[word==w1,] 
        if (nrow(res11)==0) {
            w1 <- "<unk>"
            res11 <- unigram[word==w1,]
        }
        
        # search the second word in unigrams
        res12 <- unigram[word==w2,] 
        if (nrow(res12)==0) {
            w2 <- "<unk>"
            res12 <- unigram[word==w2,]
        }
        
        id12 <- res12$id
        id11 <- res11$id
        
        res2 <- bigram[word==w2 & back_id==id11,] # search in bigrams
        id2 <- res2$id
        
        res3 <- trigram[back_id==id2,] # search in trigrams
        res3 <- res3[word!="<unk>",]

        res3[,layer:=3]
        res3[,back_word:=paste(w1,w2)]
        
        if (nrow(res3)<5) {
            res2 <- bigram[back_id==id12,] # search in bigrams
            res2 <- res2[word!="<unk>",]
            
            res2[,layer:=2]
            res2[,back_word:=w2]
            
            res <- rbind(res3[order(-res3$pkn),],res2[order(-res2$pkn),])
            
        } else {
            res <- res3[order(-res3$pkn),]
        }
        
        nres <- nrow(res) # if nrow res < 5 add most_freq
         if (nres < 5) {
             most_freq[,count:=0.16*count]
             res <- rbind(res, most_freq)
         }
        
        #t_end <- Sys.time() # diagnostic
        #print(paste("layer3:", t_end - t_start))
        
        return(res)
        
    } # end size 2
    
    if(size>2) { # predict by foregram ########################################
        
        #t_start <- Sys.time() # diagnostic
        #print("predict by bigram, trigram and foregram")
        
        w1 <- query[(size-2)]
        w2 <- query[(size-1)]
        w3 <- query[size]
        
        # search the third word in unigrams
        res13 <- unigram[word==w3,]
        if (nrow(res13)==0) {
            w3="<unk>"
            res13 <- unigram[word==w3,]    
        }    
        
        res12 <- unigram[word==w2,]
        if (nrow(res13)==0) {
            w2="<unk>"
            res12 <- unigram[word==w2,]    
        }      
        
        res11 <- unigram[word==w1,]
        if (nrow(res11)==0) {
            w1="<unk>"
            res11 <- unigram[word==w1,]
        }
        
        id11 <- res11$id
        id12 <- res12$id
        id13 <- res13$id
        
        # ???
        res22 <- bigram[word==w2 & back_id==id11,]
        id22 <- res22$id
        
        res23 <- bigram[word==w3 & back_id==id12,]
        id23 <- res23$id
        
        res33 <- trigram[word==w3 & back_id==id22,]
        id33 <- res33$id
        
        res4 <- foregram[back_id==id33,] #3+2+1
        res4 <- res4[word!="<unk>",]
        
        res4[,layer:=4]
        res4[,back_word:=paste(w1, w2, w3)]
        
        res <- res4[order(-res4$pkn)]
        
        if (nrow(res4)<5) {
            
            res3 <- trigram[back_id==id23,] #3+2
            res3 <- res3[word!="<unk>",]
            res3[,layer:=3]
            res3[,back_word:=paste(w2, w3)]
            res <- rbind(res4[order(-res4$pkn),],
                         res3[order(-res3$pkn),])
            
            if(nrow(res4)+nrow(res3)<5) {
                
                res2 <- bigram[back_id==id13,] #3
                res2 <- res2[word!="<unk>",]
                
                res2[,layer:=2]
                res2[,back_word:=w3]
                res <- rbind(res4[order(-res4$pkn),],
                             res3[order(-res3$pkn),],
                             res2[order(-res2$pkn),])
            }    
        }
        
        nres <- nrow(res) # if nrow res < 5 add most_freq
        if (nres < 5) {
            #most_freq[,count:=0.064*count]
            res <- rbind(res, most_freq)
        }
        
        #t_end <- Sys.time() # diagnostic
        #print(paste("layer4:", t_end - t_start))
        
        return(res)
    } # end size > 2
}

fiveWords <- function(data) {
    #print(data[1:10,])
    w <- unique(data$word)[1:5]
    #w <- gsub("_","'",w)
    return(w)        
}

### Accuracy ##################################################################
#### second word test #########################################################


sent <- test_bigram_freq$word
count <- test_bigram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){x[1]})
w2 <-sapply(sent, function(x){x[2]})

pred <- rep(0,length(w1))
res  <- rep(0,length(w1))

for (i in 1:length(w1)) {  #length(w1)
    if(i %% 10000 == 0) print(paste(i, "processed"))
    words <- fiveWords(predict(w1[i]))
    res[i] <- as.integer(w2[i] %in% words)
    res[i] <- res[i] * count[i]
    pred[i] <- paste(words, collapse=" ")
}

sum_count <- sum(test_bigram_freq$count)
res_tab <- cbind(w1,w2,res,pred)
sum_res <- res_tab[,3]
100*sum(as.integer(sum_res))/sum_count
# 34.52486 (back-off, PML)
## 34.55704 (back-off, PKN)

#### third word test ##########################################################
sent <- test_trigram_freq$word
count <- test_trigram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){paste(x[1],x[2])})
w2 <-sapply(sent, function(x){x[3]})

## experimental parallel
cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:length(w1), .packages=c('data.table')) %dopar% { #length(w1)
    words <- fiveWords(predict(w1[i]))
    res <- as.integer(w2[i] %in% words)
    res <- res * count[i]
    pred <- paste(words, collapse=" ")
    c(w1[i], w2[i], res, pred)
}

stopCluster(cl) 
t_end <- Sys.time()
t_end-t_start

res_tab <- t(sapply(x,c))

sum_count <- sum(test_trigram_freq$count)
#res_tab <- cbind(w1,w2,res,pred)
sum_res <- res_tab[,3]
100*sum(as.integer(sum_res))/sum_count
# 55.97867 (back-off, PML)
# 56.13627 (back-off, PKN)

#### fourth word test ###########################################################
sent <- test_foregram_freq$word
count <- test_foregram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){paste(x[1],x[2],x[3])})
w2 <-sapply(sent, function(x){x[4]})

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:length(w1), .packages=c('data.table')) %dopar% { #length(w1)
    words <- fiveWords(predict(w1[i]))
    res <- as.integer(w2[i] %in% words)
    res <- res * count[i] 
    pred <- paste(words, collapse=" ")
    c(w1[i], w2[i], res, pred)
}

stopCluster(cl) 
t_end <- Sys.time()
t_end-t_start

res_tab <- t(sapply(x,c))

sum_count <- sum(test_foregram_freq$count)
#res_tab <- cbind(w1,w2,res,pred)
sum_res <- res_tab[,3]
100*sum(as.integer(sum_res))/sum_count
# [1] 72.96959 (PML back-off)
# [1] 73.38884 (PKN back-off)


### quize test strings ########################################################

test <- "Very early - observation on the Bills game: Offense still struggling but"
test <- "Very early - observation on the Bills game: Offense still struggling"

s1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I_d"
ans1 <-c("give", "eat", "sleep", "die")

s2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
ans2 <- c("financial", "spiritual", "horticultural", "marital")    

s3 <- "I'd give anything to see arctic monkeys this"
ans3 <- c("month", "decade", "weekend", "morning")

s4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
ans4 <- c("happiness", "hunger", "stress", "sleepiness")

s5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
ans5 <- c("walk", "picture", "minute", "look")

s6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
ans6 <- c("incident", "case", "account", "matter")

s7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
ans7 <- c("toe", "arm", "finger", "hand")

s8 <- "Every inch of you is perfect from the bottom to the"
ans8 <- c("center", "middle", "top", "side")

s9 <- "I?m thankful my childhood was filled with imagination and bruises from playing"
ans9 <- c("outside", "weekly", "inside", "daily")

s10 <- "I like how the same people are in almost all of Adam Sandler s"
ans10 <- c("pictures", "stories", "movies", "novels")

# old bigram accuracy
#34.42709
#34.52486
#34.55712 KN + most freq
#34.51745 MKN without most freq
#34.5569 MKN + most freq
#12.9 WB

# old trigram accuracy
#55.89868

# old foregram accuracy
# [1] 72.96959

