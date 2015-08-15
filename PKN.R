###############################################################################
# PKN calculation
# ver 1.0
# author: khmelkoff
###############################################################################

### Load the data
load("Data/unigram.RData")
load("Data/bigram.RData")
load("Data/trigram.RData")
load("Data/foregram.RData")
load("Data/one_freq.RData")

load("Data/PKN2.RData")


library(data.table)
library(parallel)
library(foreach)
library(doParallel)

Y2 <- one_freq$bn/(one_freq$bn + 2*nrow(bigram[count==2,]))
#Y2 <- 0.77

Y3 <- one_freq$tn/(one_freq$tn + 2*nrow(trigram[count==2,]))
#Y3 <- 0.85

Y4 <- one_freq$fn/(one_freq$fn + 2*nrow(foregram[count==2,]))
#Y4 <- 0.92

### KN for bigram calculation #################################################

Noo <- nrow(bigram)

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(bigram), .packages=c('data.table')) %dopar% {
    
    current <- bigram[i]
    id_w1 <- current$back_id
    count_w1 <- unigram[id==id_w1, count]
    count_w12 <- current$count
    w2 <- current$word
    
    discount <- max(c(count_w12 - Y2,0)) ## count_w12?
    
    # number of bigrams which w2 completes
    # number of different words w1 precede w2
    # |{w1:c(w1,w2)>0}|
    Now <- length(bigram[word==w2, back_id]) ## need unique?
    
    # number of different words completed wi-1
    # |{w2:c(w1,2w)>0}| 
    Nwo <- nrow(bigram[back_id==id_w1,])
    
    PKN <- discount/count_w1
    Lambda <- Y2*Nwo/count_w1
    PKN <- PKN + Lambda*Now/Noo
    
}

stopCluster(cl)    
PKN2 <- unlist(x)
save(PKN2, file="Data/PKN2.RData")
t_end <- Sys.time()
t_end-t_start

### KN for trigram calculation #################################################

bigram[,pkn:=PKN2]

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(trigram), .packages=c('data.table')) %dopar% { #
    
    current <- trigram[i]
    id_w12 <- current$back_id
    count_w12 <- bigram[id==id_w12, count]
    count_w123 <- current$count
    w3 <- current$word
    w2 <- bigram[id==id_w12, word]
    back_id_w2 <- unigram[word==w2, id]
    
    discount <- max(c(count_w123 - Y3,0))
    
    # number of different wordss completed bigram w12
    # |{w3:c(w12,w3)>0}|
    Nwo <- nrow(trigram[back_id==id_w12,])
    
    PKN2 <- bigram[word==w3 & back_id==back_id_w2, pkn]
    
    PKN <- discount/count_w12
    Lambda <- Y3*Nwo/count_w12
    PKN <- PKN + Lambda*PKN2
    
}

stopCluster(cl)    
PKN3 <- unlist(x)
save(PKN3, file="Data/PKN3.RData")
t_end <- Sys.time()
t_end-t_start

### KN for foregram calculation #################################################

trigram[,pkn:=PKN3]

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(foregram), .packages=c('data.table')) %dopar% {
    
    current <- foregram[i]
    id_w123 <- current$back_id
    count_w123 <- trigram[id==id_w123, count]
    count_w1234 <- current$count
    w4 <- current$word
    w3 <- trigram[id==id_w123, word]
    id_w2 <- trigram[id==id_w123, back_id]
    w2 <- bigram[id==id_w2, word]
    back_id_w2 <- unigram[word==w2, id]
    back_id_w23 <- bigram[word==w3 & back_id==back_id_w2, id]
    
    # ? back_id for w3 w2 in trigram ?
    
    id_w2 <- unigram[word==w2, id]
    
    discount <- max(c(count_w1234 - Y4,0))
    
    # number of different wordss completed trigram w123
    # |{w3:c(w12,w3)>0}|
    Nwo <- nrow(foregram[back_id==id_w123,])
    
    PKN3 <- trigram[word==w4 & back_id== back_id_w23,pkn]
    
    PKN <- discount/count_w123
    Lambda <- Y4*Nwo/count_w123
    PKN <- PKN + Lambda*PKN3
    
}

stopCluster(cl)    
PKN3 <- unlist(x)
save(PKN4, file="Data/PKN4.RData")
t_end <- Sys.time()
t_end-t_start