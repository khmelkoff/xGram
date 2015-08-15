###############################################################################
# Tree and unknown processing
# author: khmelkoff
###############################################################################

### Load the data
load("Data/unigram_freq.Rdata")
load("Data/bigram_freq.Rdata")
load("Data/trigram_freq.Rdata")
load("Data/foregram_freq.Rdata")

library(data.table)

### Creat the fixed vocabulary
unigram_freq <- unigram_freq[order(-count),]
count_sum <- sum(unigram_freq$count)
unigram_freq[,cum:=100*cumsum(count)/count_sum]
fixed_vocab <- unigram_freq[cum<99.1,word] # 99% coverage

### unigram processing
know_idx <- unigram_freq$word %in% fixed_vocab
unigram_clear <- unigram_freq[know_idx,]
unigram_unk <- unigram_freq[!know_idx,]
unigram_unk[,word := "<unk>"]

unigram_unk <- unigram_unk[,mean(count),by=word]
unigram_unk <- unigram_unk[,V1:=as.integer(V1)]
setnames(unigram_unk, "V1","count")

unigram <- rbind(unigram_clear[,word,count], unigram_unk)
unigram <- unigram[,id:=1:nrow(unigram)]
setkey(unigram, key=word)
save(unigram, file="Data/unigram.RData")

rm(unigram_freq)
rm(unigram_clear)
rm(unigram_unk)

### bigram processing
sent <- bigram_freq$word
count <- bigram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){x[1]})
w2 <-sapply(sent, function(x){x[2]})
w1 <- ifelse(w1 %in% fixed_vocab, w1, "<unk>")
w2 <- ifelse(w2 %in% fixed_vocab, w2, "<unk>")
w <- sapply(1:length(w1), function(x){
    paste(w1[x],w2[x])
})

bigram <- data.table(word=w, count=count)
bigram <- bigram[,mean(count),by=word]
bigram <- bigram[,V1:=as.integer(V1)]
setnames(bigram, "V1","count")
bigram <- bigram[,id:=1:nrow(bigram)]
setkey(bigram, key=word)
save(bigram, file="Data/bigram.RData")

rm(bigram_freq)

### trigram processing
sent <- trigram_freq$word
count <- trigram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){x[1]})
w2 <-sapply(sent, function(x){x[2]})
w3 <-sapply(sent, function(x){x[3]})
w1 <- ifelse(w1 %in% fixed_vocab, w1, "<unk>")
w2 <- ifelse(w2 %in% fixed_vocab, w2, "<unk>")
w3 <- ifelse(w3 %in% fixed_vocab, w3, "<unk>")
w <- sapply(1:length(w1), function(x){
    paste(w1[x],w2[x],w3[x])
})

trigram <- data.table(word=w, count=count)
trigram <- trigram[,mean(count),by=word]
trigram <- trigram[,V1:=as.integer(V1)]
setnames(trigram, "V1","count")
trigram <- trigram[,id:=1:nrow(trigram)]
setkey(trigram, key=word)
save(trigram, file="Data/trigram.RData")

rm(trigram_freq)

### foregram processing
sent <- foregram_freq$word
count <- foregram_freq$count
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){x[1]})
w2 <-sapply(sent, function(x){x[2]})
w3 <-sapply(sent, function(x){x[3]})
w4 <-sapply(sent, function(x){x[4]})
w1 <- ifelse(w1 %in% fixed_vocab, w1, "<unk>")
w2 <- ifelse(w2 %in% fixed_vocab, w2, "<unk>")
w3 <- ifelse(w3 %in% fixed_vocab, w3, "<unk>")
w4 <- ifelse(w4 %in% fixed_vocab, w4, "<unk>")
w <- sapply(1:length(w1), function(x){
    paste(w1[x],w2[x],w3[x], w4[x])
})

foregram <- data.table(word=w, count=count)
foregram <- foregram[,mean(count),by=word]
foregram <- foregram[,V1:=as.integer(V1)]
setnames(foregram, "V1","count")
foregram <- foregram[,id:=1:nrow(foregram)]
setkey(foregram, key=word)
save(foregram, file="Data/foregram.RData")

rm(foregram_freq)
gc()

###############################################################################

### Build a tree ##############################################################
#### Foregram processing
foregram <- foregram[,back_id:=0]

sent <- foregram$word
sent <- strsplit(sent, " ")
w_left <-sapply(sent, function(x){paste(x[1],x[2],x[3])})
w_right <-sapply(sent, function(x){x[4]})
foregram[,word := w_right]
back_idx <- trigram[word==w_left, id]
foregram[,back_id := back_idx]
setkey(foregram, key=word)
save(foregram, file="Data/foregram.RData")


#### Trigram processing
trigram <- trigram[,back_id:=0]

sent <- trigram$word
sent <- strsplit(sent, " ")
w_left <-sapply(sent, function(x){paste(x[1],x[2])})
w_right <-sapply(sent, function(x){x[3]})
trigram[,word := w_right]
back_idx <- bigram[word==w_left, id]
trigram[,back_id := back_idx]
setkey(trigram, key=word)
save(trigram, file="Data/trigram.RData")


#### bigram processing
bigram <- bigram[,back_id:=0]

sent <- bigram$word
sent <- strsplit(sent, " ")
w1 <-sapply(sent, function(x){x[1]})
w2 <-sapply(sent, function(x){x[2]})
bigram[,word := w2]
back_idx <- unigram[word==w1, id]
bigram[,back_id := back_idx]
setkey(bigram, key=word)
save(bigram, file="Data/bigram.RData")
