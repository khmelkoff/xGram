###############################################################################
# Data preparation script
###############################################################################

load("Data/unigram.RData")
load("Data/bigram.RData")
load("Data/trigram.RData")
load("Data/foregram.RData")

library(data.table)

load("Data/PKN2.RData")
load("Data/PKN3.RData")
load("Data/PKN4.RData")

unigram <- subset(unigram, select=c("id", "word"))
bigram <- subset(bigram, select=c("id", "word", "back_id"))
trigram <- subset(trigram, select=c("id", "word", "back_id"))
foregram <- subset(foregram, select=c("id", "word", "back_id"))

bigram[,p:=as.integer(10*log(PKN2))]
trigram[,p:=as.integer(10*log(PKN3))]
foregram[,p:=as.integer(10*log(PKN4))]

#### word coding
tmp <- unigram[word==bigram$word,id]
bigram[,word:=tmp]

tmp <- unigram[word==trigram$word,id]
trigram[,word:=tmp]

tmp <- unigram[word==foregram$word,id]
foregram[,word:=tmp]

##### indexing
setkey(bigram, word, back_id)
setkey(trigram, word, back_id)
setkey(foregram, word, back_id)
setkey(unigram, word, id)

save(unigram, file="Prod/unigram.RData")
save(bigram, file="Prod/bigram.RData")
save(trigram, file="Prod/trigram.RData")
save(foregram, file="Prod/foregram.RData")