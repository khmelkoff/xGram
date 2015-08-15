###############################################################################
# PML calculation
# ver 1.1
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

### PML for bigram calculation ################################################

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(bigram), .packages=c('data.table')) %dopar% {
    current <- bigram[i]
    id_w1 <- current$back_id
    count_w1 <- unigram[id==id_w1, count]
    count_w12 <- current$count
    count_w12/count_w1
}

stopCluster(cl)    
PML2 <- unlist(x)
save(PML2, file="Data/PML2.RData")
t_end <- Sys.time()
t_end-t_start

### PML for trigram calculation ###############################################

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(trigram), .packages=c('data.table')) %dopar% {
    current <- trigram[i]
    id_w12 <- current$back_id
    count_w12 <- bigram[id==id_w12, count]
    count_w123 <- current$count
    count_w123/count_w12
}

stopCluster(cl)    
PML3 <- unlist(x)
save(PML3, file="Data/PML3.RData")
t_end <- Sys.time()
t_end-t_start

### PML for foregram calculation ################################################

cl <- makeCluster(4)
registerDoParallel(cl)
t_start <- Sys.time()
x <- foreach(i=1:nrow(foregram), .packages=c('data.table')) %dopar% {
    current <- foregram[i]
    id_w123 <- current$back_id
    count_w123 <- trigram[id==id_w123, count]
    count_w1234 <- current$count
    count_w1234/count_w123
}

stopCluster(cl)    
PML4 <- unlist(x)
save(PML4, file="Data/PML4.RData")
t_end <- Sys.time()
t_end-t_start