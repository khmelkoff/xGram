library(shiny)
library(data.table)

#options(shinyapps.locale='ru_RU')

### load the data
load("Data/unigram.RData")
load("Data/bigram.RData")
load("Data/trigram.RData")
load("Data/foregram.RData")
load("Data/suffix.RData")

text <- "" # query

start_words <- c("I", "The", "We", "It", "But", "And", "You", "This",
                 "If", "So", "A", "It's", "I'm", "He","She", "In",
                 "Thanks", "What", "They", "My")

words <- sample(start_words, 5, replace=FALSE) # prediction

add_words <- c(3, 7, 1, 2, 9)

unk_id <- 59819

## clear and preprocess query
clear <- function(query) {
    query <- tolower(query)
    #query <- gsub("'","_", query)
    query <- gsub("([[:punct:]])\\1{1,}", "\\1", query)
    query <- gsub("[;?!]", ".", query)
    query <- gsub("[^a-z -\\.']", "", query) ##  not work properly ?? 
    query <- gsub("\\.", " .", query)
    query <- gsub("-\\s", " ", query)
    query <- gsub("\\s-", " ", query)
    query <- unlist(strsplit(query, split=" "))
    query <- query[query!=""]
    
    for (i in 1:length(query)) {
        if (grepl("'", query[i])) {
            if (query[i] %in% suffix) {
              query[i] <- gsub("'", "_", query[i])
            } else {
              query[i] <- gsub("'", " ", query[i])
              query <- unlist(strsplit(query, split=" "))
            }
        } 
    }
    return(query)
}

## predict next word (PKN + back-off)

predict <- function(query) {
    
    query <- clear(query)
    size <- length(query)
    
    if (size==1) {
        
        w1 <- query
        
        res1 <- unigram[word==w1,] # search in unigrams
        if (nrow(res1)==0) {
            w1 <- "<unk>"
            res1 <- unigram[word==w1,]
        }
        
        id1 <- res1$id
        res2 <- bigram[back_id==id1,] # search in bigrams
        res2 <- res2[word!=unk_id,]
        
        res2 <- res2[order(-p),]
        
        return(res2)
        
    } # end size 1
    
    if (size==2) { # predict by trigram #######################################
                   
                   
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
                   
                   res2 <- bigram[word==id12 & back_id==id11,] # search in bigrams
                   id2 <- res2$id
                   
                   res3 <- trigram[back_id==id2,] # search in trigrams
                   res3 <- res3[word!=unk_id,]
                   
                   
                   if (nrow(res3)<5) {
                       res2 <- bigram[back_id==id12,] # search in bigrams
                       res2 <- res2[word!=unk_id,]
                       
                       res <- rbind(res3[order(-res3$p),],res2[order(-res2$p),])
                       
                   } else {
                       res <- res3[order(-res3$p),]
                   }
                   
                   return(res)
                   
    } # end size 2
    
    if(size>2) { # predict by foregram ########################################
                 
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
                 res22 <- bigram[word==id12 & back_id==id11,]
                 id22 <- res22$id
                 
                 res23 <- bigram[word==id13 & back_id==id12,]
                 id23 <- res23$id
                 
                 res33 <- trigram[word==id13 & back_id==id22,]
                 id33 <- res33$id
                 
                 res4 <- foregram[back_id==id33,] #3+2+1
                 res4 <- res4[word!=unk_id,]
                 
                 res <- res4[order(-res4$p)]
                 
                 if (nrow(res4)<5) {
                     
                     res3 <- trigram[back_id==id23,] #3+2
                     res3 <- res3[word!=unk_id,]

                     res <- rbind(res4[order(-res4$p),],
                                  res3[order(-res3$p),])
                     
                     if(nrow(res4)+nrow(res3)<5) {
                         
                         res2 <- bigram[back_id==id13,] #3
                         res2 <- res2[word!=unk_id,]
                         
                         res <- rbind(res4[order(-res4$p),],
                                      res3[order(-res3$p),],
                                      res2[order(-res2$p),])
                     }    
                 }
                 
                 return(res)
    } # end size > 2
}

fiveWords <- function(data) {
    w <- data$word
    if (length(unique(data$word)) < 5) {
        w <- c(w, add_words)
    } 
    w <- unique(w)[1:5]
    
    w <- unigram[id==w,word]
    
    w <- gsub("_","'",w)
    return(w)        
}

# Server logic ################################################################
shinyServer(function(input, output) {
    
    # prediction buttons
    output$ui <- renderUI({
        list(
            actionButton("w1Button", words[1]),
            actionButton("w2Button", words[2]),
            actionButton("w3Button", words[3]),
            actionButton("w4Button", words[4]),
            actionButton("w5Button", words[5])
        )
        
    })

    # input box
    output$box <- renderUI({
            list(
            textInput("box", "Just type a word and press spacebar...", value = ""),
            HTML("<small>For evaluation paste a sentense here and click space/submit</small>")
            )
    })    
    
    
    # prediction button events ################################################
    observeEvent(input$w1Button, {
        text <<- paste0(text, words[1], " ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        ### word prediction code
        words <<- fiveWords(predict(text))
        output$ui <- renderUI({
            list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
            )            
        })
    })
    observeEvent(input$w2Button, {
        text <<- paste0(text, words[2], " ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        ### word prediction code
        words <<- fiveWords(predict(text))
        output$ui <- renderUI({
            list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
            )            
        })
    })
    observeEvent(input$w3Button, {
        text <<- paste0(text, words[3], " ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        ### word prediction code        
        words <<- fiveWords(predict(text))
        output$ui <- renderUI({
            list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
            )            
        })
    })
    observeEvent(input$w4Button, {
        text <<- paste0(text, words[4], " ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        ### word prediction code        
        words <<- fiveWords(predict(text))
        output$ui <- renderUI({
            list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
            )            
        })
    })
    
    observeEvent(input$w5Button, {
        text <<- paste0(text, words[5], " ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        ### word prediction code        
        words <<- fiveWords(predict(text))
        output$ui <- renderUI({
            list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
            )            
        })
    })
    
    # symbol button events ####################################################
    observeEvent(input$dotButton, {
        text <<- gsub("\\s+$", "", text) # kill space before dot
        text <<- paste0(text, ". ")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        words <<- sample(start_words, 5, replace=FALSE)
        output$ui <- renderUI({
          list(
            actionButton("w1Button", words[1]),
            actionButton("w2Button", words[2]),
            actionButton("w3Button", words[3]),
            actionButton("w4Button", words[4]),
            actionButton("w5Button", words[5])
          )            
        })        
        
    
        
    })
    observeEvent(input$backButton, {
        len <- nchar(text)
        if (len > 1) {
            text <<- substr(text, 1,len-1)       
            output$box <- renderUI({
                textInput("box", "Just type a word and press spacebar...", value = text)
            })
        }
        if (len == 1) {
            text <<- ""       
            output$box <- renderUI({
                textInput("box", "Just type a word and press spacebar...", value = text)
            })
            words <<- sample(start_words, 5, replace=FALSE)
            output$ui <- renderUI({
              list(
                actionButton("w1Button", words[1]),
                actionButton("w2Button", words[2]),
                actionButton("w3Button", words[3]),
                actionButton("w4Button", words[4]),
                actionButton("w5Button", words[5])
              )            
            })
        }        
    })
    
    observeEvent(input$apoButton, {
        text <<- paste0(text, "'")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    }) 
    
    observeEvent(input$spaceButton, {
        
        len <- nchar(text)
        ending <- substr(text, len, len)
        ending_ <- substr(text, len-1, len)
        ending <- gsub("[[:punct:]]", ".", ending)
        ending_ <- gsub("[[:punct:]]", ".", ending_)
        
        if (len==0) {
            words <<- sample(start_words, 5, replace=FALSE)
            output$ui <- renderUI({
                list(
                    actionButton("w1Button", words[1]),
                    actionButton("w2Button", words[2]),
                    actionButton("w3Button", words[3]),
                    actionButton("w4Button", words[4]),
                    actionButton("w5Button", words[5])
                )            
            })            
            
        } else if (ending=="." | ending_==". ") {
            words <<- sample(start_words, 5, replace=FALSE)

            text <<- paste0(text, " ")       
            output$box <- renderUI({
                textInput("box", "Just type a word and press spacebar...", value = text)
            })
            
            output$ui <- renderUI({
                list(
                    actionButton("w1Button", words[1]),
                    actionButton("w2Button", words[2]),
                    actionButton("w3Button", words[3]),
                    actionButton("w4Button", words[4]),
                    actionButton("w5Button", words[5])
                )            
            })
        } else {
        
            text <<- paste0(text, " ")       
            output$box <- renderUI({
                textInput("box", "Just type a word and press spacebar...", value = text)
            })
            
            words <<- fiveWords(predict(text))
            output$ui <- renderUI({
                list(
                    actionButton("w1Button", words[1]),
                    actionButton("w2Button", words[2]),
                    actionButton("w3Button", words[3]),
                    actionButton("w4Button", words[4]),
                    actionButton("w5Button", words[5])
                )            
            })
        }
        
    })
    
    observeEvent(input$clearButton, { ### clear
        text <<- ""       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
        words <<- sample(start_words, 5, replace=FALSE)
        output$ui <- renderUI({
          list(
            actionButton("w1Button", words[1]),
            actionButton("w2Button", words[2]),
            actionButton("w3Button", words[3]),
            actionButton("w4Button", words[4]),
            actionButton("w5Button", words[5])
          )            
        })
    })

    observeEvent(input$box, {
        text <<- input$box
    })    
    
    # alphabet button events ################################################## 
    observeEvent(input$aButton, {
        text <<- paste0(text, "a")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })

    })
    observeEvent(input$bButton, {
        text <<- paste0(text, "b")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$cButton, {
        text <<- paste0(text, "c")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })    
    observeEvent(input$dButton, {
        text <<- paste0(text, "d")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$eButton, {
        text <<- paste0(text, "e")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$fButton, {
        text <<- paste0(text, "f")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$gButton, {
        text <<- paste0(text, "g")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$hButton, {
        text <<- paste0(text, "h")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$iButton, {
        text <<- paste0(text, "i")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$jButton, {
        text <<- paste0(text, "j")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$kButton, {
        text <<- paste0(text, "k")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$lButton, {
        text <<- paste0(text, "l")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$nButton, {
        text <<- paste0(text, "n")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$mButton, {
        text <<- paste0(text, "m")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$oButton, {
        text <<- paste0(text, "o")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$pButton, {
        text <<- paste0(text, "p")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$qButton, {
        text <<- paste0(text, "q")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$rButton, {
        text <<- paste0(text, "r")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$sButton, {
        text <<- paste0(text, "s")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$tButton, {
        text <<- paste0(text, "t")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$uButton, {
        text <<- paste0(text, "u")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$vButton, {
        text <<- paste0(text, "v")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$wButton, {
        text <<- paste0(text, "w")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$xButton, {
        text <<- paste0(text, "x")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$yButton, {
        text <<- paste0(text, "y")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
    observeEvent(input$zButton, {
        text <<- paste0(text, "z")       
        output$box <- renderUI({
            textInput("box", "Just type a word and press spacebar...", value = text)
        })
    })
})