library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)
library(shinythemes)

Unigrams <- read_rds("tb_unigrams_fin.rds")
Bigrams <- read_rds("tb_bigrams_fin.rds")
Trigrams <- read_rds("tb_trigrams_fin.rds")

search3grams <- function(tkns){
        
        tknsIndexed <- tkns %>%
                left_join(Unigrams %>% select(word, index), by = "word") %>%
                select(index) %>%
                tail(2) %>%
                add_column(wordsIn = c("word1", "word2")) %>%
                spread(key = wordsIn, value = index) %>%
                mutate(
                        word1 = ifelse(is.na(word1), 500000, word1),
                        word2 = ifelse(is.na(word2), 500000, word2)
                        )
        
        rezIndexed <- tknsIndexed %>%
                select(word1, word2) %>%
                inner_join(Trigrams, by = c("word1", "word2"))
        
        rezWord <- rezIndexed %>%
                select(word3, score) %>%
                inner_join(Unigrams %>% select(word, index), by = c("word3" = "index")) %>%
                select(word, score) %>%
                arrange(desc(score))
        
        rezWord 
}

search2grams <- function(tkns){
        
        tknsIndexed <- tkns %>%
                left_join(Unigrams %>% select(word, index), by = "word") %>%
                select(index) %>%
                tail(1) %>%
                add_column(wordsIn = c("word1")) %>%
                spread(key = wordsIn, value = index) %>%
                mutate(
                        word1 = ifelse(is.na(word1), 500000, word1)
                )
        
        rezIndexed <- tknsIndexed %>%
                select(word1) %>%
                inner_join(Bigrams, by = c("word1"))
        
        rezWord <- rezIndexed %>%
                select(word2, score) %>%
                inner_join(Unigrams %>% select(word, index), by = c("word2" = "index")) %>%
                select(word, score) %>%
                arrange(desc(score))
        
        rezWord 
}

search1grams <- function(tkns){
        
        rezWord <- Unigrams %>%
                select(word, score) %>%
                arrange(desc(score))
        
        rezWord 
}


ui <- fluidPage(theme = shinytheme("slate"),
   
   titlePanel("PrediComplete"),
   
   hr(),
   
   textAreaInput("text", label = h3("Try me:")),
   
   uiOutput("buttons"),
   
   hr()
)

server <- function(session, input, output) {
        
        rv <- reactiveValues(is_space = TRUE, ngrams = 0, tkns = NULL, pred = NULL, tb = NULL, type = "")
        
        observeEvent(input$text, {
                
                withProgress(message = "Working", value = 50, {
                
                try3grams <- function(rv, tkns){
                        if((rv$ngrams > 2) & (!rv$is_space)){
                                
                                rv$pred <- search3grams(tkns %>% tail(3) %>% head(2))
                                rv$tb <- rv$pred %>%
                                        filter(str_detect(word, str_c("^", tkns %>% tail(1)))) %>%
                                        top_n(5)
                        }
                        else if ((rv$ngrams >= 2) & (rv$is_space)){
                                rv$pred <- search3grams(tkns %>% tail(2))
                                rv$tb <- rv$pred %>%
                                        top_n(5)
                               
                        }
                       
                        rv$tb
                }
                
                try2grams <- function(rv, tkns){
                                
                                if(rv$ngrams > 1){
                                       
                                        rv$pred <- search2grams(tkns %>% tail(2) %>% head(1))
                                        rv$tb <- rv$pred %>%
                                                filter(str_detect(word, str_c("^", tkns %>% tail(1)))) %>%
                                                top_n(5)
                                }
                                else{
                                        rv$pred <- search2grams(tkns %>% tail(1))
                                        rv$tb <- rv$pred %>%
                                                top_n(5)
                                }
                                rv$tb
                }
                
                try1grams <- function(rv, tkns){
                        
                                rv$tkns <- tkns
                                rv$pred <- search1grams(tkns)
                                rv$tb <- rv$pred %>%
                                        filter(str_detect(word, str_c("^", tkns %>% tail(1)))) %>%
                                        top_n(5)     
                                rv$tb
                }
                             
                tb <- as.tibble(input$text)
                             s <- str_count(tb, "\\s")
                             
                             rv$is_space <- str_detect(tb, "\\s+$")
                             
                             tkns <- tb %>%
                                     unnest_tokens(word, value)  
                             
                             rv$ngrams <- nrow(tkns)
                             
                             if (
                                     ((rv$ngrams == 2) & (rv$is_space)) |
                                     ((rv$ngrams > 2))
                                ){
                                     
                                     try3grams(rv, tkns)
                                     if(nrow(rv$tb) > 0){
                                             rv$type = "Tri-gram match found"
                                     }else{
                                             try2grams(rv, tkns)
                                             if(nrow(rv$tb) > 0){
                                                     rv$type = "Tri to Bi-gram match found"
                                             }else{
                                                     try1grams(rv, tkns)
                                                     rv$type = "Tri to Bi to Uni-gram guess"
                                             }
                                     }
                             } else if(
                                        ((rv$ngrams == 1) & (rv$is_space)) |
                                        ((rv$ngrams == 2) & (!rv$is_space))
                                     ){
                                     try2grams(rv, tkns)
                                     
                                     if(nrow(rv$tb) > 0){
                                             rv$type = "Bi-gram match found"
                                     }else{
                                             try1grams(rv, tkns)
                                             rv$type = "Bi to Uni-gram guess"
                                     }  
                             } else if(
                                        (rv$ngrams == 1) & (!rv$is_space)
                                     ){
                                     try1grams(rv, tkns)
                                     rv$type = "Uni-gram guess"
                             } else if(rv$ngrams == 0){
                                     rv$pred <- NULL
                                     rv$tb <- NULL
                                     rv$type <- NULL
                                     rv$tkns <- NULL
                             }
                     })
        })
        
        output$value <- renderPrint({ 
                input$text 
        })
        
        output$spaces <- renderPrint({
                str_count(input$text, "\\s")
        })
        
        output$grams <- renderPrint({
                rv$ngrams
        })
        
        output$intext <- renderTable({
                rv$pred
        })
        
        output$predics <- renderPrint({
                paste0("Predictions: ", nrow(rv$pred))
        })
        
        output$tb <- renderTable({
                rv$tb
        })
        
        output$nbrrez <- renderPrint({
                paste0("Results: ", nrow(rv$tb))
        })
        
        output$gramtype <- renderPrint({
                rv$type
        })
        
        output$rvisspace <- renderPrint({
                rv$is_space
        })
        
        output$rvtkns <- renderTable({
                rv$tkns
        })
        
        output$buttons <- renderUI({
                if (is.null(rv$tb)) {
                        return(helpText("No matches"))
                } else if(nrow(rv$tb) == 0){
                        return(helpText("No matches"))
                }
                
                btnlist <- list()
                
                for(i in seq_along(rv$tb$word)){
        
                                btnlist[[i]] <- actionButton(paste0("btn",i), rv$tb$word[i])
                }
                head(btnlist, 5)
        })
        
        observeEvent(input$btn1, {
                
                oldtxt <- as.tibble(input$text) %>%
                                unnest_tokens(word, value, to_lower = FALSE)
                
                newtxt <- as.tibble()
                
                if(!rv$is_space){
                        
                        newtxt <- oldtxt %>%
                                mutate(index = row_number()) %>%
                                mutate(word = ifelse(row_number() == nrow(.), rv$tb$word[1], word)) 
                        
                }else{
                        newtxt <- oldtxt %>%
                                add_row(word = rv$tb$word[1]) %>%
                                filter(!str_detect(word, " "))
                }
                
                newtxt <- paste0(newtxt$word, collapse = " ")
                
                updateTextInput(session, inputId = "text", value = paste0(newtxt, " ")) 
        })
        
        observeEvent(input$btn2, {
                oldtxt <- as.tibble(input$text) %>%
                        unnest_tokens(word, value, to_lower = FALSE)
                
                newtxt <- as.tibble()
                
                if(!rv$is_space){
                        
                        newtxt <- oldtxt %>%
                                mutate(index = row_number()) %>%
                                mutate(word = ifelse(row_number() == nrow(.), rv$tb$word[2], word)) 
                        
                }else{
                        newtxt <- oldtxt %>%
                                add_row(word = rv$tb$word[2]) %>%
                                filter(!str_detect(word, " "))
                }
                
                newtxt <- paste0(newtxt$word, collapse = " ")
                
                updateTextInput(session, inputId = "text", value = paste0(newtxt, " ")) 
        })
        
        observeEvent(input$btn3, {
                oldtxt <- as.tibble(input$text) %>%
                        unnest_tokens(word, value, to_lower = FALSE)
                
                newtxt <- as.tibble()
                
                if(!rv$is_space){
                        
                        newtxt <- oldtxt %>%
                                mutate(index = row_number()) %>%
                                mutate(word = ifelse(row_number() == nrow(.), rv$tb$word[3], word)) 
                        
                }else{
                        newtxt <- oldtxt %>%
                                add_row(word = rv$tb$word[3]) %>%
                                filter(!str_detect(word, " "))
                }
                
                newtxt <- paste0(newtxt$word, collapse = " ")
                
                updateTextInput(session, inputId = "text", value = paste0(newtxt, " ")) 
        })
        
        observeEvent(input$btn4, {
                oldtxt <- as.tibble(input$text) %>%
                        unnest_tokens(word, value, to_lower = FALSE)
                
                newtxt <- as.tibble()
                
                if(!rv$is_space){
                        
                        newtxt <- oldtxt %>%
                                mutate(index = row_number()) %>%
                                mutate(word = ifelse(row_number() == nrow(.), rv$tb$word[4], word)) 
                        
                }else{
                        newtxt <- oldtxt %>%
                                add_row(word = rv$tb$word[4]) %>%
                                filter(!str_detect(word, " "))
                }
                
                newtxt <- paste0(newtxt$word, collapse = " ")
                
                updateTextInput(session, inputId = "text", value = paste0(newtxt, " ")) 
        })
        
        observeEvent(input$btn5, {
                oldtxt <- as.tibble(input$text) %>%
                        unnest_tokens(word, value, to_lower = FALSE)
                
                newtxt <- as.tibble()
                
                if(!rv$is_space){
                        
                        newtxt <- oldtxt %>%
                                mutate(index = row_number()) %>%
                                mutate(word = ifelse(row_number() == nrow(.), rv$tb$word[5], word)) 
                        
                }else{
                        newtxt <- oldtxt %>%
                                add_row(word = rv$tb$word[5]) %>%
                                filter(!str_detect(word, " "))
                }
                
                newtxt <- paste0(newtxt$word, collapse = " ")
                
                updateTextInput(session, inputId = "text", value = paste0(newtxt, " ")) 
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

