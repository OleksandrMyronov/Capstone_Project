library(dplyr)
library(tidytext)
#==========================================
library(dplyr)
library(sbo)
library(tidytext)
library(widyr)

ns<-sample(readLines("en_US/en_US.news.txt", encoding = 'UTF-8'), size=18000)
bs<-sample(readLines("en_US/en_US.blogs.txt", encoding = 'UTF-8'), size=160000)
ts<-sample(readLines("en_US/en_US.twitter.txt", encoding = 'UTF-8'), size=320000)
append(ns, bs) %>% append(ts) -> cfile
p <- sbo_predtable(object = cfile,
                   N = 3, # Train a 3-gram model
                   dict = target ~ 0.85, # cover 85% of training corpus
                   .preprocess = sbo::preprocess, # Preprocessing transformation 
                   EOS = ".?!:;", # End-Of-Sentence tokens
                   lambda = 0.4, # Back-off penalization in SBO algorithm
                   L = 15L, # Number of predictions for input
                   filtered = c("<UNK>", "<EOS>") # Exclude the <UNK> token from predictions
)
#========================================================
saveRDS(p, "./ngram_model.rds")
p <- readRDS("./ngram_model.rds")
#============================= creating sample ========================
ns<-sample(readLines("en_US/en_US.news.txt", encoding = 'UTF-8'), size=9000)
bs<-sample(readLines("en_US/en_US.blogs.txt", encoding = 'UTF-8'), size=80000)
ts<-sample(readLines("en_US/en_US.twitter.txt", encoding = 'UTF-8'), size=160000)
append(ns, bs) %>% append(ts) -> cfile
cfile<-gsub('[0-9]','',cfile)
cfile<-gsub('  ',' ',cfile)
cfile<-gsub('  ',' ',cfile)
dict<-dictionary(cfile, target=0.8)
#============================creating correlation base================================
cfile<-tibble(cfile)
cfile %>% cbind(TextBlock=row.names(cfile)) %>%
unnest_tokens(word, cfile) %>%
filter(!word %in% stop_words$word) %>%
filter(word %in% dict) %>%
pairwise_cor(word, TextBlock, sort=T) %>%
saveRDS(p, "./correlation_model.rds")
#=============================correlation algorithm ==============================
readRDS("./correlation_model.rds") %>% 
filter(item1 %in% Wordlist$word) %>%
group_by(item2) %>%
summarize(correlation=sum(correlation)) %>%
arrange(desc(correlation)) %>%
head(15)$item2 ->ns10
  