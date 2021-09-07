library(dplyr)
library(sbo)
library(tidytext)
library(widyr)
#=================== creating trigram database =================================
set.seed(2021)
ns<-sample(readLines("en_US/en_US.news.txt", encoding = 'UTF-8'), size=18000)
bs<-sample(readLines("en_US/en_US.blogs.txt", encoding = 'UTF-8'), size=160000)
ts<-sample(readLines("en_US/en_US.twitter.txt", encoding = 'UTF-8'), size=320000)
append(ns, bs) %>% append(ts) %>%                  # combining samples
gsub(pattern='[0-9]',replacement='') %>%           # deleting numbers
gsub(pattern='  ',replacement=' ') -> cfile        # deleting double spaces and saving
cfile %>% 
  sbo_predtable( N = 3,                            # train a 3-gram model
                 dict = target ~ 0.85,             # cover 85% of training corpus
                 .preprocess = sbo::preprocess,    # preprocessing transformation 
                 EOS = ".?!:;",                    # End-Of-Sentence tokens
                 lambda = 0.4,                     # back-off penalization in SBO algorithm
                 L = 10L,                          # number of predictions for input
                 filtered=c("<UNK>", "<EOS>")) %>% # exclude <UNK> and <EOS> from predictions
saveRDS("./Word_Prediction/trigram10_model.rds")   # saving predtable to RDS file
dict<-dictionary(cfile, target=0.85)               # creating dictonary with 80% covering
#============================= creating correlation base =======================
cfile<-tibble(cfile)                               # converting to tibble
cfile %>% cbind(TextBlock=row.names(cfile)) %>%    # adding TextBlock index
unnest_tokens(word, cfile) %>%                     # tokenizing
filter(!word %in% stop_words$word) %>%             # filtering non stop_words
filter(word %in% dict) %>%                         # filtering words in dictionary
pairwise_cor(word, TextBlock, sort=T) %>%          # calculating word correlation
filter(correlation>=0.01) %>%                      # filtering more than 0.01 positive correlation
saveRDS("./Word_Prediction/correlation_model.rds") # saving to RDS file
