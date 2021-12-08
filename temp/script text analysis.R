library(tidyverse)
library(readxl)

library(tidytext)

library(here)
file_path <- here("data","cider_data.xlsx") 

cider <- read_xlsx(file_path) %>% 
  mutate(sample = as.character(sample))

  # Tokenization
cider <- cider %>% 
  unnest_tokens(tokens, comments, token="regex", pattern="[;|,|:|.|/]", to_lower=FALSE)

cider <- cider %>% 
  mutate(tokens = str_to_lower(tokens)) %>% 
  mutate(tokens = str_trim(tokens)) %>% 
  mutate(tokens = str_squish(tokens)) %>% 
  mutate(tokens = str_remove_all(tokens, pattern="[(|)|?|!]")) %>% 
  mutate(tokens = str_remove_all(tokens, pattern="[ó|ò]")) %>% 
  mutate(tokens = str_replace_all(tokens, pattern="õ", replacement="'"))

  # Stop Words
cider <- cider %>% 
  relocate(subject, .before=sample) %>% 
  group_by(subject, sample) %>% 
  mutate(num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(tokens, tokens, token="regex", pattern=" ")

cider %>%
  count(tokens) %>%
  arrange(desc(n))

    ## Creating the List
library(stopwords)
stopword_list <- stopwords(source="snowball")

word_list <- cider %>% 
  count(tokens) %>% 
  pull(tokens)

intersect(stopword_list, word_list)
word_list[!word_list %in% stopword_list]

stopword_list <- stopword_list[!stopword_list %in% c("off","no","not","too","very")]
stopword_list <- c(stopword_list, c("accompany","amount","anything","considering","despite","expected",
                                    "just","like","neither","one","order","others","products",
                                    "sample","seems","something","thank","think","though","time","way",
                                    "-"))

stopword_list[order(stopword_list)]
    
    ## Cleaning the Data
cider <- cider %>% 
  anti_join(tibble(tokens = stopword_list), by="tokens")

(cider %>% 
  count(tokens) %>% 
  arrange(desc(n)))

  ## Stemming
library(SnowballC)
cider <- cider %>% 
  mutate(stem = wordStem(tokens))

cider %>% 
  count(stem) %>% 
  arrange(desc(n))
  
  ## Lemmatization
library(spacyr)

spacy_initialize(entity=FALSE)

lemma <- spacy_parse(cider$tokens) %>% 
  as_tibble() %>% 
  dplyr::select(tokens=token, lemma) %>% 
  unique()

cider <- full_join(cider, lemma, by="tokens")

cider %>% 
  count(lemma) %>% 
  filter(lemma %in% c("moldy","rotten"))

cider %>% 
  mutate(lemma = str_replace(lemma, "moldy", "rotten")) %>% 
  count(lemma) %>% 
  filter(lemma %in% c("moldy","rotten"))

new_list <- read_xlsx("temp/Example of word grouping.xlsx")
cider <- cider %>% 
  full_join(new_list, by="lemma") %>% 
  mutate(lemma = ifelse(is.na(`new name`), lemma, `new name`)) %>% 
  dplyr::select(-`new name`)
