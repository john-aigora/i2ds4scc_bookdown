library(tidyverse)
library(readxl)
library(here)
library(tidytext)

file_path <- here("data","cider_text_data.xlsx") 
cider_og <- read_xlsx(file_path) %>% 
  mutate(sample = as.character(sample))

# Tokenization ------------------------------------------------------------

cider <- cider_og %>% 
  unnest_tokens(tokens, comments, token="regex", pattern="[;|,|:|.|/]", to_lower=FALSE)

# Cleaning ----------------------------------------------------------------

  ## First Clean
cider <- cider %>% 
  mutate(tokens = str_to_lower(tokens)) %>% 
  mutate(tokens = str_trim(tokens)) %>% 
  mutate(tokens = str_squish(tokens)) %>% 
  mutate(tokens = str_remove_all(tokens, pattern="[(|)|?|!]")) %>% 
  mutate(tokens = str_remove_all(tokens, pattern="[ó|ò]")) %>% 
  mutate(tokens = str_replace_all(tokens, pattern="õ", replacement="'"))

  ## Further Clean
cider <- cider %>% 
  relocate(subject, .before=sample) %>% 
  group_by(subject, sample) %>% 
  mutate(num = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(tokens, tokens, token="regex", pattern=" |-")

cider %>%
  count(tokens) %>%
  arrange(desc(n))

  ## Stop Words
library(stopwords)
length(stopwords(source="snowball"))
length(stopwords(source="stopwords-iso"))

stopword_list <- stopwords(source="snowball")

word_list <- cider %>% 
  count(tokens) %>% 
  pull(tokens)

intersect(stopword_list, word_list)
stopword_list <- stopword_list[!stopword_list %in% c("off","no","not","too","very")]

word_list[!word_list %in% stopword_list]
stopword_list <- c(stopword_list, c("accompany","amount","anything","considering","despite","expected",
                                    "just","like","neither","one","order","others","products",
                                    "sample","seems","something","thank","think","though","time","way"))

stopword_list[order(stopword_list)]
    
cider <- cider %>% 
  anti_join(tibble(tokens = stopword_list), by="tokens")

cider %>% 
  count(tokens) %>% 
  arrange(desc(n))

  ## Stemming
library(SnowballC)
cider <- cider %>% 
  mutate(stem = wordStem(tokens))

cider %>% 
  count(stem)
  
  ## Lemmatization
library(spacyr)

spacy_initialize(entity=FALSE)
lemma <- spacy_parse(cider$tokens) %>% 
  as_tibble() %>% 
  dplyr::select(tokens=token, lemma) %>% 
  unique()
cider <- full_join(cider, lemma, by="tokens")

cider %>% 
  count(lemma)

  ## Manual grouping
cider %>% 
  count(lemma) %>% 
  filter(lemma %in% c("moldy","rotten"))

cider %>% 
  mutate(lemma = str_replace(lemma, "moldy", "rotten")) %>% 
  count(lemma) %>% 
  filter(lemma %in% c("moldy","rotten"))

new_list <- read_xlsx("data/Example of word grouping.xlsx")
cider <- cider %>% 
  full_join(new_list, by="lemma") %>% 
  mutate(lemma = ifelse(is.na(`new name`), lemma, `new name`)) %>% 
  dplyr::select(-`new name`)

# Data Analysis -----------------------------------------------------------

  ## Visualization (Overall)
cider %>% 
  group_by(lemma) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  filter(n>=10, !is.na(lemma)) %>% 
  ggplot(aes(x=reorder(lemma, n), y=n))+
  geom_col()+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(axis.line = element_line(colour="grey80"))+
  coord_flip()+
  ggtitle("List of words mentioned at least 10 times")

  ## Contingency Table
cider %>% 
  filter(!is.na(lemma), !is.na(sample)) %>% 
  group_by(sample, lemma) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from=lemma, values_from=n, values_fill=0)

prod_term <- cider %>% 
  filter(!is.na(lemma), !is.na(sample)) %>% 
  group_by(sample, lemma) %>% 
  count() %>% 
  ungroup() %>% 
  split(.$sample) %>% 
  map(function(data){
    data %>% 
      arrange(desc(n)) %>% 
      filter(n>=5) %>% 
      ggplot(aes(x=reorder(lemma, n), y=n))+
      geom_col()+
      theme_minimal()+
      xlab("")+
      ylab("")+
      theme(axis.line = element_line(colour="grey80"))+
      coord_flip()+
      ggtitle(paste0("List of words mentioned at least 5 times for ", 
                     data %>% pull(sample) %>% unique()))
  })

  ## Correspondence Analysis
cider_ct <- cider %>% 
  filter(!is.na(lemma), !is.na(sample)) %>% 
  group_by(sample, lemma) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 5) %>% 
  pivot_wider(names_from=lemma, values_from=n, values_fill=0) %>% 
  as.data.frame() %>% 
  column_to_rownames(var="sample")

library(FactoMineR)
cider_CA <- CA(cider_ct)

  ## Wordclouds
cider_wc <- cider %>% 
  filter(!is.na(lemma), !is.na(sample)) %>% 
  group_by(sample, lemma) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 5)

library(ggwordcloud)
ggplot(cider_wc, aes(x=sample, colour=sample, label=lemma, size=n))+
  geom_text_wordcloud(eccentricity = 2.5)+
  xlab("")+
  theme_minimal()

cider_wc %>% 
  group_by(lemma) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(colour = prop < 1/6, label=lemma, size=n, angle_group = prop < 1/6))+
  geom_text_wordcloud(eccentricity = 2.5)+
  xlab("")+
  theme_minimal()+
  facet_wrap(~sample)

  ## n-grams
cider_2grams <- cider_og %>% 
  unnest_tokens(bigrams, comments, token="ngrams", n=2)

cider_2grams %>% 
  count(bigrams) %>% 
  arrange(desc(n))

cider_2grams %>% 
  group_by(sample) %>% 
  count(bigrams) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  filter(sample == "182")
