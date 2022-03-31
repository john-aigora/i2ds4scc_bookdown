library(tidyverse)
library(skimr)
library(readxl)
library(here)

# Sensory Data ------------------------------------------------------------

file_path <- here("data","Sensory Profile.xlsx") 
p_info <- read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

# Data Preparation --------------------------------------------------------

sensory
skim_without_charts(sensory)

library("report")


# Design Evaluation -------------------------------------------------------

library(tidyverse)
library(SensoMineR)

data(chocolates)

dataset <- sensochoc %>% 
  as_tibble() %>% 
  mutate(across(c(Panelist, Session, Rank, Product), as.character))

# Presentation Order
dataset %>% 
  group_by(Product) %>% 
  count(Rank) %>% 
  ungroup() %>% 
  pivot_wider(names_from=Rank, values_from=n)

xtabs(~ Product+Rank, sensochoc)

# Carry-Over
current <- dataset %>% 
  dplyr::select(Panelist, Product, Session, Rank) %>% 
  mutate(Rank = as.numeric(Rank))

previous <- current %>% 
  rename(Previous = Product) %>% 
  mutate(Rank = Rank + 1) %>% 
  filter(Rank <= length(unique(dataset$Product)))

(cur_prev <- current %>% 
  left_join(previous, by=c("Panelist", "Session", "Rank")))

cur_prev %>% 
  group_by(Session, Product, Previous) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Product = factor(Product, levels=paste0("choc",1:6)),
         Previous = factor(Previous, levels=c("NA",paste0("choc",1:6)))) %>%
  arrange(Previous) %>% 
  pivot_wider(names_from=Previous, values_from=n) %>% 
  arrange(Product) %>% 
  split(.$Session)

