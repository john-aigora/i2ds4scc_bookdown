library(tidyverse)
library(readxl)
library(here)

# Sensory Data ------------------------------------------------------------

file_path <- here("data","Sensory Profile (NA).xlsx") 
p_info <- read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

# Data Exploration --------------------------------------------------------

sensory
summary(sensory)
glimpse(sensory)

library(skimr)
skim(sensory)

library(DataExplorer)
create_report(sensory)

# Missing Values ----------------------------------------------------------

  # Visualize and Detect
library(visdat)
sensory %>% 
  vis_miss()

sensory %>% 
  split(.$Product) %>% 
  map(function(data){
    vis_miss(data)
  })

library(naniar)
sensory %>%
  gg_miss_upset()

ggplot(sensory, aes(x=Product, y=Sour))+
  geom_miss_point()

  # Imputation
senso_long <- sensory %>%
  pivot_longer(-c("Judge","Product","Protein","Fiber"), names_to="Attribute", values_to="Score")

prod_mean <- senso_long %>% 
  group_by(Product, Attribute) %>% 
  summarize(ProdMean = mean(Score, na.rm=TRUE)) %>% 
  ungroup()

senso_long %>% 
  full_join(prod_mean, by=c("Product","Attribute")) %>% 
  mutate(Score2 = ifelse(is.na(Score), ProdMean, Score)) %>% 
  dplyr::select(-c("Score","ProdMean")) %>% 
  pivot_wider(names_from=Attribute, values_from=Score2) %>% 
  summary(.)

library(simputation)

senso_long %>% 
  split(.$Attribute) %>% 
  map(function(data){
    
    if (any(is.na(data$Score))){
      data %>% 
        dplyr::select(-Attribute) %>% 
        impute_lm(Score ~ Product + Judge)
    } else {
      data %>% 
        dplyr::select(-Attribute)
    }
    
  }) %>% 
  enframe(name = "Attribute", value = "res") %>% 
  unnest(res) %>% 
  pivot_wider(names_from=Attribute, values_from=Score) %>% 
  summary(.)

library(missMDA)

imputePCA(sensory, quali.sup=1:4)$completeObs %>% 
  summary(.)

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
