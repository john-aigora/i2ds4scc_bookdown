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

  # Remove
sensory %>% 
  filter(!is.na(Sour))

sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  filter(!is.na(Scores)) %>% 
  group_by(Product, Variables) %>% 
  summarize(Means = mean(Scores)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Variables, values_from = Means) %>% 
  dplyr::select(Product, Sour, Light)

sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  filter(!is.na(Scores),
         Variables %in% c("Light","Sour")) %>%
  group_by(Product, Variables) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from=Variables, values_from=n)

sensory_long <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores")

(attr_rmv <- sensory_long %>% 
    filter(is.na(Scores)) %>% 
    pull(Variables) %>% 
    unique())

sensory_clean <- sensory_long %>% 
  filter(!(Variables %in% attr_rmv)) %>% 
  pivot_wider(names_from=Variables, values_from=Scores)

  # Imputation
sensory %>% 
  replace_na(list(Sour = 888, Light = 999)) %>% 
  dplyr::select(Judge, Product, Sour, Light)

prod_mean <- sensory_long %>% 
  group_by(Product, Variables) %>% 
  summarize(Mean = mean(Scores, na.rm=TRUE)) %>% 
  ungroup()

sensory_long %>% 
  full_join(prod_mean, by=c("Product","Variables")) %>% 
  mutate(Scores = ifelse(is.na(Scores), Mean, Scores)) %>% 
  dplyr::select(-"Mean") %>% 
  pivot_wider(names_from=Variables, values_from=Scores) %>% 
  dplyr::select(Judge, Product, Sour, Light)

library(simputation)
sensory %>% 
  impute_lm(Sour + Light ~ Product) %>% 
  dplyr::select(Judge, Product, Sour, Light)


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
