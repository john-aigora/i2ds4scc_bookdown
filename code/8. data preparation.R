library(tidyverse)
library(readxl)
library(here)

file_path <- here("data","biscuits_sensory_profile_with_NA.xlsx")
sensory <- read_xlsx(file_path, sheet="Data")

# Inspect -----------------------------------------------------------------

  ## Exploration
sensory
summary(sensory)
glimpse(sensory)

library(skimr)
skim(sensory)

library(DataExplorer)
create_report(sensory)

# Missing Values ----------------------------------------------------------

  ## Visualize and Detect
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

  ## Ignoring Missing Values
broom::tidy(aov(Light ~ Product + Judge, data=sensory))

sensory %>% 
  group_by(Product) %>% 
  summarise(Light = mean(Light)) %>% 
  ungroup()

  ## Removing Missing Values

    ### Filtering
sensory %>% 
  filter(!is.na(Sour))

    ### na.rm=TRUE
sensory %>% 
  group_by(Product) %>% 
  summarise(Light = mean(Light, na.rm=TRUE)) %>% 
  ungroup()

    ### pivot_wider
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  filter(!is.na(Scores)) %>% 
  group_by(Product, Variables) %>% 
  summarize(Means = mean(Scores)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Variables, values_from = Means) %>% 
  dplyr::select(Product, Sour, Light)

    ### Unbalanced Data
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  filter(!is.na(Scores),
         Variables %in% c("Light","Sour")) %>%
  group_by(Product, Variables) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from=Variables, values_from=n)

    ### Remove block of data
sensory_long <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores")

(attr_rmv <- sensory_long %>% 
    filter(is.na(Scores)) %>% 
    pull(Variables) %>% 
    unique())

sensory_clean <- sensory_long %>% 
  filter(!(Variables %in% attr_rmv)) %>% 
  pivot_wider(names_from=Variables, values_from=Scores)

  ## Imputating Missing Values

    ### Replacing with a fixed value
sensory %>% 
  replace_na(list(Sour = 888, Light = 999)) %>% 
  dplyr::select(Judge, Product, Sour, Light)

    ### Replacing with the mean by product
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

    ### Using  a linear model
library(simputation)
sensory %>% 
  impute_lm(Sour + Light ~ Product) %>% 
  dplyr::select(Judge, Product, Sour, Light)

    ### Multivariate imputation
library(missMDA)
imputePCA(sensory, quali.sup=1:4, method="EM")$completeObs %>% 
  dplyr::select(Judge, Product, Sour, Light)

# Design Evaluation -------------------------------------------------------

library(SensoMineR)
data(chocolates)

dataset <- sensochoc %>% 
  as_tibble() %>% 
  mutate(across(c(Panelist, Session, Rank, Product), as.character))

  ## Presentation Order
xtabs(~ Product+Rank, sensochoc)

dataset %>% 
  group_by(Product) %>% 
  count(Rank) %>% 
  ungroup() %>% 
  pivot_wider(names_from=Rank, values_from=n)

  ## Carry-Over
current <- dataset %>% 
  dplyr::select(Panelist, Product, Session, Rank) %>% 
  mutate(Rank = as.numeric(Rank))

previous <- current %>% 
  rename(Previous = Product) %>% 
  mutate(Rank = Rank + 1) %>% 
  filter(Rank <= length(unique(dataset$Product)))

cur_prev <- current %>% 
  left_join(previous, by=c("Panelist", "Session", "Rank"))

cur_prev %>% 
  group_by(Session, Product, Previous) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Product = factor(Product, levels=paste0("choc",1:6)),
         Previous = factor(Previous, levels=c("NA",paste0("choc",1:6)))) %>%
  arrange(Previous) %>% 
  pivot_wider(names_from=Previous, values_from=n, values_fill=0) %>% 
  arrange(Product) %>% 
  split(.$Session)

# Clean -------------------------------------------------------------------

file_path <- here("Data", "biscuits_traits.xlsx")

demo_var <- read_xlsx(file_path, sheet="Variables") %>% 
  dplyr::select(Code, Name)
demo_lev <- read_xlsx(file_path, sheet="Levels") %>% 
  dplyr::select(Question, Code, Levels) %>% 
  inner_join(demo_var, by=c("Question"="Code")) %>% 
  dplyr::select(-Question)
demographic <- read_xlsx(file_path, sheet="Data", skip=1, col_names=unlist(demo_var$Name))

  ## Data Types
demographic
str(demographic[,1:5])

    ### Binary
x <- runif(10, 1, 10)
test <- x>5
sum(test)
  
    ### Character and Factor
example <- demographic %>% 
  dplyr::select(Judge) %>% 
  mutate(Judge_fct = as.factor(Judge))
summary(example)

unique(example$Judge)
length(unique(example$Judge))
levels(example$Judge_fct)
nlevels(example$Judge_fct)

example <- demographic %>% 
  dplyr::select(Judge) %>% 
  mutate(Judge_fct = factor(Judge, str_sort(Judge, numeric=TRUE)))
levels(example$Judge_fct)

example_reduced <- example %>%  
  filter(Judge %in% paste0("J",1:20))

unique(example_reduced$Judge)
length(unique(example_reduced$Judge))
levels(example_reduced$Judge_fct)
nlevels(example_reduced$Judge_fct)

example_reduced %>% 
  count(Judge, .drop=FALSE)
example_reduced %>% 
  count(Judge_fct, .drop=FALSE)

    ### Renaming Levels
example = demographic %>% 
  mutate(Area = factor(`Living area`, levels=c(1,2,3), labels=c("Urban", "Rurban", "Rural")))
levels(example$Area)
nlevels(example$Area)
table(example$`Living area`, example$Area)

example = demographic %>% 
  mutate(Area = factor(`Living area`, levels=c(2,3,1), labels=c("Rural", "Rural", "Urban")))
levels(example$Area)
nlevels(example$Area)
table(example$`Living area`, example$Area)

  ## Converting between types

    ### Character to Number
example <- tibble(Numbers = c("2","4","9","6","8","12","10"),
                  Text = c("Data","Science","4","Sensory","and","Consumer","Research"))
example %>% 
  mutate(NumbersN = as.numeric(Numbers), TextN = as.numeric(Text))

    ### Factor to Number
example <- example %>% 
  mutate(Numbers = as.factor(Numbers)) %>% 
  mutate(Text = factor(Text, levels=c("Data","Science","4","Sensory","and","Consumer","Research")))

example %>% 
  mutate(NumbersN = as.numeric(Numbers), TextN = as.numeric(Text))

example %>%
  mutate(Numbers = as.numeric(as.character(Numbers)))
