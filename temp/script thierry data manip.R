library(tidyverse)
library(here)
library(readxl)

# Sensory Data ------------------------------------------------------------

file_path <- here("data","Sensory Profile.xlsx") 
p_info <- read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

# Simple Transformation ---------------------------------------------------

  ## Handling columns
sensory %>% 
  names()

sensory %>% 
  rename(Panellist = Judge, Sample = Product) %>% 
  names()

library(janitor)
sensory %>% 
  clean_names(case="snake") %>% 
  names()

sensory %>% 
  relocate(starts_with("Qty"), .after=Product) %>% 
  names()

sensory %>% 
  relocate(starts_with("Qty"), .before=Shiny) %>% 
  names()

sensory %>% 
  dplyr::select(Judge, Product, Shiny)

sensory %>% 
  dplyr::select(Judge, Product, `Cereal flavor`:Warming)

sensory %>% 
  dplyr::select(-c(Shiny, Melting))

sensory %>% 
  dplyr::select(starts_with("Qty"))

sensory %>% 
  dplyr::select(where(is.numeric))

sensory %>% 
  dplyr::select(Panellist = Judge, Sample = Product, Shiny:Sticky, -starts_with("Qty"))

sensory %>% 
  dplyr::select(Shiny, Sticky, Melting) %>% 
  mutate(Shiny2 = Shiny^2, 
         StiMelt = Sticky + Melting)

sensory %>% 
  mutate(Product = tolower(Product))

round(sensory, 0) #returns an error because Judge and Product are characters

sensory %>% 
  mutate(across(where(is.numeric), round, 0))

file_path <- here("data","Sensory Profile.xlsx") 
prodinfo <- read_xlsx(file_path, sheet="Product Info") %>%  
  unite(ProtFib, Protein, Fiber, sep="-")
prodinfo

prodinfo %>% 
  separate(ProtFib, c("Protein","Fiber"), sep="-")

sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = ifelse(Shiny < 30, "Low", "High"))

sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = ifelse(Shiny < 20, "Low", 
                             ifelse(Shiny < 40, "Medium", "High")))


sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = case_when(Shiny < 20 ~ "Low",
                                between(Shiny, 20, 40) ~ "Medium",
                                Shiny > 40 ~ "High"))

  ## Handling Rows
sensory %>% 
  arrange(Judge, desc(Product))

sensory %>% 
  slice(seq(1,89,11))

sensory %>% 
  filter(Product == "P02")

(sensory_na <- sensory %>% 
    dplyr::select(Judge, Product, Shiny) %>% 
    mutate(Shiny = ifelse(Shiny > 40, NA, Shiny)))

sensory_na %>% 
  filter(!is.na(Shiny))

split(sensory, sensory$Product)
sensory %>% 
  split(.$Product)

  ## Re-Shaping Data
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score")

sensory %>% 
  dplyr::select(Judge, Product, ends_with("odor"), ends_with("flavor")) %>% 
  pivot_longer(-c(Judge,Product), names_to=c("Attribute","Modality"), values_to="Score", names_sep=" ")

library(reshape2)
melt(sensory)

sensory %>% 
  dplyr::select(Judge, Product, Shiny) %>% 
  pivot_wider(names_from = Judge, values_from = Shiny)

# Transformation that Alters the Data -------------------------------------

sensory %>% 
  summarise(across(where(is.numeric), mean))

sensory %>% 
  summarise(across(where(is.numeric), list(min=min, max=max)))

sensory %>% 
  group_by(Product) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  ungroup()

sensory %>% 
  group_by(Product) %>% 
  summarise(across(Shiny:Melting, mean)) %>% 
  ungroup()

sensory_attr <- colnames(sensory)[5:ncol(sensory)]
sensory %>% 
  group_by(Product) %>% 
  summarise(across(all_of(sensory_attr), mean)) %>% 
  ungroup()

sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  mutate(Attribute = fct_inorder(Attribute)) %>% 
  group_by(Product, Attribute) %>% 
  summarise(Scores = mean(Scores)) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores) %>% 
  ungroup()

sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores)

sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores, values_fn=mean)

# Combining Data ----------------------------------------------------------

  ## By rows
path <- file.path("data", "excel_scrap.xlsx")

session1 <- read_xlsx(path, sheet=1)
session2 <- read_xlsx(path, sheet=2) 
session3 <- read_xlsx(path, sheet=3)

all_data <- bind_rows(session1, session2, session3, .id = "Session")

  ## Bu Columns
file_path <- here("data","Consumer Test.xlsx")
excel_sheets(file_path)

time <- read_xlsx(file_path, sheet="Time Consumption")
weight <- read_xlsx(file_path, sheet="Weight")
(consumption <- time %>% 
    full_join(weight, by="Product") %>% 
    mutate(Amount = `Nb biscuits`*Weight))

(biscuits <- read_xlsx(file_path, sheet="Biscuits") %>% 
  mutate(Consumer = str_c("J",Consumer)) %>% 
  full_join(consumption, by=c("Consumer"="Judge", "Samples"="Product")))
