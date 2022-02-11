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

  ## Handling Rows
sensory %>% 
  arrange(Judge, desc(Product))

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
