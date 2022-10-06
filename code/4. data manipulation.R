library(tidyverse)

# Importing the data set --------------------------------------------------

library(here)
library(readxl)

file_path <- here("data","biscuits_sensory_profile.xlsx") 
sensory <- read_xlsx(file_path, sheet="Data")

# Column Transformation ---------------------------------------------------

# Renaming Column
  
  ## Let's show the names of sensory
sensory %>% 
  names()

  ## Renaming some columns
sensory %>% 
  rename(Panellist = Judge, Sample = Product) %>% 
  names()

sensory %>% 
  rename(Consumer=1, Biscuit=2) %>% 
  names()

  ## Automatic renaming using {janitor}
library(janitor)
sensory %>% 
  clean_names(case="snake") %>% 
  names()

# Re-organizing Columns

  ## Re-positioning columns
sensory %>% 
  relocate(starts_with("Qty"), .after=Product) %>% 
  names()

sensory %>% 
  relocate(starts_with("Qty"), .before=Shiny) %>% 
  names()

  ## Removing/Selecting columns
sensory %>% 
    dplyr::select(Judge, Product, Shiny)

sensory %>% 
    dplyr::select(Judge, Product, `Cereal flavor`:`Dairy flavor`)

sensory %>% 
    dplyr::select(-c(Shiny, Melting))

sensory %>% 
    dplyr::select(starts_with("Qty"))

sensory %>%
    dplyr::select(where(is.character))

sensory %>% 
    dplyr::select(Panellist = Judge, Sample = Product, Shiny:Thickness, -contains("olor"))

  ## Creating Columns
sensory %>% 
  dplyr::select(Shiny, Sticky, Melting) %>% 
  mutate(Shiny2 = Shiny^2, 
         StiMelt = Sticky + Melting)

sensory %>% 
  mutate(across(where(is.numeric), round, digits=0))

sensory %>% 
  dplyr::select(Shiny, Sticky, Melting) %>% 
  mutate(across(c("Shiny","Sticky"), round, digits=0))

  ## Merging and Separating Columns
(prod_info <- read_xlsx(file_path, sheet="Product Info") %>%  
  unite(ProtFib, Protein, Fiber, sep="-"))

prod_info %>% 
  separate(ProtFib, c("Protein","Fiber"), sep="-")

  ## Conditions
sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = ifelse(Shiny < 30, "Low", "High"))

sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = ifelse(Shiny < 20, "Low", 
                             ifelse(Shiny < 48, "Medium", "High")))

sensory %>% 
  dplyr::select(Shiny) %>% 
  mutate(ShinyGroup = case_when(Shiny < 20 ~ "Low",
                                between(Shiny, 20, 48) ~ "Medium",
                                Shiny > 48 ~ "High"))

# Row Transformation ------------------------------------------------------

# Re-arranging Rows
sensory %>% 
  arrange(Judge, desc(Product))

# Selecting Rows

  ## Based on position
sensory %>% 
  slice(seq(1,89,11))

# Filtering Data

  ## One unique product
sensory %>% 
  filter(Product == "P02")

  ## Conditions
sensory_na <- sensory %>% 
  dplyr::select(Judge, Product, Shiny) %>% 
  mutate(Shiny = ifelse(Shiny > 40, NA, Shiny))

sensory_na %>% 
  filter(!is.na(Shiny))

# Splitting Data
sensory %>% 
  split(.$Product)

# Re-Shaping Data ---------------------------------------------------------

# Pivot Longer
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score")

sensory %>% 
  dplyr::select(Judge, Product, ends_with("odor"), ends_with("flavor")) %>% 
  pivot_longer(-c(Judge,Product), names_to=c("Attribute","Modality"), values_to="Score", names_sep=" ")

  ## How to Maintain the Original Order?
sensory %>% 
  dplyr::select(Judge, Product, Shiny, Salty, Bitter, Light) %>% 
  pivot_longer(-c("Judge","Product"), names_to="Variable", values_to="Score") %>% 
  split(.$Variable) %>% 
  names(.)

sensory %>% 
  dplyr::select(Judge, Product, Shiny, Salty, Bitter, Light) %>% 
  pivot_longer(-c("Judge","Product"), names_to="Variable", values_to="Score") %>% 
  mutate(Variable = fct_inorder(Variable)) %>% 
  split(.$Variable) %>% 
  names(.)

  ## Alternative Option: melt()
library(reshape2)
melt(sensory)

# Pivot Wider
sensory %>% 
  dplyr::select(Judge, Product, Shiny) %>% 
  pivot_wider(names_from = Judge, values_from = Shiny)

# Altering Data -----------------------------------------------------------

# Overall Mean
sensory %>% 
  summarise(across(where(is.numeric), mean))

sensory %>% 
  summarise(across(where(is.numeric), list(min=min, max=max)))

sensory %>% 
  summarise(across(where(is.numeric), list(min=min, max=max))) %>% 
  pivot_longer(Shiny_min:Melting_max, names_to=c("Attribute","Statistic"), values_to="Score", names_sep="_") %>% 
  pivot_wider(names_from=Statistic, values_from=Score)

# Grouping

  ## Across Rows
sensory %>% 
  group_by(Product) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  ungroup()

  ## Rowwise
sensory %>% 
  dplyr::select(Judge, Product, Shiny, Salty, Bitter) %>% 
  rowwise() %>% 
  mutate(Min = min(Shiny, Salty, Bitter))

# Illustrations -----------------------------------------------------------

# Computing the Mean

  ## Directly
sensory %>% 
  group_by(Product) %>% 
  summarise(across(Shiny:Melting, mean)) %>% 
  ungroup()

  ## Using a vector
sensory_attr <- colnames(sensory)[5:ncol(sensory)]
sensory %>% 
  group_by(Product) %>% 
  summarise(across(all_of(sensory_attr), mean)) %>% 
  ungroup()

  ## Using pivot_longer, summarise, and pivot_wider
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  mutate(Attribute = fct_inorder(Attribute)) %>%
  group_by(Product, Attribute) %>% 
  summarise(Scores = mean(Scores)) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores) %>% 
  ungroup()

  ## Omitting summarise...
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores)

  ## Using values_fn in pivot_wider
sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Scores") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Scores, values_fn=mean)

# Combining Data ----------------------------------------------------------

# Binding Vertically

  ## Reading the Data
path <- file.path("data", "excel_scrap.xlsx")
session1 <- read_xlsx(path, sheet=1)
session2 <- read_xlsx(path, sheet=2) 
session3 <- read_xlsx(path, sheet=3)

  ## Binding Rows
all_data <- bind_rows(session1, session2, session3, .id = "Session")

# Binding Horizontally

  ## Reading the Data
file_path <- here("data","biscuits_consumer_test.xlsx")
excel_sheets(file_path)

time <- read_xlsx(file_path, sheet="Time Consumption")
weight <- read_xlsx(file_path, sheet="Weight")

(consumption <- time %>% 
    full_join(weight, by="Product") %>% 
    mutate(Amount = `Nb biscuits`*Weight))

(biscuits <- read_xlsx(file_path, sheet="Biscuits") %>% 
    mutate(Consumer = str_c("J",Consumer)) %>% 
    full_join(consumption, by=c("Consumer"="Judge", "Samples"="Product")))
