library(tidyverse)
library(readxl)
library(here)

# Importing the Data ------------------------------------------------------

file_path <- here("data","biscuits_sensory_profile.xlsx") 

mean_sensory <- read_xlsx(file_path, sheet="Data") %>% 
  select(Product, Shiny:`Color contrast`) %>%
  group_by(Product) %>%
  summarize(across(.cols = where(is.numeric),.fns = mean))

# Flextable ---------------------------------------------------------------

library(officer)
library(flextable)

flex_table <- mean_sensory %>% 
  flextable()

flex_table_design <- flex_table %>%
  colformat_double(digits = 2) %>%
  fontsize(size = 10, part = "all") %>% 
  bold(bold = TRUE, part = "header") %>%
  italic(j=-1, italic = TRUE, part = "body") %>%
  align(align = "center", part = "all")

flex_table_design %>%
  bg(bg = "black", part = "header") %>% 
  color(color = "white", part = "header") %>% 
  fontsize(size = 13, part = "header", i = 1) %>%
  color(i = 11, color = "orange", part = "body") %>%
  color(i = 1:10, color = "grey70", part = "body") %>% 
  add_header_lines(values = "Appearance Profile of 11 biscuits")

flex_table_design %>%
  hline(i=10, border=fp_border(color="grey70", style="dashed")) %>% 
  autofit()

color_code <- ifelse(mean_sensory$Thickness <= 20, "blue", 
                     ifelse(mean_sensory$Thickness >= 40, "red", "black"))
flex_table_design %>% 
  color(j=~Thickness, color=color_code)