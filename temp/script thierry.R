library(tidyverse)
library(here)
library(readxl)

file_path <- here("data","Sensory Profile.xlsx") 
sensory <- read_xlsx(file_path, sheet="Data")
