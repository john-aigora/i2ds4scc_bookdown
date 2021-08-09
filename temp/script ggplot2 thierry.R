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


# First Plot --------------------------------------------------------------

p <- ggplot(sensory)
p + geom_point(aes(x=Sticky, y=Melting))
p + geom_point(aes(x=Sticky, y=Melting, colour=Product))
p + geom_point(aes(x=Sticky, y=Melting, colour=Product), pch=15, cex=5)

p <- ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Product))+
  geom_text(aes(x=Sticky, y=Melting, label=Judge), nudge_y=1)
p
