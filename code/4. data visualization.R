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

# gt  ---------------------------------------------------------------------

library(gt)

file_path <- here("data","biscuits_consumer_test.xlsx") 

mean_consumer <- read_xlsx(file_path, sheet="Time Consumption") %>%
  dplyr::select(Product, `Time (min)`, `Nb biscuits`) %>%
  separate(`Time (min)`, c("Min", "Sec"), sep="min") %>%
  mutate(across(c("Min","Sec"), as.numeric)) %>% 
  mutate(Time = Min+Sec/60) %>% 
  group_by(Product) %>% 
  summarise(across(c("Time", "Nb biscuits"), mean, na.rm = TRUE)) %>% 
  ungroup()

consumer_gt_table <- mean_consumer %>%
  gt () %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = c( "Time", "Nb biscuits") , decimals = 2) %>%
  tab_header(title = md ("**Consumption time and number of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*"))

mean_consumer_2 <- mean_consumer %>%
  dplyr::select(-Time) %>%
  arrange(desc(`Nb biscuits`))

note <- str_c("Avg. consumption time: ", round(mean(mean_consumer$Time),2), " min")

consumption <- mean_consumer_2 %>%
  gt () %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = "Nb biscuits" , decimals = 2) %>%
  tab_header(title = md ("**Number of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*")) %>%
  tab_source_note(source_note = note)

nb_range <- range(mean_consumer_2$`Nb biscuits`)

library(scales)
consumption %>% 
  data_color(columns=`Nb biscuits`, colors=col_numeric(c("#FEF0D9","#990000"), domain=nb_range, alpha=0.75))

library(gtExtras)
mean_consumer_2 %>%
  mutate(`Nb biscuits (%)` = 10*`Nb biscuits`) %>% 
  gt () %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = "Nb biscuits" , decimals=2) %>%
  tab_header(title = md ("**Number of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*")) %>%
  tab_source_note(source_note = note) %>% 
  gt_plt_bar_pct(`Nb biscuits (%)`, scaled=TRUE)
