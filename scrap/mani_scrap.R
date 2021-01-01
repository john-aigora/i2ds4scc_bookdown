library(tidyverse)

num_panelists <- 5

num_evals <- sample(2:4, num_panelists, replace = TRUE)

eval_inds <- num_evals %>% map(~seq(1,.))

ex_tbl <- tibble(ID = 1:num_panelists, Eval = eval_inds) %>% 
  unnest(cols = c(Eval))

1:num_panelists %>% 
  set_names(letters[.]) %>% 
  enframe(name = "label", value = "number") %>% 
  deframe() %>% 
  bind_rows()

?separate_rows()


df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6")
)

separate_rows(df, y, z, convert = TRUE)

library(readxl)

path <- file.path("scrap", "excel_scrap.xlsx")  

path %>% 
  excel_sheets() %>% 
  set_names(.) %>% 
  map(~read_excel(path, sheet = .)) %>% 
  enframe(name = "Session", value = "data") %>% 
  unnest(cols = c(data))

  


