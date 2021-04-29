
library(tidyverse)
library(factoextra)
library(extrafont)

data("USArrests")

dd <- USArrests %>% 
  scale() %>% 
  dist(method = "euclidean")

hc <- dd %>% 
  hclust(method = "ward.D2")

p <- fviz_dend(hc, k = 4, type = "phylogenic", repel = TRUE, phylo_layout = "layout.gem") +
  theme_void()

p

gp <- ggplot_build(p)

va_ind <- which(gp$data[[3]]$label == "Virginia")

gp$data[[2]][va_ind, 1] <- "black"
gp$data[[3]][va_ind, 1] <- "black"

gp$data[[3]]$fontface[[va_ind]] <- "bold"
gp$data[[3]]$family <- "Roboto"
gp$data[[3]]$family[[va_ind]] <- "Roboto Light"
gp$data[[3]]$size[[va_ind]] <- 10
 
q <- gp %>% 
  ggplot_gtable() %>% 
  ggplotify::as.ggplot()

q
