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
p + geom_point(aes(x=Sticky, y=Melting), pch=15, cex=5)

ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Product))+
  geom_label(aes(x=Sticky, y=Melting, label=Product), nudge_y=1)

p <- ggplot(sensory, aes(x=Sticky, y=Melting, label=Product))+
  geom_point(aes(colour=Judge))+
  geom_text(nudge_y=1)

p +
  geom_smooth(method="lm", formula="y~x", se=FALSE)


ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Protein))

ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Fiber))

sensory %>% 
  dplyr::select(Judge, Melting, Sticky) %>% 
  group_by(Judge) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  ggplot(aes(x=Sticky, y=Melting, label=Judge))+
  geom_point()+
  geom_text(nudge_y=1)

line_p <- sensory %>% 
  dplyr::select(Product, Melting, Sticky) %>% 
  group_by(Product) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  ggplot(aes(x=Sticky, y=Melting, label=Product))+
  geom_point()+
  geom_text(nudge_y=1)


# Introduction to geom_ and stat ------------------------------------------

file_path <- here("Data","Consumer Test.xlsx")

Nbiscuit <- read_xlsx(file_path, sheet="Time Consumption") %>% 
  mutate(Product = str_c("P", Product)) %>% 
  rename(N = `Nb biscuits`)

ggplot(Nbiscuit, aes(x=N))+
  geom_histogram(binwidth=1)

Nbiscuit %>% 
  group_by(Product) %>% 
  count(N) %>% 
  ungroup()

Nbiscuit %>% 
  mutate(bin = cut(N, seq(0, max(N), 1), right = TRUE)) %>% 
  group_by(Product) %>% 
  count(bin) %>% 
  ungroup() %>% 
  ggplot(aes(x=bin, y=n, fill=Product))+
  geom_bar(stat="identity", position="dodge")

biscuit_count <- Nbiscuit %>% 
  mutate(bin = cut(N, seq(0, max(N), 1), right = TRUE)) %>% 
  group_by(Product) %>% 
  count(bin) %>% 
  ungroup() %>% 
  filter(!is.na(bin), Product %in% c("P1","P2","P3"))

biscuit_plot <- ggplot(biscuit_count, aes(x=bin, y=n, fill=Product))+
  geom_col(position="dodge")


# Considering other geom_ with stats --------------------------------------

p1 <- ggplot(sensory, aes(x=Sticky, y=Melting))+
  geom_point()+
  geom_smooth(se=TRUE)+
  ggtitle("Melting = f(Sticky) with smoothing and confidence interval" )

p2 <- ggplot(sensory, aes(x=Sticky, y=Melting))+
  geom_point()+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  ggtitle("Melting = ")

p3 <- ggplot(sensory, aes(x=Sticky, y=Melting, colour=Protein))+
  geom_point()+
  geom_smooth(method="lm", formula="y~x", se=FALSE)

p4 <- ggplot(sensory, aes(x=Sticky, y=Melting))+
  geom_point()+
  geom_smooth(method="lm", formula="y~x+I(x^2)", se=FALSE)

library(patchwork)
(p1 + p2) / (p3 + p4)


# Making Graphs Pretty ----------------------------------------------------

  # Barchart

## Add Values on the Chart
p <- biscuit_plot+
  geom_text(aes(label=n), position=position_dodge(width=1), vjust=-0.5, cex=3)

## Editing (axis) Titles
p = p+
  xlab("")+
  scale_y_continuous(name = "Count", breaks = seq(0, 40, 10), labels = c("Low Count", "", " Mid Count", "", "High Count"), limits = c(0, 40))+
  ggtitle("Counts of the amount of cookies eaten", "(Results provided for P1, P2, and P3)")

## Editing the Theme
p+
  theme(panel.background = element_rect(colour="black", fill="beige"),
        panel.grid.major = element_line(colour="grey80"),
        legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())

p + theme_bw()
p + theme_minimal()
p + coord_flip()

  # Line Chart
line_p +
  scale_x_continuous(limits=c(20,40))+
  scale_y_continuous(limits=c(15,30))+
  theme_bw()+
  coord_fixed()
