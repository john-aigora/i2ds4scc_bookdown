library(here)
library(readxl)
library(tidyverse)


# Importing the Data ------------------------------------------------------

# Sensory Profiles Data
file_path <- here("data","Sensory Profile.xlsx") 
p_info <- read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

# Number of Biscuits Eaten Data
file_path <- here("Data","Consumer Test.xlsx")

Nbiscuit <- read_xlsx(file_path, sheet="Time Consumption") %>% 
  mutate(Product = str_c("P", Product)) %>% 
  rename(N = `Nb biscuits`)


# First Graphic -----------------------------------------------------------

p <- ggplot(sensory)
p + geom_point(aes(x=Sticky, y=Melting))

# aes()
p + geom_point(aes(x=Sticky, y=Melting, colour=Product))
p + geom_point(aes(x=Sticky, y=Melting, colour=Product), pch=15, cex=5)

# geom_*()
ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Product))+
  geom_text(aes(x=Sticky, y=Melting, label=Judge), nudge_y=1)

p <- ggplot(sensory, aes(x=Sticky, y=Melting, label=Judge))+
  geom_point(aes(colour=Product))+
  geom_text(nudge_y=1)


# Line Chart --------------------------------------------------------------

line_p <- p + geom_smooth(method="lm", formula="y~x", se=FALSE)


# Bar Chart ---------------------------------------------------------------

Nbiscuit <- Nbiscuit %>% 
  mutate(N = floor(N))

# stat_count()
bar_p <- ggplot(Nbiscuit, aes(x=N, fill=Product))+
  geom_bar(stat="count", position="dodge")

# stat_identity()
Nbiscuit %>% 
  count(Product, N) %>% 
  ggplot(aes(x=N, y=n, fill=Product))+
  geom_bar(stat="identity", position="dodge")


# Themes ------------------------------------------------------------------

# Bar Chart
bar_p + 
  theme_minimal()+
  scale_x_continuous(name="Number of Biscuits eaten", breaks=seq(0,10,1), labels=c("None", 1:9, "All of them"), limits=c(-1,11))+
  ylab("Number of Respondents")+
  ggtitle("Distribution of the number of biscuits eaten","(Results are split per biscuit type)")+
  scale_fill_manual(values=c("P1"="grey50", "P2"="grey50", "P3"="darkorange", "P4"="grey50", "P5"="grey50",
                             "P6"="grey50", "P7"="grey50", "P8"="grey50", "P9"="grey50", "P10"="grey50"))

line_p +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,50,10), limits=c(0,60))+
  scale_y_continuous(breaks=seq(0,50,10), limits=c(0,60))+
  coord_fixed()+
  ggtitle("Relationship between Melting and Sticky", "Biscuits perceived as more sticky tend to be less melting.")+
  guides(colour="none")+
  theme(panel.grid=element_blank(), panel.border=element_blank(),
        axis.line=element_line(arrow = arrow(ends = "last", type = "closed")),
        axis.title=element_text(hjust=1))


# Spider Plot -------------------------------------------------------------

sensory_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  mutate(Variables = fct_inorder(Variables)) %>% 
  group_by(Product, Variables) %>% 
  summarize(Mean = mean(Scores)) %>% 
  ungroup() %>% 
  filter(Product %in% c("P03", "POpt"))
  
spider_line <- ggplot(sensory_mean, aes(x=Variables, y=Mean, colour=Product, linetype=Product))+
  geom_point(pch=20, cex=2)+
  geom_line(aes(group=Product), lwd=1)+
  theme_minimal()+
  xlab("")+
  scale_y_continuous(name="", labels=NULL, limits=c(0,50))+
  scale_colour_manual(values=c("P03"="darkorange", "POpt"="grey50"))+
  scale_linetype_manual(values=c("P03"="solid", "POpt"="dashed"))

spider_line+
  coord_polar()

var <- levels(sensory_mean$Variables)
sensory_mean_pos <- tibble(Variables = c(var[length(var)], var),
                           Position = 0:length(var)) %>%
  full_join(sensory_mean, var_pos, by="Variables")

spiderplot <- ggplot(sensory_mean_pos, aes(x=Position, y=Mean, colour=Product, linetype=Product))+
  geom_point(pch=20, cex=2)+
  geom_line(aes(group=Product), lwd=1)+
  theme_minimal()+
  scale_x_continuous(name="", breaks=1:length(var), labels=var, limits=c(0,length(var)))+
  scale_y_continuous(name="", labels=NULL, limits=c(0,50))+
  scale_colour_manual(values=c("P03"="darkorange", "POpt"="grey50"))+
  scale_linetype_manual(values=c("P03"="solid", "POpt"="dashed"))+
  coord_polar()

# Axis Labels Positioning -------------------------------------------------

spider_line + 
  theme(axis.text = element_text(angle=45, hjust=1))

spider_line + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
