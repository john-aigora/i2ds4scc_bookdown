library(tidyverse)
library(readxl)
library(here)

# Importing the Data ------------------------------------------------------

file_path <- here("data","biscuits_sensory_profile.xlsx") 

mean_sensory <- read_xlsx(file_path, sheet="Data") %>% 
  select(Product, Shiny, Sweet, Sour, Bitter, Salty, Astringent) %>%
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
  add_header_lines(values = "Sensory Profile of 11 biscuits") %>% 
  border_remove() %>% 
  border_outer(border=fp_border(color="darkorange", width=2)) %>%
  fix_border_issues()

flex_table_design %>%
  hline(i=10, border=fp_border(color="grey70", style="dashed")) %>% 
  autofit()

color_code <- ifelse(mean_sensory$Sweet <= 20, "blue", 
                     ifelse(mean_sensory$Sweet >= 30, "red", "black"))
flex_table_design %>% 
  color(j=~Sweet, color=color_code)

# gt  ---------------------------------------------------------------------

library(gt)

file_path <- here("data","biscuits_consumer_test.xlsx") 

mean_consumer <- readxl::read_xlsx(file_path, 
                                   sheet="Time Consumption") %>%
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
  tab_header(title = md ("**Cons. time and nb. of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*"))

mean_consumer_2 <- mean_consumer %>%
  dplyr::select(-Time) %>%
  arrange(desc(`Nb biscuits`))

note <- str_c("Avg. consumption time: ", 
              round(mean(mean_consumer$Time),2), " min")

consumption <- mean_consumer_2 %>%
  gt () %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = "Nb biscuits" , decimals = 2) %>%
  tab_header(title = md ("**Number of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*")) %>%
  tab_source_note(source_note = note)

library(scales)

nb_range <- range(mean_consumer_2$`Nb biscuits`)

consumption %>% 
  data_color(columns=`Nb biscuits`, 
             colors=col_numeric(c("#FEF0D9","#990000"), 
                                domain=nb_range, alpha=0.75))

library(gtExtras)
mean_consumer_2 %>%
  mutate(`Nb biscuits (%)` = 100*(`Nb biscuits`/10)) %>% 
  gt () %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_number(columns = "Nb biscuits" , decimals=2) %>%
  tab_header(title = md ("**Number of biscuits eaten**"), 
             subtitle = md ("*Average taken from 99 consumers*")) %>%
  tab_source_note(source_note = note) %>% 
  gt_plt_bar_pct(`Nb biscuits (%)`, scaled=TRUE)

# ggplot2 -----------------------------------------------------------------

  ## Importing the Data
file_path <- here("data","biscuits_sensory_profile.xlsx") 

p_info <- readxl::read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- readxl::read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

file_path <- here("Data","biscuits_consumer_test.xlsx")

Nbiscuits <- readxl::read_xlsx(file_path, sheet="Time Consumption") %>% 
  mutate(Product = str_c("P", Product)) %>% 
  rename(N = `Nb biscuits`)

  ## Creating a graph
p <- ggplot(sensory)
p + geom_point(aes(x=Sticky, y=Melting))
p + geom_point(aes(x=Sticky, y=Melting, colour=Product))
p + geom_point(aes(x=Sticky, y=Melting, colour=Product), pch=15, cex=5)

ggplot(sensory)+
  geom_point(aes(x=Sticky, y=Melting, colour=Product))+
  geom_text(aes(x=Sticky, y=Melting, label=Judge), nudge_y=1)

p <- ggplot(sensory, aes(x=Sticky, y=Melting, label=Judge))+
  geom_point(aes(colour=Product))+
  geom_text(nudge_y=1)

line_p <- p+
  geom_smooth(method=lm, formula="y~x", se=FALSE)

  ## Bar chart
Nbiscuits <- Nbiscuits %>% 
  mutate(N = floor(N))

bar_p <- ggplot(Nbiscuits, aes(x=N, fill=Product))+
  geom_bar(stat="count", position="dodge")

Nbiscuits %>% 
  count(Product, N) %>% 
  ggplot(aes(x=N, y=n, fill=Product))+
  geom_bar(stat="identity", position="dodge")

  ## Themes
bar_p <- bar_p+
  theme_minimal()

bar_p <- bar_p +
  scale_x_continuous(name="Number of Biscuits eaten", 
                     breaks=seq(0,10,1), 
                     labels=c("None", 1:9, "All of them"), 
                     limits=c(-1,11))+
  ylab("Number of Respondents")

bar_p <- bar_p +
  ggtitle("Distribution of the number of biscuits eaten",
          "(Results are split per biscuit type)")

line_p <- line_p +
  theme_bw()+
  scale_x_continuous(breaks=seq(0,50,10), limits=c(0,60))+
  scale_y_continuous(breaks=seq(0,50,10), limits=c(0,60))+
  coord_fixed()+
  ggtitle("Relationship between Melting and Sticky",
          "Stickier biscuits tend to be less melting.")+
  guides(colour="none")

line_p +
  theme(panel.grid=element_blank(), 
        panel.border=element_blank(),
        axis.line=element_line(arrow = arrow(ends = "last", 
                                             type = "closed")),
        axis.title=element_text(hjust=1))

bar_p + 
  scale_fill_manual(values=c("P1"="gray50", "P2"="gray50", 
                             "P3"="darkorange", "P4"="gray50", 
                             "P5"="gray50", "P6"="gray50",
                             "P7"="gray50", "P8"="gray50",
                             "P9"="gray50", "P10"="gray50"))

  ## Other modifications
bar_p + coord_flip()

  ## Spider Plot
sensory_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Variables", values_to="Scores") %>% 
  mutate(Variables = fct_inorder(Variables)) %>% 
  group_by(Product, Variables) %>% 
  summarize(Mean = mean(Scores)) %>% 
  ungroup() %>% 
  filter(Product %in% c("P03", "POpt"))

spider_line <- ggplot(sensory_mean, aes(x=Variables, y=Mean, colour=Product, linetype=Product))+
  geom_point(pch=20, cex=3)+
  geom_line(aes(group=Product), lwd=1)+
  theme_minimal()+
  xlab("")+
  scale_y_continuous(name="", labels=NULL, limits=c(0,50))+
  scale_colour_manual(values=c("P03"="darkorange", "POpt"="grey50"))+
  scale_linetype_manual(values=c("P03"="solid", "POpt"="dashed"))

spider_line + coord_polar()

var <- levels(sensory_mean$Variables)
sensory_mean_pos <- tibble(Variables = c(var[length(var)], var),
                           Position = 0:length(var)) %>%
  full_join(sensory_mean, var_pos, by="Variables")

spider_plot <- ggplot(sensory_mean_pos, aes(x=Position, y=Mean, colour=Product, linetype=Product))+
  geom_point(pch=20, cex=2)+
  geom_line(aes(group=Product), lwd=1)+
  theme_minimal()+
  scale_x_continuous(name="", breaks=1:length(var), labels=var, limits=c(0,length(var)))+
  scale_y_continuous(name="", labels=NULL, limits=c(0,50))+
  scale_colour_manual(values=c("P03"="darkorange", "POpt"="grey50"))+
  scale_linetype_manual(values=c("P03"="solid", "POpt"="dashed"))+
  coord_polar()+
  theme(legend.position = "bottom", legend.title = element_blank())

  ## Combining graphs
library(patchwork)
p <- spider_plot + (bar_p / line_p)

p = p + plot_annotation(title = "Example of 'ggplots' I've learned today", tag_levels='a')

  ## Adjusting the legends
spider_line + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

spider_line + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

  ## Saving Graphs
ggsave(filename="spiderplot.png", plot=spiderplot, device="png")