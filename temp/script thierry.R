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


#* ANOVA ------------------------------------------------------------------

shiny_aov <- lm(Shiny ~ Product + Judge, data=sensory)
anova(shiny_aov)

senso_aov_data <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score")

senso_aov1 <- senso_aov_data %>% 
  split(.$Attribute) %>% 
  map(function(data){
    
    res <- broom::tidy(anova(lm(Score ~ Product + Judge, data=data)))
    return(res)
    
  }) %>% 
  enframe(name="Attribute", value="res") %>% 
  unnest(res)

senso_aov2 <- senso_aov_data %>% 
  nest_by(Attribute) %>% 
  mutate(mod = list(lm(Score ~ Product + Judge, data=data))) %>% 
  summarise(broom::tidy(anova(mod))) %>% 
  ungroup()

senso_aov1 %>% 
  filter(term == "Product") %>% 
  dplyr::select(Attribute, statistic, p.value) %>% 
  mutate(Signif = ifelse(p.value <= 0.05, "Signif.", "Not Signif.")) %>% 
  mutate(Signif = factor(Signif, levels=c("Signif.", "Not Signif."))) %>% 
  ggplot(aes(x=reorder(Attribute, statistic), y=statistic, fill=Signif))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("Signif."="forestgreen", "Not Signif."="orangered2"))+
  ggtitle("Sensory Attriubtes","(The attributes are sorted according to the F-values)")+
  theme_bw()+
  xlab("")+
  ylab("F-values")+
  coord_flip()
  

#* Mean Table -------------------------------------------------------------

senso_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Score, values_fn=mean)


#* PCA --------------------------------------------------------------------

library(FactoMineR)
library(factoextra)

senso_pca <- senso_mean %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  PCA(., quali.sup=1:2, graph=FALSE)

fviz_pca_ind(senso_pca, habillage="Protein")
fviz_pca_ind(senso_pca, habillage=2)
fviz_pca_var(senso_pca)
fviz_pca_biplot(senso_pca)


# Demographic Data --------------------------------------------------------

file_path <- here("Data", "TFEQ.xlsx")

demo_var <- read_xlsx(file_path, sheet="Variables") %>% 
  dplyr::select(Code, Name)

demo_lev <- read_xlsx(file_path, sheet="Levels") %>% 
  dplyr::select(Question, Code, Levels) %>% 
  inner_join(demo_var, by=c("Question"="Code")) %>% 
  dplyr::select(-Question)

demographic <- read_xlsx(file_path, sheet="Data", skip=1, col_names=unlist(demo_var$Name))


#* Frequency Table (Raw) --------------------------------------------------

library(formattable)

demog_reduced <- demographic %>% 
  dplyr::select(Judge, `Living area`, Housing, `Income range`, `Occupation`) %>% 
  pivot_longer(-Judge, names_to="Question", values_to="Response") %>% 
  inner_join(demo_lev, by=c("Question"="Name", "Response"="Code")) %>% 
  group_by(Question, Levels) %>% 
  summarize(N = n()) %>% 
  mutate(Pct = percent(N/sum(N), digits=1L)) %>% 
  ungroup()

demog_reduced %>% 
  split(.$Question) %>% 
  map(function(data){
    
    var = as.character(unlist(data$Question))
    
    ggplot(data, aes(x=reorder(Levels, Pct), y=Pct, label=Pct))+
      geom_bar(stat="identity", fill="grey50")+
      geom_text(aes(y = Pct/2), colour="white")+
      xlab("")+
      ylab("")+
      ggtitle(var)+
      theme_bw()+
      coord_flip()
    
  })


#* Frequency Table (Ordered) ----------------------------------------------

library(RVerbalExpressions)

rdh <- rx() %>% 
  rx_either_of(c("R","D","H")) %>% 
  rx_digit() %>% 
  rx_one_or_more()

myfreq <- function(data, info){
  var = unique(unlist(data$TFEQ))
  info <- info %>% 
    filter(Name == var)
  res <- data %>% 
    mutate(Response = factor(Response, levels=info$Code, labels=info$Levels)) %>% 
    arrange(Response) %>% 
    group_by(Response) %>% 
    summarize(N = n()) %>% 
    mutate(Pct = percent(N/sum(N), digits=1L)) %>% 
    ungroup()
  return(res)
}

TFEQ_freq <- demographic %>% 
  dplyr::select(Judge, matches(rdh)) %>% 
  pivot_longer(-Judge, names_to="TFEQ", values_to="Response") %>% 
  split(.$TFEQ) %>%
  map(myfreq, info=demo_lev) %>% 
  enframe(name = "TFEQ", value="res") %>% 
  unnest(res) %>% 
  mutate(TFEQ = factor(TFEQ, levels=unique(str_sort(.$TFEQ, numeric=TRUE)))) %>% 
  arrange(TFEQ)

d <- rx() %>% 
  rx_find("D") %>% 
  rx_digit() %>% 
  rx_one_or_more()

TFEQ_freq %>% 
  filter(str_detect(TFEQ, d)) %>% 
  ggplot(aes(x=Response, y=Pct, label=Pct))+
  geom_bar(stat="identity", fill="grey50")+
  geom_text(aes(y = Pct/2), colour="white")+
  theme_bw()+
  facet_wrap(~TFEQ, scales="free")
  

#* TFEQ Score -------------------------------------------------------------

var_rdh <- read_xlsx(file_path, sheet="Variables") %>% 
  filter(str_detect(Name, rdh)) %>% 
  dplyr::select(Name, Direction, Value)

TFEQ <- demographic %>% 
  dplyr::select(Judge, matches(rdh)) %>% 
  pivot_longer(-Judge, names_to="DHR", values_to="Score") %>% 
  inner_join(var_rdh, by=c("DHR"="Name")) %>% 
  mutate(TFEQValue = ifelse(Direction == "Equal" & Score == Value, 1, 
                        ifelse (Direction == "Superior" & Score > Value, 1,
                                ifelse(Direction == "Inferior" & Score < Value, 1, 0)))) %>% 
  mutate(Factor = ifelse(str_starts(.$DHR, "D"), "Disinhibitor",
                         ifelse(str_starts(.$DHR, "H"), "Hunger", "Restriction"))) %>% 
  mutate(Factor = factor(Factor, levels=c("Restriction","Disinhibitor","Hunger"))) %>%
  group_by(Judge, Factor) %>% 
  summarize(TFEQ = sum(TFEQValue)) %>% 
  mutate(Judge = factor(Judge, levels=unique(str_sort(.$Judge, numeric=TRUE)))) %>% 
  arrange(Judge) %>% 
  pivot_wider(names_from=Factor, values_from=TFEQ) %>% 
  mutate(Total = sum(across(where(is.numeric))))

TFEQ %>% 
  dplyr::select(-Total) %>% 
  pivot_longer(-Judge, names_to="Factor", values_to="Scores") %>% 
  ggplot(aes(x=Scores, colour=Factor))+
  geom_density(lwd=1.5)+
  xlab("TFEQ Score")+
  ylab("")+
  ggtitle("Distribution of the Individual TFEQ-factor Scores")+
  theme_bw()


# Consumer Data -----------------------------------------------------------

file_path <- here("Data","Consumer Test.xlsx")

Nbiscuit <- read_xlsx(file_path, sheet="Time Consumption") %>% 
  mutate(Product = str_c("P", Product)) %>% 
  rename(N = `Nb biscuits`)

consumer <- read_xlsx(file_path, sheet="Biscuits") %>% 
  rename(Judge=Consumer, Product=Samples) %>% 
  mutate(Judge = str_c("J", Judge), Product = str_c("P", Product)) %>% 
  inner_join(Nbiscuit, by=c("Judge", "Product"))


#* ANOVA and Post'hoc Test ------------------------------------------------

consumer %>% 
  dplyr::select(Judge, Product, `1stbite_liking`, `after_liking`) %>% 
  group_by(Product) %>% 
  summarise(across(where(is.numeric), mean))

library(agricolae)

liking_start <- lm(`1stbite_liking` ~ Product + Judge, data=consumer)
liking_start_hsd <- HSD.test(liking_start, "Product")$groups %>% 
  as_tibble(rownames = "Product")

liking_end <- lm(`after_liking` ~ Product + Judge, data=consumer)
liking_end_hsd <- HSD.test(liking_end, "Product")$groups %>% 
  as_tibble(rownames = "Product")


#* Start vs. End Evaluation -----------------------------------------------

list(Start = liking_start_hsd %>% rename(Liking=`1stbite_liking`), 
     End = liking_end_hsd %>% rename(Liking=`after_liking`)) %>% 
  enframe(name = "Moment", value = "res") %>%
  unnest(res) %>% 
  mutate(Moment = factor(Moment, levels=c("Start","End"))) %>% 
  ggplot(aes(x=reorder(Product, -Liking), y=Liking, fill=Moment))+
  geom_bar(stat="identity", position="dodge")+
  xlab("")+
  ggtitle("Comparison of the liking scores at the start and at the end of the evaluation")+
  theme_bw()

consumer %>% 
  dplyr::select(Judge, Product, Start=`1stbite_liking`, End=`after_liking`) %>% 
  filter(Judge %in% str_c("J",1:12)) %>% 
  mutate(Judge = factor(Judge, levels=unique(str_sort(.$Judge, numeric=TRUE)))) %>% 
  ggplot(aes(x=Start, y=End))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  theme_bw()+
  ggtitle("Overall Liking", "(Assessment after first bite vs. end of the tasting)")+
  facet_wrap(~Judge)

consumer %>% 
  dplyr::select(Judge, Product,  End=`after_liking`, Liking=`end_liking 9pt`) %>% 
  mutate(Judge = factor(Judge, levels=unique(str_sort(.$Judge, numeric=TRUE)))) %>% 
  mutate(Liking = 10-Liking) %>% 
  ggplot(aes(x=End, y=Liking))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  theme_bw()+
  ggtitle("Overall Liking", "(Continuous scale vs. Categorical scale)")+
  facet_wrap(~Judge)


#* Liking and Number of Biscuits Eaten ------------------------------------

consumer %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`, N) %>% 
  mutate(Judge = factor(Judge, levels=unique(str_sort(.$Judge, numeric=TRUE)))) %>% 
  mutate(Liking = 10-Liking) %>% 
  ggplot(aes(x=Liking, y=N))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  theme_bw()+
  ggtitle("Number of Cookies eaten against Overall Liking")+
  facet_wrap(~Judge, scales="free_y")

  ## Limiting to Significant Regression only
run_reg <- function(df){
  output <- lm(N ~ Liking, data=df)
  return(output)
}

N_liking <- consumer %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`, N) %>% 
  mutate(Liking = 10-Liking) %>% 
  group_by(Judge) %>%
  nest() %>%
  ungroup() %>%
  mutate(lm_obj = map(data, run_reg)) %>% 
  mutate(glance = map(lm_obj, broom::glance)) %>% 
  unnest(glance) %>% 
  filter(p.value <= 0.05) %>% 
  arrange(p.value) %>% 
  mutate(Judge = fct_reorder(Judge, p.value)) %>% 
  unnest(data)

ggplot(N_liking, aes(x=Liking, y=N))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  theme_bw()+
  ggtitle("Number of Biscuits vs. Liking","(Consumers with a significant (5%) regression model are shown (ordered from the most to the least signif.)")+
  facet_wrap(~Judge, scales="free_y")


# Combining Data ----------------------------------------------------------

#* Internal Preference Mapping --------------------------------------------
consumer_wide <- consumer %>% 
  separate(Product, into = c("P", "Number"), sep = 1) %>% 
  mutate(Number = ifelse(nchar(Number) == 1, str_c("0", Number), Number)) %>% 
  unite(Product, P, Number, sep="") %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`) %>% 
  mutate(Liking = 10-Liking) %>% 
  pivot_wider(names_from=Judge, values_from=Liking)

data_mdpref <- senso_mean %>% 
  inner_join(consumer_wide, by="Product")

res_mdpref <- data_mdpref %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  PCA(., quali.sup=1:2, quanti.sup=3:34, graph=FALSE)

fviz_pca_ind(res_mdpref, habillage=1)
fviz_pca_var(res_mdpref, label="quanti.sup", select.var=list(cos2=0.5), repel=TRUE)

#* Clustering -------------------------------------------------------------

  ## Using Cluster
consumer_dist <- consumer_wide %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  scale(., center=TRUE, scale=FALSE) %>% 
  t(.) %>% 
  dist(., method="euclidean")

# library(cluster)

res_hclust <- hclust(consumer_dist, method="ward.D2")
res_clust <- cutree(res_hclust, k=2) %>% 
  as_tibble(rownames="Judge") %>% 
  rename(Cluster = value) %>% 
  mutate(Cluster = as.character(Cluster))

res_clust %>% 
  group_by(Cluster) %>% 
  count() %>% 
  ungroup()

fviz_dend(res_hclust, k=2)
fviz_dend(res_hclust, k=2, type="phylogenic")

# res_agnes <- agnes(consumer_dist, method="ward")
# cutree(res_agnes, k=2) %>% 
#   as_tibble() %>% 
#   group_by(value) %>% 
#   count()

mean_cluster <- consumer %>% 
  separate(Product, into = c("P", "Number"), sep = 1) %>% 
  mutate(Number = ifelse(nchar(Number) == 1, str_c("0", Number), Number)) %>% 
  unite(Product, P, Number, sep="") %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`) %>%
  full_join(res_clust, by="Judge") %>% 
  group_by(Product, Cluster) %>% 
  summarize(Liking = mean(Liking), N=n()) %>% 
  mutate(Cluster = str_c(Cluster," (",N,")")) %>% 
  ungroup()

ggplot(mean_cluster, aes(x=Product, y=Liking, colour=Cluster, group=Cluster))+
  geom_point(pch=20)+
  geom_line(aes(group=Cluster), lwd=2)+
  xlab("")+
  scale_y_continuous(name="Average Liking Score", limits=c(1,9), breaks=seq(1,9,1))+
  ggtitle("Cluster differences in the appreciation of the Products (using hclust)")+
  theme_bw()

  ## Extend to HCPC
res_hcpc <- consumer_wide %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  scale(., center=TRUE, scale=FALSE) %>% 
  t(.) %>% 
  PCA(., scale.unit=FALSE, ncp=Inf, graph=FALSE) %>% 
  HCPC(., nb.clust=2, consol=TRUE, graph=FALSE) %>% 
  .$data.clust %>% 
  as_tibble(rownames="Judge") %>% 
  dplyr::select(Judge, Cluster=clust)

res_hcpc %>% 
  group_by(Cluster) %>% 
  count()

mean_cluster2 <- consumer %>% 
  separate(Product, into = c("P", "Number"), sep = 1) %>% 
  mutate(Number = ifelse(nchar(Number) == 1, str_c("0", Number), Number)) %>% 
  unite(Product, P, Number, sep="") %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`) %>%
  full_join(res_hcpc, by="Judge") %>% 
  group_by(Product, Cluster) %>% 
  summarize(Liking = mean(Liking), N=n()) %>% 
  mutate(Cluster = str_c(Cluster," (",N,")")) %>% 
  ungroup()

ggplot(mean_cluster2, aes(x=Product, y=Liking, colour=Cluster, group=Cluster))+
  geom_point(pch=20)+
  geom_line(aes(group=Cluster), lwd=2)+
  xlab("")+
  scale_y_continuous(name="Average Liking Score", limits=c(1,9), breaks=seq(1,9,1))+
  ggtitle("Cluster differences in the appreciation of the Products (using HCPC with consolidation)")+
  theme_bw()


#* Linear and Quadratic Relationship --------------------------------------

  ## Correlation
data_cor <- mean_cluster %>% 
  dplyr::select(-N) %>% 
  pivot_wider(names_from=Cluster, values_from=Liking) %>% 
  inner_join(senso_mean %>% dplyr::select(-c(Protein, Fiber)), by="Product") %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product")

library(ggcorrplot)

res_cor <- cor(data_cor)
res_cor_pmat <- cor_pmat(data_cor)

ggcorrplot(res_cor, type="full", p.mat=res_cor_pmat, insig="blank", lab=TRUE, lab_size=2)

  ## Simple and Quadratic Regression
data_reg <- mean_cluster %>% 
  dplyr::select(-N) %>% 
  pivot_wider(names_from=Cluster, values_from=Liking) %>% 
  inner_join(senso_mean %>% dplyr::select(-c(Protein, Fiber)), by="Product") %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score") %>% 
  mutate(Attribute = factor(Attribute, levels=colnames(senso_mean)[4:ncol(senso_mean)])) %>% 
  mutate(Score2 = Score^2)

res_reg <- data_reg %>% 
  nest_by(Attribute) %>% 
  mutate(lin_mod = list(lm(`1 (74)`~Score, data=data)), quad_mod = list(lm(`1 (74)`~Score + Score2, data=data)))

lin <- res_reg %>% 
  summarise(broom::tidy(lin_mod)) %>% 
  ungroup() %>% 
  filter(term == "Score", p.value <= 0.05) %>% 
  dplyr::select(Attribute) %>% 
  unlist() %>% 
  as.character()

quad <- res_reg %>% 
  summarise(broom::tidy(quad_mod)) %>% 
  ungroup() %>% 
  filter(term == "Score2", p.value <= 0.06) %>%
  dplyr::select(Attribute) %>% 
  unlist() %>% 
  as.character()

library(ggrepel)
data_reg %>% 
  filter(Attribute %in% unique(c(lin,quad))) %>% 
  ggplot(aes(x=Score, y=`1 (74)`, label=Product))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", formula="y~x", se=FALSE)+
  geom_smooth(method="lm", formula="y~x+I(x^2)", se=FALSE, colour="red", lty=2)+
  geom_text_repel()+
  theme_bw()+
  facet_wrap(~Attribute, scales="free_x")

#* External Preference Mapping --------------------------------------------



