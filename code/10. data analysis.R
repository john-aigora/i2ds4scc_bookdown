library(tidyverse)
library(here)
library(readxl)

# Sensory Data ------------------------------------------------------------

file_path <- here("data","biscuits_sensory_profile.xlsx") 
p_info <- read_xlsx(file_path, sheet="Product Info") %>% 
  dplyr::select(-Type)

sensory <- read_xlsx(file_path, sheet="Data") %>% 
  inner_join(p_info, by="Product") %>% 
  relocate(Protein:Fiber, .after=Product)

  ## ANOVA
sweet_aov <- lm(Sweet ~ Product + Judge, data=sensory)
anova(sweet_aov)

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

res_aov <- senso_aov1 %>%
  filter(term == "Product") %>%
  dplyr::select(Attribute, statistic, p.value) %>%
  arrange(desc(p.value)) %>%
  mutate(p.value = round(p.value, 3))

res_aov %>% 
  filter(p.value >= 0.05)

res_aov %>% 
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

  ## PCA
senso_mean <- sensory %>% 
  pivot_longer(Shiny:Melting, names_to="Attribute", values_to="Score") %>% 
  dplyr::select(-Judge) %>% 
  pivot_wider(names_from=Attribute, values_from=Score, values_fn=mean)

library(FactoMineR)

senso_pca <- senso_mean %>% 
  arrange(Product) %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  PCA(., ind.sup=nrow(.), quali.sup=1:2, graph=FALSE)

library(factoextra)

fviz_pca_ind(senso_pca, habillage="Protein")
fviz_pca_ind(senso_pca, habillage=2)
fviz_pca_var(senso_pca)
fviz_pca_biplot(senso_pca)

# Demographic Data --------------------------------------------------------

file_path <- here("Data", "biscuits_traits.xlsx")

demo_var <- read_xlsx(file_path, sheet="Variables") %>% 
  dplyr::select(Code, Name)

demo_lev <- read_xlsx(file_path, sheet="Levels") %>% 
  dplyr::select(Question, Code, Levels) %>% 
  inner_join(demo_var, by=c("Question"="Code")) %>% 
  dplyr::select(-Question)

demographic <- read_xlsx(file_path, sheet="Data", skip=1, col_names=unlist(demo_var$Name))

  ## Frequency and Proportion
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
    var <- data %>%
      pull(Question) %>%
      unique()
    ggplot(data, aes(x = reorder(Levels, Pct), y = Pct, label = Pct)) +
      geom_bar(stat = "identity", fill = "grey50") +
      geom_text(aes(y = Pct / 2), colour = "white") +
      xlab("") +
      ylab("") +
      ggtitle(var) +
      theme_bw() +
      coord_flip()
  })

  ## TFEQ data

    ### Frequency
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
  theme(axis.text = element_text(hjust=1, angle=30))+
  facet_wrap(~TFEQ, scales="free")
  
    ### TFEQ Score
var_rdh <- read_xlsx(file_path, sheet="Variables") %>% 
  filter(str_detect(Name, rdh)) %>% 
  dplyr::select(Name, Direction, Value)

TFEQ <- demographic %>% 
  dplyr::select(Judge, matches(rdh)) %>% 
  pivot_longer(-Judge, names_to="DHR", values_to="Score") %>% 
  inner_join(var_rdh, by=c("DHR"="Name"))

TFEQ_coded <- TFEQ %>% 
  mutate(TFEQValue = ifelse(Direction == "Equal" & Score == Value, 1, 
                        ifelse (Direction == "Superior" & Score > Value, 1,
                                ifelse(Direction == "Inferior" & Score < Value, 1, 0)))) %>% 
  mutate(Factor = ifelse(str_starts(.$DHR, "D"), "Disinhibitor",
                         ifelse(str_starts(.$DHR, "H"), "Hunger", "Restriction"))) %>% 
  mutate(Factor = factor(Factor, levels=c("Restriction","Disinhibitor","Hunger"))) 

TFEQ_score <- TFEQ_coded %>%
  group_by(Judge, Factor) %>% 
  summarize(TFEQ = sum(TFEQValue)) %>% 
  mutate(Judge = factor(Judge, levels=unique(str_sort(.$Judge, numeric=TRUE)))) %>% 
  arrange(Judge) %>% 
  pivot_wider(names_from=Factor, values_from=TFEQ) %>% 
  mutate(Total = sum(across(where(is.numeric))))

TFEQ_score %>% 
  dplyr::select(-Total) %>% 
  pivot_longer(-Judge, names_to="Factor", values_to="Scores") %>% 
  ggplot(aes(x=Scores, color=Factor))+
  geom_density(lwd=1.5, key_glyph="path")+
  xlab("TFEQ Score")+
  ylab("")+
  guides(color = guide_legend(override.aes = list(linetype = 1)))+
  ggtitle("Distribution of the Individual TFEQ-factor Scores")+
  theme_bw()

# Consumer Data -----------------------------------------------------------

file_path <- here("Data","biscuits_consumer_test.xlsx")

Nbiscuit <- read_xlsx(file_path, sheet="Time Consumption") %>% 
  mutate(Product = str_c("P", Product)) %>% 
  rename(N = `Nb biscuits`)

consumer <- read_xlsx(file_path, sheet="Biscuits") %>% 
  rename(Judge=Consumer, Product=Samples) %>% 
  mutate(Judge = str_c("J", Judge), Product = str_c("P", Product)) %>% 
  inner_join(Nbiscuit, by=c("Judge", "Product"))

  ## ANOVA & Post'Hoc test
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

  ## Liking Start vs. End
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

  ## Liking and N Biscuits Eaten
run_reg <- function(df){
  output <- lm(N ~ Liking, data=df)
  return(output)
}

N_liking_reg <- consumer %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`, N) %>% 
  mutate(Liking = 10-Liking) %>% 
  group_by(Judge) %>%
  nest() %>%
  ungroup() %>%
  mutate(lm_obj = map(data, run_reg)) %>% 
  mutate(glance = map(lm_obj, broom::glance))

N_liking <- N_liking_reg %>% 
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

  ## Internal Preference Mapping
consumer_wide <- consumer %>% 
  separate(Product, into = c("P", "Number"), sep = 1) %>% 
  mutate(Number = ifelse(nchar(Number) == 1, str_c("0", Number), Number)) %>% 
  unite(Product, P, Number, sep="") %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`) %>% 
  mutate(Liking = 10-Liking) %>% 
  pivot_wider(names_from=Judge, values_from=Liking)

data_mdpref <- senso_mean %>% 
  inner_join(consumer_wide, by="Product")

# The first two columns are qualitative and are projected as supplementary through quali.sup=1:2
# The next 32 columns correspond to the sensory attributes which are projected as supplementary through quanti.sup=3:34
res_mdpref <- data_mdpref %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  PCA(., quali.sup=1:2, quanti.sup=3:34, graph=FALSE)

# The sensory product can be coloured based on their Protein content through habillage=1 (1st qualitative variable)
fviz_pca_ind(res_mdpref, habillage=1)

# For the variables representation, we only show the one with a decent quality of representation (here cos2 >= 0.5)
fviz_pca_var(res_mdpref, label="quanti.sup", select.var=list(cos2=0.5), repel=TRUE)

  ## Clustering
consumer_dist <- consumer_wide %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product") %>% 
  scale(., center=TRUE, scale=FALSE) %>% 
  t(.) %>% 
  dist(., method="euclidean")

res_hclust <- hclust(consumer_dist, method="ward.D2")
fviz_dend(res_hclust, k=2)
# fviz_dend(res_hclust, k=2, type="phylogenic")

res_clust <- cutree(res_hclust, k=2) %>% 
  as_tibble(rownames="Judge") %>% 
  rename(Cluster = value) %>% 
  mutate(Cluster = as.character(Cluster))

res_clust %>% 
  group_by(Cluster) %>% 
  count() %>% 
  ungroup()

mean_cluster <- consumer %>% 
  separate(Product, into = c("P", "Number"), sep = 1) %>% 
  mutate(Number = ifelse(nchar(Number) == 1, str_c("0", Number), Number)) %>% 
  unite(Product, P, Number, sep="") %>% 
  dplyr::select(Judge, Product, Liking=`end_liking 9pt`) %>%
  mutate(Liking = 10-Liking) %>% 
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

  ## Drivers of Liking

    ### Correlation
data_cor <- mean_cluster %>% 
  dplyr::select(-N) %>% 
  pivot_wider(names_from=Cluster, values_from=Liking) %>% 
  inner_join(senso_mean %>% dplyr::select(-c(Protein, Fiber)), by="Product") %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product")

res_cor <- cor(data_cor)

library(ggcorrplot)
res_cor_pmat <- cor_pmat(data_cor)
ggcorrplot(res_cor, type="full", p.mat=res_cor_pmat, insig="blank", lab=TRUE, lab_size=2)

  ## Simple and Quadratic Regression
data_reg <- mean_cluster %>%
  filter(Cluster == "1 (74)") %>% 
  dplyr::select(-N) %>%
  inner_join(senso_mean %>% dplyr::select(-c(Protein, Fiber)), by = "Product") %>%
  pivot_longer(Shiny:Melting, names_to = "Attribute", values_to = "Score") %>%
  mutate(Attribute = factor(Attribute, levels = colnames(senso_mean)[4:ncol(senso_mean)]))

res_reg <- data_reg %>% 
  nest_by(Attribute) %>% 
  mutate(lin_mod = list(lm(Liking ~ Score, data=data)), 
         quad_mod = list(lm(Liking ~ poly(Score, 2), data=data)))
  
lin <- res_reg %>% 
  summarise(broom::tidy(lin_mod)) %>% 
  ungroup() %>% 
  filter(term == "Score", p.value <= 0.05) %>% 
  pull(Attribute) %>% 
  as.character()

quad <- res_reg %>% 
  summarise(broom::tidy(quad_mod)) %>% 
  ungroup() %>% 
  filter(term == "poly(Score, 2)2", p.value <= 0.06) %>%
  pull(Attribute) %>% 
  as.character()

library(ggrepel)

df <- data_reg %>% 
  filter(Attribute %in% unique(c(lin,quad)))

p <- ggplot(df, aes(x=Score, y=Liking, label=Product))+
  geom_point(pch=20, cex=2)+
  geom_text_repel()+
  theme_bw()+
  facet_wrap(~Attribute, scales="free_x")
lm.mod <- function(df, quad){
  ifelse(df$Attribute %in% quad, "y~x+I(x^2)", "y~x")
}
p_smooth <- by(df, df$Attribute, 
               function(x) geom_smooth(data=x, method=lm, formula=lm.mod(x, quad=quad), se=FALSE))
p + p_smooth

  ## External Preference Mapping
senso <- senso_pca$ind$coord[,1:2] %>% 
  as_tibble(rownames="Product") %>% 
  arrange(Product) %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product")

consu <- consumer_wide %>% 
  arrange(Product) %>% 
  as.data.frame() %>% 
  column_to_rownames(var="Product")

    ### Running the PrefMap
library(SensoMineR)
PrefMap <- carto(Mat=senso, MatH=consu, regmod=1, graph.tree=FALSE, graph.corr=FALSE, graph.carto=TRUE)

    ### Rebuilding the plot in {ggplot2}
senso <- senso %>% 
  as_tibble(rownames="Product")

senso_sup <- senso_pca$ind.sup$coord %>% 
  as_tibble(rownames="Product")

dimnames(PrefMap$nb.depasse) <- list(round(PrefMap$f1,2), round(PrefMap$f2,2))
PrefMap_plot <- PrefMap$nb.depasse %>% 
  as_tibble(rownames="Dim1") %>% 
  pivot_longer(-Dim1, names_to="Dim2", values_to="Acceptance (%)") %>% 
  mutate(across(where(is.character), as.numeric))

ggplot()+
  geom_tile(data=PrefMap_plot, aes(x=Dim1, y=Dim2, fill=`Acceptance (%)`, color=`Acceptance (%)`))+
  geom_contour(data=PrefMap_plot, aes(x=Dim1, y=Dim2, z=`Acceptance (%)`), breaks=seq(0,100,10), colour="black")+
  geom_hline(yintercept=0, lty=2)+
  geom_vline(xintercept=0, lty=2)+
  geom_point(data=senso, aes(x=Dim.1, y=Dim.2), pch=20, col="black", cex=3)+
  geom_text_repel(data=senso, aes(x=Dim.1, y=Dim.2, label=Product), col="black")+
  geom_point(data=senso_sup, aes(x=Dim.1, y=Dim.2), pch=20, col="white", cex=3)+
  geom_text_repel(data=senso_sup, aes(x=Dim.1, y=Dim.2, label=Product), col="white")+
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=50)+
  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=50)+
  xlab(str_c("Dimension 1(",round(senso_pca$eig[1,2],1),"%)"))+
  ylab(str_c("Dimension 2(",round(senso_pca$eig[2,2],1),"%)"))+
  ggtitle("External Preference Mapping applied to the biscuits data","(The PrefMap is based on the quadratic model)")+
  theme_bw()
