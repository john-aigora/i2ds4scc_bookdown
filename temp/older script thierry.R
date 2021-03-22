library(tidyverse)
library(readxl)

#===== Sensory Data =====

## Computing the LSMeans using broom and emmeans
library(emmeans)

v_att <- colnames(sensory)[4:ncol(sensory)]
v_prod <- colnames(sensory)[3]
v_juge <- colnames(sensory)[1]

senso_adjmean3 <- sensory %>% 
  pivot_longer(all_of(v_att), names_to="attribute", values_to="scores") %>% 
  nest_by(attribute) %>% 
  mutate(mod = list(emmeans(aov(as.formula(paste0("scores ~", v_prod, "+", v_juge)), data=data), v_prod))) %>%
  summarise(broom::tidy(mod)) %>% 
  dplyr::select(all_of(v_prod), attribute, estimate) %>% 
  pivot_wider(names_from=attribute, values_from=estimate) %>% 
  column_to_rownames(var=v_prod)
round(senso_adjmean3, 2)

#===== Consumer Test =====

  # From this file, there are 3 tables to import
  # Two of these tables need to have their name translated
    # -> for Biscuits, the translation is in Variable Names
    # -> for Produit final, the translation is in Var Names
  
  # For Variable Names, there are some attributes that have similar names (same question asked at different time point, after 1st bite and at the end)
  # I used the part of the header that informs to which part of the test it relates to and combined it to the name

#----- Import the Respondents Data with the correct names -----
var_names <- read_xlsx("thierry_code/Data/Resultats Bisens.xlsx", sheet="Variable Names", col_names=FALSE)
replacement <- ""
for (i in 1:nrow(var_names)){
  if (is.na(var_names[i,2])){
    if (!is.na(replacement)){
      var_names[i,2]=replacement
    }
  } else {
    replacement <- var_names[i,2]
  }
}
  
  # This first part of the code fills in the empty cell by repeating the test part from before (due to merge cells in Excel)

var_names <- var_names %>% 
  mutate(new_name = ifelse(!is.na(.data[[colnames(var_names)[4]]]), .data[[colnames(var_names)[4]]], .data[[colnames(var_names)[3]]]))
colnames(var_names) <- c("test_og", "test", "var_name_og", "var_name", "new_name")
var_names <- var_names %>% 
  unite(final_name, test, new_name, sep="_")
  
  # This part of the code generate the variable names after combining its name with its test part (test_name)
  # The dataset can finally be imported using the right variable names

biscuits <- read_xlsx("thierry_code/Data/Resultats Bisens.xlsx", sheet="Biscuits", col_names=var_names$final_name, skip=2) %>% 
  clean_names() %>% 
  mutate(prefix=ifelse(product<10, "P0", "P")) %>% 
  unite(product, prefix, product, sep="") %>% 
  as.data.frame()

  # Running the ANOVA with SNK (using {agricolae})
  # This partly reproduces the results from slide 10
  # Slide 11 cannot be reproduced as I do not know which variables were used for hunger start and hunger end
biscuits$judge <- as.factor(biscuits$judge)

firstOL_aov <- lm(firstbite_initial_ol~product+judge, data=biscuits)
anova(firstOL_aov)
require(agricolae)
firstOL_SNK <- SNK.test(firstOL_aov, "product", group=TRUE)$groups %>% 
  rownames_to_column(var="product")

ggplot(firstOL_SNK, aes(x=reorder(product, -firstbite_initial_ol), y=firstbite_initial_ol))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Initial Liking after a 1st bite")+
  ggtitle("Initial liking after first bite")+
  theme_bw()


#----- Import the Data related to the Time and Quantity eaten -----
quantity <- read_xlsx("thierry_code/Data/Resultats Bisens.xlsx", sheet="tps qtt consomme") %>% 
  clean_names() %>% 
  mutate(prefix=ifelse(product < 10, "P0", "P")) %>% 
  unite(product, prefix, product, sep="") %>% 
  mutate(nb_biscuits=round(nb_biscuits, 0))

  # Necessity to replace the Sample names with one that match the codes
  # Here, mutate is required because some values in the data are weird (they ate 7.91 biscuits)
  # So all the values are being replace by their value without decimals to simplify

qtt_biscuit_freq <- quantity %>% 
  dplyr::select(product, nb_biscuits) %>% 
  group_by(product) %>% 
  count(nb_biscuits)

  # The view below shows that the table now is logical
  # And the frequency is computed per sample, visualized, and represented using barchart

(qtt_biscuit_freq %>% pivot_wider(names_from=nb_biscuits, values_from=n, values_fill=list(n=0)))

  # Kind of reproduces Slide 9 although some of the data were weird...
  # Also Julien did it across all samples while I've split the results by sample
ggplot(qtt_biscuit_freq, aes(x=nb_biscuits, y=n, fill=product)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_x_continuous("Number of biscuits", limits=c(0,10), breaks=seq(0,10,1)) +
  theme_bw()

ggplot(qtt_biscuit_freq, aes(x=nb_biscuits, y=n, colour=product)) +
  geom_line(aes(group=product), lwd=1.2) +
  scale_x_continuous("Number of biscuits", limits=c(0,10), breaks=seq(0,10,1)) +
  ylab("Frequency")+
  ggtitle("Frequency of biscuits eaten","(Frequency are provided per sample)")+
  theme_bw()

ggplot(quantity, aes(x=nb_biscuits, colour=product, fill=product)) +
  geom_density(alpha=0.15) +
  scale_x_continuous("Number of biscuits", limits=c(0,10), breaks=seq(0,10,1)) +
  ylab("Density")+
  ggtitle("Density Distribution of Nb of Biscuits Eaten")+
  theme_bw()

  # Computing the average amount of biscuits eaten per biscuit
qq_biscuit_mean <- quantity %>% 
  dplyr::select(product, nb_biscuits) %>% 
  mutate(nb_biscuits=round(nb_biscuits, 0)) %>% 
  group_by(product) %>% 
  summarize(mean=mean(nb_biscuits))

#----- Liking at first bite and N biscuits eaten -----

  # Combining liking after first bite (biscuits) with quantity eaten (quantity)
  # The mergeing should be done using both judge and product
firstbite_ol <- biscuits %>% 
  dplyr::select(judge, product, firstbite_initial_ol) %>% 
  mutate(judge=paste0("J",judge)) %>% 
  as_tibble()
n_biscuits <- quantity %>% 
  dplyr::select(judge, product, nb_biscuits) %>% 
  as_tibble()

firstbiteOL_nbiscuit <- full_join(firstbite_ol, n_biscuits, by=c("judge", "product")) %>% 
  as.data.frame()

  # The two following lines of code are optional
  # They simply order the graph so that we have J1, J2, J3, J4, etc. instead of J1, J10, J100, J11, etc.
firstbiteOL_nbiscuit$judge <- as.factor(firstbiteOL_nbiscuit$judge)
juge <- str_sort(levels(firstbiteOL_nbiscuit$judge),numeric=TRUE)
nbjuge <- length(juge)
firstbiteOL_nbiscuit$judge <- factor(firstbiteOL_nbiscuit$judge, levels=juge)

  # Kind of recreate slide 13, just missing the coding based on samples contents...
ggplot(firstbiteOL_nbiscuit, aes(x=firstbite_initial_ol, y=nb_biscuits))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", se=FALSE)+
  scale_x_continuous("Initial liking", limits=c(0,10), breaks=seq(0,10,1))+
  scale_y_continuous("Nb biscuits eaten", limits=c(0,10), breaks=seq(0,10,1))+
  ggtitle("Liking after first bite vs. N biscuits eaten")+
  theme_bw()

  # Recreate slide 15 (except for overall graph)
ggplot(firstbiteOL_nbiscuit, aes(x=firstbite_initial_ol, y=nb_biscuits))+
  geom_point(pch=20, cex=2)+
  geom_smooth(method="lm", se=FALSE)+
  scale_x_continuous("Initial liking", limits=c(0,10), breaks=seq(0,10,1))+
  scale_y_continuous("Nb biscuits eaten", limits=c(0,10), breaks=seq(0,10,1))+
  ggtitle("Liking after first bite vs. N biscuits eaten", "(Results split by assessor)")+
  theme_bw()+
  facet_wrap(~judge)

  # Extracting the results for the individual regression
  # Would be eventually used to create slide 16, although I'm not sure I would know how to do (and did not look for it)
  # Note that my results are slightly different from Julien's
    # -> e.g. J01 is not a Hedonist for me but is for Julien
    # I'm getting 35 Hedonist whereas Julien's getting 40 (it seems my 35 are all incl. withing J's 40)

judge_lm <- matrix(0, nbjuge, 4, 
                   dimnames=list(juge, c("Intercept","Slope", "R2", "p-value")))
for (j in 1:nbjuge){
  res_lm <- firstbiteOL_nbiscuit %>% 
    filter(judge == juge[j]) %>% 
    as.data.frame() %>% 
    lm(data=., formula=nb_biscuits~firstbite_initial_ol)
  
  judge_lm[juge[j], 1:2] <- coefficients(res_lm)
  judge_lm[juge[j], 3] <- summary(res_lm)$r.squared
  judge_lm[juge[j], 4] <- pf(summary(res_lm)$fstatistic, 1, summary(res_lm)$df[2], lower.tail=FALSE)[1]
}

judge_lm <- as.data.frame(judge_lm) %>% 
  rownames_to_column(var="judge") %>% 
  mutate(behavior = ifelse(`p-value` <= 0.05, "Hedonist", "Indifferent"))

table(judge_lm$behavior)

#----- Import the Liking and Final Sample selected -----
var_names <- read_xlsx("thierry_code/Data/Resultats Bisens.xlsx", sheet="Var Names", col_names=FALSE)
var_names <- var_names %>% 
  mutate(new_name = ifelse(!is.na(.data[[colnames(var_names)[2]]]), .data[[colnames(var_names)[2]]], .data[[colnames(var_names)[1]]]))

selected_prod <- read_xlsx("thierry_code/Data/Resultats Bisens.xlsx", sheet="Produit final", col_names=var_names$new_name, skip=1) %>% 
  clean_names()

#===== Preference Mapping =====
axes=senso_pca$ind$coord[,1:2]

  # First bite initial liking
conso_firstbite <- biscuits %>% 
  dplyr::select(judge, product, firstbite_initial_ol) %>% 
  pivot_wider(names_from=judge, values_from=firstbite_initial_ol) %>% 
  column_to_rownames(var="product") %>% 
  as.data.frame()

firstbite_carto <- carto(Mat=axes, MatH=conso_firstbite, regmod=1)

  # First bite final liking
conso_firstfinal <- biscuits %>% 
  dplyr::select(judge, product, firstbite_final_ol) %>% 
  pivot_wider(names_from=judge, values_from=firstbite_final_ol) %>% 
  column_to_rownames(var="product") %>% 
  as.data.frame()

firstfinal_carto <- carto(Mat=axes, MatH=conso_firstfinal, regmod=1)

  # Final
conso_final <- biscuits %>% 
  dplyr::select(judge, product, liking_ol) %>% 
  pivot_wider(names_from=judge, values_from=liking_ol) %>% 
  column_to_rownames(var="product") %>% 
  as.data.frame()

final_carto <- carto(Mat=axes, MatH=conso_final, regmod=1)
