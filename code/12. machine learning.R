library (tidyverse)

# Importing the Data ------------------------------------------------------

library(rattle)

wine <- rattle::wine %>% 
  as_tibble()

# Unsupervised ML ---------------------------------------------------------

  ## Dimensionality Reduction
library (FactoMineR)
res_pca <- PCA(wine, quali.sup=1, scale.unit=TRUE, graph=FALSE)

library(factoextra)
fviz_eig(res_pca, addlabels=TRUE, ylim=c(0,50))
fviz_pca_biplot(pca_results, repel=TRUE, label="var", col.var="red", col.ind="black")

pca_plot <- fviz_pca_biplot(pca_results, repel = TRUE,
                            label = "var",
                            col.var = "red",
                            col.ind = "black")

wine_reduced <- as_tibble(pca_results$ind$coord[,1:2])

  ## Clustering
fviz_nbclust(wine_reduced, kmeans, method="wss")

set.seed(123)
wine_kmeans <- kmeans(wine_reduced, centers=3, nstart=20)

fviz_cluster(list(data=wine_reduced, cluster=wine_kmeans$cluster),
             ellipse.type="norm", geom="point", stand=FALSE)

# Supervised ML -----------------------------------------------------------

library(tidymodels)

  ## Sampling the data
wine_split <- initial_split(data=wine, strata="Type", prop=0.7)
wine_train <- training(wine_split)
wine_testing <- testing(wine_split)

  ## Cross Validation
wine_cv <- wine_train %>% 
  vfold_cv(v=5, strata=Type)

  ## Create the Model
model_recipe <- wine_train %>% 
  recipe(Type ~ .)

  ## ML algorithm specification
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("classification") %>% 
  set_engine(engine = "ranger")

  ## Workflow
rf_wf <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(rf_spec)

  ## Parameters tuning
params_grid <- rf_spec %>%
  parameters() %>%
  update(mtry=mtry(range=c(1,2)), trees=trees(range=c(10,200))) %>% 
  grid_regular(levels = 5)

tuning <- tune_grid(rf_wf, resamples=wine_cv, grid=params_grid)

autoplot(tuning)
params_best <- select_best(tuning, "roc_auc")

  ## Model Training
final_model <- rf_wf %>%
  finalize_workflow(params_best) %>%
  fit(wine_train)

  ## Assess Model Quality
obs_vs_pred <- wine_testing %>%
  bind_cols(predict(final_model, .))

cm <- conf_mat(obs_vs_pred, Type, .pred_class)
autoplot(cm, type = "heatmap")
