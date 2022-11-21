library (tidyverse)

# Importing the Data ------------------------------------------------------

library(rattle)

wine <- rattle::wine %>% 
  as_tibble()

# Unsupervised ML ---------------------------------------------------------

  ## Dimensionality Reduction
library(FactoMineR)
library (factoextra)

res_pca <- PCA(wine, quali.sup=1, scale.unit=TRUE, graph=FALSE)

fviz_pca_biplot(res_pca, repel=TRUE, label="var", col.var="red", col.ind="black")

wine_reduced <- as_tibble(res_pca$ind$coord[,1:2])

  ## Clustering
fviz_nbclust(wine_reduced, kmeans, method = "wss")

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

  ## Data Preprocessing
model_recipe <- wine_train %>% 
  recipe(Type ~ .)

  ## Model definition
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

  ## Tuning the parameters
params_grid <- rf_spec %>%
  parameters() %>%
  update(mtry = mtry(range = c(1,2)),
         trees = trees(range = c(10,200))) %>% 
  grid_regular(levels=5)

tuning <- tune_grid(rf_wf, resamples=wine_cv, grid=params_grid)

autoplot(tuning)
params_best <- select_best(tuning, "roc_auc")

  ## Model Training
final_model <- rf_wf %>%
  finalize_workflow(params_best) %>%
  fit(wine_train)

  ## Model evaluation
obs_vs_pred <- wine_testing %>%
  bind_cols(predict(final_model, .))

cm <- conf_mat(obs_vs_pred, Type, .pred_class)
autoplot(cm, type = "heatmap")

obs_vs_pred_prob <- bind_cols(wine_testing %>% select(Type), predict(final_model, wine_testing, type = "prob")) %>% 
  mutate(Type = as.factor(Type))

accuracy(obs_vs_pred, truth = "Type", estimate = ".pred_class")
kap(obs_vs_pred, truth = "Type", estimate = ".pred_class")

library(modelStudio)

data_to_explain <- wine_train

explainer <- explain_tidymodels(
  model = final_model, 
  data = data_to_explain %>% 
    select(-Type),
  y = data_to_explain$Type
)

var_imp <- variable_importance(explainer, loss_function = loss_cross_entropy, type = "ratio")
plot(var_imp)
