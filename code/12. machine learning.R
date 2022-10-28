library (tidyverse)
library (rattle)
library (FactoMineR)
library (factoextra)
library (tidymodels)

# Unsupervised Machine Learning ---------------------------------

# load data

wine.fl <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(wine.fl,header = F)

# Names of the variables

wine.names=c("Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium",
             "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins",
             "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
colnames(wine)[2:14]=wine.names
colnames(wine)[1]="Wine_type"

wine_dataset <- tibble(wine)

# PCA
pca_results <- PCA(wine_dataset, scale.unit = TRUE)

pca_eig <- fviz_eig(pca_results, addlabels = TRUE, ylim = c(0,50))

pca_plot <- fviz_pca_biplot(pca_results, repel = TRUE,
                            label = "var",
                            col.var = "red",
                            col.ind = "black")

pca_plot

reduced_dataset <- data.frame(pca_results$ind$coord[, 1:2]) %>%
  tibble()

# Cluster

fviz_nbclust(reduced_dataset, kmeans, method = "wss")


set.seed(123)

km_dw <- kmeans(reduced_dataset, 3, nstart = 20)

fviz_cluster(
  list(data = reduced_dataset, cluster = km_dw$cluster),
  ellipse.type = "norm",
  geom = "point",
  stand = FALSE
)

# Supervised Machine Learning ---------------------------------

# Preparing data for ML
wine_classification_dataset <- wine_dataset %>% 
  mutate(Wine_type = as.factor(Wine_type))

# Sampling the data
initial_split <- initial_split(data = wine_classification_dataset, strata = "Wine_type", prop = 0.7)

wine_train <- training(initial_split)
wine_testing <- testing(initial_split)

# Cross Validation
wine_cv <- wine_train %>% vfold_cv(v = 5,strata = Wine_type)

# Choose ML method using "recipe"
# Random forest model definition
model_recipe <- wine_train %>% 
  recipe(Wine_type ~ .)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("classification") %>% 
  set_engine(engine = "ranger")

# Workflow
rf_wf <- workflow() %>%
  add_recipe(model_recipe) %>% 
  add_model(rf_spec)

# Tuning the parameter
params_grid <- rf_spec %>%
  parameters() %>%
  update(mtry = mtry(range = c(1, 2)),
         trees = trees(range = c(10, 200))) %>% 
  grid_regular(levels = 5)

# Best parameters searching
tuning <- tune_grid(
  rf_wf,
  resamples = wine_cv,
  grid = params_grid
)

autoplot(tuning)

params_best <- select_best(tuning, "roc_auc")

# Finalize model 
final_model <- rf_wf %>%
  finalize_workflow(params_best) %>%
  fit(wine_train)

# Assess Model Quality
validation_data_pred <- wine_testing %>%
  bind_cols(predict(final_model, .))

cm <- conf_mat(validation_data_pred, Wine_type, .pred_class)

autoplot(cm, type = "heatmap")