# ============================================================================
# Section 00 - SET UP
# ============================================================================
# Data manipulation
library(tidyverse)
library(lubridate)
library(tidymodels)
# Data visualisation
library(ggplot2)
library(scales)
#library(viridis)
# Statistical analysis
library(psych)
library(rstatix)
# Correlation plots
library(corrplot)
# Tables
library(knitr)
# Themes
library(hrbrthemes)
library(xgboost)

# ============================================================================
# Colour theme
# ============================================================================
# Colors
dissertation_colors <- c(
  primary = "#1B3A5C",
  secondary = "#2E6F95",
  accent = "#F2A541",
  background = "#F7F9FB",
  text = "#1F1F1F"
)
# Theme
theme_dissertation <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F7F9FB", color = NA),
    panel.background = element_rect(fill = "#F7F9FB", color = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "#1B3A5C", face = "bold"),
    plot.subtitle = element_text(color = "#2E6F95"),
    axis.title = element_text(color = "#1F1F1F"),
    axis.text = element_text(color = "#1F1F1F"),
    legend.title = element_text(color = "#1B3A5C", face = "bold")
  )
# Discrete color/fill scales
scale_fill_dissertation <- function(...) scale_fill_manual(values = unname(dissertation_colors), ...)
scale_color_dissertation <- function(...) scale_color_manual(values = unname(dissertation_colors), ...)

# ============================================================================
# Section 01 - Data Import and Cleaning
# ============================================================================
data <- read.csv("KaggleV2-May-2016.csv")
head(data)
str(data)
dim(data)
names(data)
colSums(is.na(data))
#Quick summary
summary(data)
# ---- Dates ----
data$ScheduledDay <- as.Date(data$ScheduledDay)
data$AppointmentDay <- as.Date(data$AppointmentDay)
# ---- Lead time (engineered predictor; Samorani & LaGanga, 2015; Deina et al., 2024) ----
data$LeadTime <- as.numeric(data$AppointmentDay - data$ScheduledDay)
# ---- Invalid-record summary ----
invalid_records <- data.frame(
  Issue = c("Negative Age (-1)", "Negative Lead Time"),
  Count = c(sum(data$Age == -1, na.rm = TRUE),
    sum(data$LeadTime < 0, na.rm = TRUE)),
  Action = c("Removed", "Removed")
)
# ---- Remove invalid records (1 negative age, 5 negative lead times -> 110,521) ----
n_before <- nrow(data)
data <- data %>% filter(Age >= 0, LeadTime >= 0)
cat("Removed", n_before - nrow(data), "invalid records (",
  n_before, "->", nrow(data), ")\n")
# ---- Target ----
data$No_show_numeric <- ifelse(data$No.show == "Yes", 1, 0)
# ---- Booking features ----
data$LeadTimeBand <- cut(data$LeadTime,
  breaks = c(-1, 0, 3, 7, 14, 30, 1000),
  labels = c("Same day", "1-3 days", "4-7 days",
    "8-14 days", "15-30 days", "30+ days"))
# ---- Demographic features (Andersen: predisposing) ----
data$AgeGroup <- cut(data$Age,
  breaks = c(-1, 12, 18, 35, 50, 65, 120),
  labels = c("Child (0-12)", "Teen (13-18)", "Young Adult (19-35)",
    "Middle Age (36-50)", "Older Adult (51-65)", "Senior (65+)"))
# ---- Clinical features (Andersen: need) ----
data$ComorbidityCount <- data$Diabetes + data$Hipertension +
  data$Alcoholism
data$Handicap_Any <- ifelse(data$Handcap > 0, 1, 0) # binary given sparsity of levels 1-4
saveRDS(data, "data_clean.rds")
cat("Saved data_clean.rds:", nrow(data), "rows,", ncol(data), "cols\n")
#Numeric summary
data %>%
  select(Age, LeadTime, ComorbidityCount, PriorNoShows, PriorNoShowRate) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(n, mean, sd, min, median, max) %>%
  gt()

# ============================================================================
# 02_split.R : Patient-level train/test split + grouped CV folds
# The whole pipeline's validity rests on this step: no patient may appear on
# both sides of the split, and no patient may span CV folds.
# GEN AI used assistively, split design and rationale are the author's own.
# ============================================================================
library(tidymodels)
set.seed(40481810) # reproducibility
data <- readRDS("data_clean.rds")
# ---- Patient-level split (group = PatientId, NOT row) ----
# group_initial_split keeps every appointment from a given patient wholly
# inside either train or test
split <- group_initial_split(data, group = PatientId, prop = 0.80)
train <- training(split)
test <- testing(split)
# ---- Grouped CV folds on the training set (for tuning in 03) ----
# Same leakage rule extended into cross-validation: a patient sits in one fold.
folds <- group_vfold_cv(train, group = PatientId, v = 5)
# ---- Sanity checks ----
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")
cat("Patient overlap (must be 0):",
  length(intersect(train$PatientId, test$PatientId)), "\n")
cat("Train no-show rate:", round(mean(train$No_show_numeric) * 100, 1),
  "%\n")
cat("Test no-show rate:", round(mean(test$No_show_numeric) * 100, 1),
  "%\n")
saveRDS(split, "split.rds")
saveRDS(train, "train.rds")
saveRDS(test, "test.rds")
saveRDS(folds, "folds.rds")
# Descriptive statistics for key numeric variables
descriptive_stats <- data %>%
  select(Age, LeadTime, No_show_numeric, ComorbidityCount,
    Handicap_Any) %>%
  psych::describe() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  select(Variable, n, mean, sd, median, min, max)
descriptive_stats %>%
  gt::gt() %>%
  gt::tab_header(title = " Descriptive Statistics")

# ============================================================================
# 03_models.R : Recipe (Andersen-grouped) + baseline ladder + model ladder
# Predictive half of the predict-to-prescribe arc (Bertsimas & Kallus, 2020).
# Features grouped by Andersen (1995): predisposing / enabling / need.
# Gen AI used for debugging while using join
# ============================================================================
library(tidymodels)
set.seed(40481810)
train <- readRDS("train.rds")
test <- readRDS("test.rds")
folds <- readRDS("folds.rds")
# Target as a factor
prep_target <- function(df) {
  df %>% mutate(No_show = factor(No_show_numeric, levels = c(1, 0),
    labels = c("no_show", "attended")))
}
train <- prep_target(train)
test <- prep_target(test)
# ---- Leakage-safe prior no-show history (built post-split) ----
add_prior_history <- function(df) {
  base <- df %>%
    select(AppointmentID, PatientId, ScheduledDay, AppointmentDay,
      No_show_numeric)
  prior <- base %>%
    select(AppointmentID, PatientId, ScheduledDay) %>%
    left_join(base %>% select(PatientId,
      AppointmentDay_prior = AppointmentDay,
      noshow_prior = No_show_numeric),
      by = "PatientId", relationship = "many-to-many") %>%
    filter(AppointmentDay_prior < ScheduledDay) %>%
    group_by(AppointmentID) %>%
    summarise(PriorAppts = n(), PriorNoShows = sum(noshow_prior), .groups = "drop")
  df %>%
    left_join(prior, by = "AppointmentID") %>%
    mutate(PriorAppts = coalesce(PriorAppts, 0L),
      PriorNoShows = coalesce(PriorNoShows, 0L),
      HasHistory = if_else(PriorAppts > 0, 1L, 0L),
      PriorNoShowRate = if_else(PriorAppts > 0, PriorNoShows / PriorAppts,
        0))
}
train <- add_prior_history(train)
test <- add_prior_history(test)
# Rebuild folds from the FULLY-PREPARED train
set.seed(40481810)
folds <- group_vfold_cv(train, group = PatientId, v = 5)
# ---- Predictor Set: features grouped by Andersen's three families ----
noshow_recipe <-
  recipe(No_show ~
      LeadTime +
      Age + Gender +
      Scholarship + SMS_received +
      Hipertension + Diabetes + Alcoholism + ComorbidityCount +
      Handicap_Any +
      PriorNoShowRate + PriorNoShows + HasHistory,
    data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())
# ---- Metric set ----
noshow_metrics <- metric_set(pr_auc, roc_auc, brier_class)

# ============================================================================
# BASELINE LADDER
# ============================================================================
# Baseline 1: majority-class (predicts "attended" for everyone).
base_rate <- mean(train$No_show_numeric)
cat("Base no-show rate (train):", round(base_rate * 100, 1), "%\n")
cat("Accuracy of always-guess-attended:", round((1 - base_rate) * 100, 1),
  "%\n")
# Baseline 2: lead-time-only logistic regression.
lead_only_recipe <- recipe(No_show ~ LeadTime, data = train)
lr_spec <- logistic_reg() %>% set_engine("glm")
lead_only_fit <- workflow() %>%
  add_recipe(lead_only_recipe) %>%
  add_model(lr_spec) %>%
  fit(data = train)
lead_only_preds <- augment(lead_only_fit, test)
cat("\n-- Lead-time-only logistic (test) --\n")
print(noshow_metrics(lead_only_preds, truth = No_show, .pred_no_show,
  estimate = .pred_class))

# ============================================================================
# DIAGNOSTIC for MODEL 3 : Penalised Logistic Regression
# Continous variables
# 1. continuous only
# 2. continuous+bands
# 3. continuous+splines
# ============================================================================
# especially for enet for the appropriate curve redemption
enet_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")
enet_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  mixture(range = c(0, 1)),
  levels = c(penalty = 20, mixture = 5)
)
# Current recipe
rec_now <- noshow_recipe
# Old recipe with Bands
rec_bands <- recipe(No_show ~
    LeadTime + LeadTimeBand +
    Age + AgeGroup + Gender +
    Scholarship + SMS_received +
    Hipertension + Diabetes + Alcoholism + ComorbidityCount +
    Handicap_Any +
    PriorNoShowRate + PriorNoShows + HasHistory,
  data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())
# 3. Spline recipe
rec_spline <- noshow_recipe %>%
  step_ns(LeadTime, deg_free = 5) %>%
  step_ns(Age, deg_free = 5)
# Helper: 5-fold grouped CV PR-AUC
cv_pr <- function(rec, label) {
  fit_resamples(
    workflow() %>% add_recipe(rec) %>% add_model(lr_spec),
    resamples = folds, metrics = noshow_metrics
  ) %>% collect_metrics() %>%
    filter(.metric == "pr_auc") %>%
    transmute(recipe = label, pr_auc = round(mean, 4), se = round(std_err, 4))
}
cat("\n== Linear model, three feature representations (5-fold grouped CV) ==\n")
bind_rows(
  cv_pr(rec_now, "continuous only (now)"),
  cv_pr(rec_bands, "continuous + bands (before)"),
  cv_pr(rec_spline, "continuous + splines")
) %>% print()

# ============================================================================
# A/B CHECK :does the elastic net survive spline inputs?
# ============================================================================
enet_recipe_spline <- noshow_recipe %>%
  step_ns(LeadTime, deg_free = 5) %>%
  step_ns(Age, deg_free = 5) %>%
  step_normalize(all_numeric_predictors())
enet_wf_spline <- workflow() %>%
  add_recipe(enet_recipe_spline) %>%
  add_model(enet_spec)
set.seed(40481810)
enet_spline_tuned <- tune_grid(
  enet_wf_spline,
  resamples = folds,
  grid = enet_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
# Selected by ROC-AUC
enet_spline_best <- select_best(enet_spline_tuned, metric = "roc_auc")
cat("\n== Elastic net + splines : chosen hyperparameters ==\n")
print(enet_spline_best)
cat("\n== Elastic net + splines :CV metrics ==\n")
collect_metrics(enet_spline_tuned) %>%
  inner_join(enet_spline_best, by = c("penalty", "mixture", ".config")) %>%
  select(.metric, mean, std_err) %>%
  print()
# Distinction preds
collect_predictions(enet_spline_tuned, parameters = enet_spline_best) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()

# ============================================================================
# MODEL SEQUENCE: Model 2: full logistic regression (Andersen recipe)
# ============================================================================
# Bundled the recipe + the glm logistic spec into one workflow.
lr_full_wf <- workflow() %>%
  add_recipe(noshow_recipe) %>%
  add_model(lr_spec)
# Cross-validated on the grouped folds. No test data is touched here.
lr_full_cv <- fit_resamples(
  lr_full_wf,
  resamples = folds,
  metrics = noshow_metrics,
  control = control_resamples(save_pred = TRUE)
)
# Mean performance across the 5 folds
cat("\n-- Full logistic regression (5-fold grouped CV) --\n")
collect_metrics(lr_full_cv) %>%
  select(.metric, mean, std_err) %>%
  print()

# ============================================================================
# Model 3 : elastic net (penalised logistic regression)
# ============================================================================
enet_recipe <- noshow_recipe %>%
  step_normalize(all_numeric_predictors())
enet_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")
enet_wf <- workflow() %>%
  add_recipe(enet_recipe) %>%
  add_model(enet_spec)
# A small grid of dial settings to try. 20 combinations is plenty here.
enet_grid <- grid_regular(
  penalty(range = c(-4, 0)), # penalty strength, on a log scale
  mixture(range = c(0, 1)), # 0 = ridge, 1 = lasso, between = elastic net
  levels = c(penalty = 20, mixture = 5)
)
# Try every grid setting on the 5 folds, scoring on your metric set.
set.seed(40481810)
enet_tuned <- tune_grid(
  enet_wf,
  resamples = folds,
  grid = enet_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
# Best setting by PR-AUC, and its CV scores.
cat("\n-- Elastic net: best by PR-AUC (5-fold grouped CV) --\n")
show_best(enet_tuned, metric = "pr_auc", n = 1) %>% print()
#Three simple checks for the penalised Logistic model
# 1. PR-AUC across the whole grid
collect_metrics(enet_tuned) %>%
  filter(.metric == "pr_auc") %>%
  arrange(desc(mean)) %>%
  select(penalty, mixture, mean) %>%
  print(n = 15)
# 2. The critical test: at the smallest penalty, elastic net = plain logistic,
collect_metrics(enet_tuned) %>%
  filter(.metric == "pr_auc") %>%
  arrange(penalty) %>%
  select(penalty, mixture, mean) %>%
  print(n = 10)
# 3. Number of distinct probabilities the "best" model actually produces
collect_predictions(enet_tuned,
  parameters = select_best(enet_tuned, metric = "pr_auc")) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4)),
    min = min(.pred_no_show),
    max = max(.pred_no_show)) %>%
  print()
#Fix for the model(penalty)
# Select the best penalty/mixture by ROC-AUC
enet_best <- select_best(enet_tuned, metric = "roc_auc")
print(enet_best)
# Report all three metrics
cat("\n-- Elastic net: best by ROC-AUC, all metrics (5-fold grouped CV) --\n")
collect_metrics(enet_tuned) %>%
  inner_join(enet_best, by = c("penalty", "mixture", ".config")) %>%
  select(.metric, mean, std_err) %>%
  print()

# ============================================================================
# Model 4: single decision tree (rpart)
# ============================================================================
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")
tree_wf <- workflow() %>%
  add_recipe(noshow_recipe) %>%
  add_model(tree_spec)
# 25 combinations: 5 pruning strengths x 5 depths.
tree_grid <- grid_regular(
  cost_complexity(range = c(-6, -1)), # log10 scale, smaller = prune less
  tree_depth(range = c(2, 12)),
  levels = 5
)
set.seed(40481810)
tree_tuned <- tune_grid(
  tree_wf,
  resamples = folds,
  grid = tree_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
# Select on ROC-AUC
tree_best <- select_best(tree_tuned, metric = "roc_auc")
print(tree_best)
cat("\n-- Decision tree: best by ROC-AUC, all metrics (5-fold grouped CV) --\n")
collect_metrics(tree_tuned) %>%
  inner_join(tree_best, by = c("cost_complexity", "tree_depth", ".config")) %>%
  select(.metric, mean, std_err) %>%
  print()
# TIE CHECK
collect_predictions(tree_tuned, parameters = tree_best) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()

# ============================================================================
# Model 5: random forest (ranger)
# ============================================================================
rf_spec <- rand_forest(
  mtry = tune(), # how many features each split may consider
  min_n = tune(), # smallest group a split is allowed to create
  trees = 500 # number of trees in the forest (fixed)
) %>%
  set_engine("ranger") %>%
  set_mode("classification")
rf_wf <- workflow() %>%
  add_recipe(noshow_recipe) %>%
  add_model(rf_spec)
# Explicit mtry range
rf_grid <- grid_regular(
  mtry(range = c(2, 12)),
  min_n(range = c(50, 2000)),
  levels = 4
)
library(ranger)
set.seed(40481810)
rf_tuned <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
rf_best <- select_best(rf_tuned, metric = "roc_auc")
print(rf_best)
cat("\n-- Random forest: best by ROC-AUC, all metrics (5-fold grouped CV) --\n")
collect_metrics(rf_tuned) %>%
  inner_join(rf_best, by = c("mtry", "min_n", ".config")) %>%
  select(.metric, mean, std_err) %>%
  print()
# Tie check
collect_predictions(rf_tuned, parameters = rf_best) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()

# ============================================================================
# Model 6: gradient boosting (XGBoost) - properly tuned
# Refs: Chen & Guestrin (2016, KDD); Probst et al. (2019, JMLR).
# Gen AI was used here for appropriate tuning and validation
# ============================================================================
xgb_spec <- boost_tree(
  trees = 1000,
  stop_iter = 25,
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  mtry = tune(),
  sample_size = tune()
) %>%
  set_engine("xgboost", validation = 0.2) %>% # inner 20% val for early stopping
  set_mode("classification")
xgb_wf <- workflow() %>%
  add_recipe(noshow_recipe) %>%
  add_model(xgb_spec)
set.seed(40481810)
xgb_grid <- grid_space_filling(
  tree_depth(range = c(3, 10)),
  learn_rate(range = c(-2.5, -0.5)),
  min_n(range = c(5, 200)),
  loss_reduction(range = c(-3, 1)),
  mtry(range = c(2, 12)),
  sample_size = sample_prop(range = c(0.5, 1.0)),
  size = 40
)
set.seed(40481810)
xgb_tuned <- tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
xgb_best <- select_best(xgb_tuned, metric = "roc_auc")
cat("\n== XGBoost (properly tuned) : best settings ==\n"); print(xgb_best)
cat("\n== XGBoost (properly tuned) : CV metrics ==\n")
collect_metrics(xgb_tuned) %>%
  inner_join(xgb_best, by = c("mtry","min_n","tree_depth","learn_rate",
    "loss_reduction","sample_size",".config")) %>%
  select(.metric, mean, std_err) %>% print()
# Tie check
collect_predictions(xgb_tuned, parameters = xgb_best) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()
#The finak xgbm model
final_xgb_wf <- finalize_workflow(xgb_wf, xgb_best)
final_fit <- fit(final_xgb_wf, data = train)

# ============================================================================
# Model 7: neural network (single-hidden-layer MLP, nnet)
# A non-tree, non-spline learner
# ============================================================================
library(nnet)
mlp_spec <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = 200
) %>%
  set_engine("nnet", MaxNWts = 5000) %>%
  set_mode("classification")
mlp_wf <- workflow() %>%
  add_recipe(enet_recipe) %>% # normalised continuous recipe
  add_model(mlp_spec)
# Modest grid: 5 capacities x 4 decay strengths = 20 configs.
mlp_grid <- grid_regular(
  hidden_units(range = c(2, 20)),
  penalty(range = c(-4, 0)), # log10 weight-decay, ~0.0001 to 1
  levels = c(hidden_units = 5, penalty = 4)
)
set.seed(40481810)
mlp_tuned <- tune_grid(
  mlp_wf,
  resamples = folds,
  grid = mlp_grid,
  metrics = noshow_metrics,
  control = control_grid(save_pred = TRUE)
)
mlp_best <- select_best(mlp_tuned, metric = "roc_auc") # tie-robust, as everywhere
cat("\n== Neural net (MLP) : chosen hyperparameters ==\n"); print(mlp_best)
cat("\n== Neural net (MLP) : CV metrics (5-fold grouped) ==\n")
collect_metrics(mlp_tuned) %>%
  inner_join(mlp_best, by = c("hidden_units", "penalty", ".config")) %>%
  select(.metric, mean, std_err) %>%
  print()
# Tie check
collect_predictions(mlp_tuned, parameters = mlp_best) %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()

# ============================================================================
# STAGE 1 WRAP-UP, Step 1 : Calibration of the final model (XGBoost)
# Uses out-of-fold predictions already saved in xgb_tuned
# ============================================================================
library(probably)
# Pull the out-of-fold predictions for the BEST xgb setting only.
xgb_oof <- collect_predictions(xgb_tuned, parameters = xgb_best)
# --- Calibration plot: predicted probability vs actual no-show rate ---
cal_plot <- xgb_oof %>%
  cal_plot_breaks(truth = No_show, estimate = .pred_no_show, num_breaks = 10)
print(cal_plot)
# --- Numeric calibration check
xgb_oof %>%
  mutate(bin = ntile(.pred_no_show, 10)) %>%
  group_by(bin) %>%
  summarise(
    n = n(),
    mean_pred = round(mean(.pred_no_show), 3), # what the model claimed
    actual_rate = round(mean(No_show == "no_show"), 3), # what really happened
    .groups = "drop"
  ) %>%
  print()
# Fit the finalised XGBoost on all of train, predict on test once.
set.seed(40481810)
test_preds <- augment(final_fit, new_data = test)
cat("\n== FINAL XGBOOST : TEST SET (honest, unbiased) ==\n")
noshow_metrics(test_preds, truth = No_show, .pred_no_show, estimate = .pred_class) %>%
  select(.metric, .estimate) %>%
  print()
# Tie check on the test predictions.
test_preds %>%
  summarise(distinct_preds = n_distinct(round(.pred_no_show, 4))) %>%
  print()

# ============================================================================
# STAGE 1 WRAP-UP, Step 3 : SHAP interpretability on the final XGBoost
# ============================================================================
library(shapviz)
xgb_engine <- extract_fit_engine(final_fit)
prepped <- extract_recipe(final_fit)
set.seed(40481810)
shap_rows <- test %>% slice_sample(n = 3000)
X <- bake(prepped, new_data = shap_rows) %>%
  select(-No_show) %>%
  as.matrix()
sv <- shapviz(xgb_engine, X_pred = X)
X_full <- bake(prepped, new_data = test) %>%
  select(-No_show) %>%
  as.matrix()
sv_full <- shapviz(xgb_engine, X_pred = X_full)
## The performance ladder diagram
library(tidyverse)
# Lead-time-only, re-scored via 5-fold grouped CV
lead_only_cv <- fit_resamples(
  workflow() %>% add_recipe(lead_only_recipe) %>% add_model(lr_spec),
  resamples = folds,
  metrics = noshow_metrics,
  control = control_resamples(save_pred = FALSE)
)
# Helpers
rung_from_fit <- function(res, label)
collect_metrics(res) %>% transmute(model = label, .metric, mean, std_err)
rung_from_tune <- function(tuned, label) {
  cfg <- select_best(tuned, metric = "roc_auc")$.config
  collect_metrics(tuned) %>% filter(.config == cfg) %>%
    transmute(model = label, .metric, mean, std_err)
}
ladder <- bind_rows(
  rung_from_fit (lead_only_cv, "Lead-time only"),
  rung_from_fit (lr_full_cv, "Logistic (full)"),
  rung_from_tune(enet_tuned, "Elastic net"),
  rung_from_tune(tree_tuned, "Decision tree"),
  rung_from_tune(rf_tuned, "Random forest"),
  rung_from_tune(xgb_tuned, "XGBoost"),
  rung_from_tune(mlp_tuned, "Neural net (MLP)")
) %>%
  mutate(model = factor(model, levels = c(
    "Lead-time only","Logistic (full)","Elastic net",
    "Decision tree","Random forest","XGBoost",
    "Neural net (MLP)")))
pr_ladder <- ladder %>% filter(.metric == "pr_auc")
ggplot(pr_ladder, aes(x = model, y = mean)) +
  geom_hline(yintercept = base_rate, linetype = "dashed",
    colour = dissertation_colors[["text"]]) +
  geom_text(data = data.frame(model = factor("Lead-time only",
    levels = levels(pr_ladder$model)),
    y = base_rate + 0.010),
    aes(x = model, y = y),
    label = "No-skill (base rate)", hjust = 0, vjust = 0,
    size = 3.2, colour = dissertation_colors[["text"]],
    inherit.aes = FALSE) +
  geom_linerange(aes(ymin = mean - std_err, ymax = mean + std_err),
    colour = dissertation_colors[["secondary"]]) +
  geom_point(size = 3, colour = dissertation_colors[["primary"]]) +
  labs(
    title = "Predictive Performance across Model families",
    subtitle = "PR-AUC, 5-fold grouped CV (mean ± SE)",
    x = NULL, y = "PR-AUC"
  ) +
  theme_dissertation +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
library(gt); library(tidyverse)
ladder %>%
  pivot_wider(names_from = .metric, values_from = mean, id_cols = model) %>%
  select(Model = model, PR_AUC = pr_auc,ROC_AUC = roc_auc,
    Brier = brier_class) %>%
  gt() %>%
  tab_header(title = md(
    "**Table 1. Predictive Performance Across Model Families**")) %>%
  fmt_number(c(PR_AUC,ROC_AUC,Brier), decimals = 3) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["accent"]],
      alpha = 0.25),
      cell_text(weight = "bold")),
    locations = cells_body(rows = Model == "XGBoost")) %>%
  tab_options(table.background.color = dissertation_colors[["background"]],
    data_row.padding = px(6))
## Caliberation ( Reliability diagram)
cal_tbl <- xgb_oof %>%
  mutate(bin = ntile(.pred_no_show, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(.pred_no_show),
    actual_rate = mean(No_show == "no_show"),
    n = n(),
    se = sqrt(actual_rate * (1 - actual_rate) / n),
    .groups = "drop"
  )
ggplot(cal_tbl, aes(x = mean_pred, y = actual_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
    colour = dissertation_colors[["text"]]) +
  geom_linerange(aes(ymin = actual_rate - 1.96 * se,
    ymax = actual_rate + 1.96 * se),
    colour = dissertation_colors[["secondary"]]) +
  geom_line(colour = dissertation_colors[["secondary"]], linewidth = 0.8) +
  geom_point(size = 2.5, colour = dissertation_colors[["primary"]]) +
  coord_equal() +
  labs(
    title = "XGBoost calibration (out-of-fold)",
    subtitle = "Predicted vs actual no-show rate (by 10 equal-sized groups)",
    x = "Mean predicted no-show probability",
    y = "Actual no-show rate"
  ) +
  theme_dissertation
## SHAP Importance
library(shapviz)
sv_importance(sv_full, kind = "bar", max_display = 12,
  fill = dissertation_colors[["secondary"]]) +
  labs(title = "Feature importance (mean |SHAP|)") +
  theme_dissertation
# Beeswarm : keep default colour (it encodes feature value; recolouring strips meaning)
sv_importance(sv_full, kind = "beeswarm", max_display = 12) +
  labs(title = "SHAP value distribution by feature") +
  theme_dissertation

# ============================================================================
# STAGE 2 : Cost-sensitive threshold (Elkan, 2001)
# DNA cost £120 (academic NHS estimate), intervention swept 5p (SMS) to £5 (phone).
# t* = c_i / (c_i + c_d). Uses test_preds (final XGBoost on the sealed test set).
# ============================================================================
# Cost of a targeting policy at a given intervention cost (c_i) and DNA cost (c_d).
policy_cost <- function(preds, c_i, c_d) {
  t_star <- c_i / (c_i + c_d) # Elkan cost-optimal threshold
  flagged <- preds$.pred_no_show >= t_star # patients we intervene on
  noshow <- preds$No_show == "no_show"
  TP <- sum(flagged & noshow) # flagged & would miss - DNA avoided
  FP <- sum(flagged & !noshow) # flagged & would attend - wasted reminder
  FN <- sum(!flagged & noshow) # not flagged & missed - pay full DNA
  tibble(
    c_i = c_i, c_d = c_d,
    threshold = round(t_star, 4),
    pct_flagged = round(100 * (TP + FP) / nrow(preds), 1),
    policy_cost = (TP + FP) * c_i + FN * c_d, # cost of ourtargeted policy
    do_nothing = sum(noshow) * c_d, # baseline: never intervene
    treat_all = nrow(preds) * c_i # baseline: remind everyone
  ) %>% mutate(saving_vs_nothing = do_nothing - policy_cost)
}
# --- Base case: SMS (5p) vs phone call (£5), both at £120 DNA ---
cat("\n== BASE CASE ==\n")
bind_rows(
  policy_cost(test_preds, c_i = 0.05, c_d = 120),
  policy_cost(test_preds, c_i = 5.00, c_d = 120)
) %>% print()
# --- Sensitivity sweep: intervention 5p → £5, across DNA £120 and £165 ---
cat("\n== SENSITIVITY SWEEP ==\n")
tidyr::expand_grid(c_i = c(0.05, 0.50, 1, 2, 3, 5),
  c_d = c(120, 165)) %>%
  purrr::pmap_dfr(~ policy_cost(test_preds, c_i = ..1, c_d = ..2)) %>%
  print(n = Inf)

# ============================================================================
# STAGE 2 (refined) : add reminder effectiveness e .
# ============================================================================
policy_cost_e <- function(preds, c_i, c_d, e) {
  t_star <- c_i / (c_i + c_d * e) # effectiveness added
  flagged <- preds$.pred_no_show >= t_star
  noshow <- preds$No_show == "no_show"
  TP <- sum(flagged & noshow)
  FP <- sum(flagged & !noshow)
  FN <- sum(!flagged & noshow)
  # Of TP no-shows we intervene on, only e*TP are actually prevented.
  dna_after <- (FN + TP * (1 - e)) # no-shows that still happen
  policy_cost <- (TP + FP) * c_i + dna_after * c_d
  tibble(c_i = c_i, c_d = c_d, effectiveness = e,
    threshold = round(t_star, 4),
    pct_flagged = round(100 * (TP + FP) / nrow(preds), 1),
    dna_prevented = round(TP * e),
    policy_cost = round(policy_cost),
    do_nothing = sum(noshow) * c_d) %>%
    mutate(saving_vs_nothing = do_nothing - policy_cost)
}
cat("\n== EFFECTIVENESS SENSITIVITY (DNA £120) ==\n")
tidyr::expand_grid(c_i = c(0.05, 1, 5),
  e = c(0.25, 0.30, 0.35)) %>%
  purrr::pmap_dfr(~ policy_cost_e(test_preds, c_i = ..1, c_d = 120, e = ..2)) %>%
  print(n = Inf)
tidyr::expand_grid(c_i = c(0.05, 1, 5), e = 0.30) %>%
  purrr::pmap_dfr(~ policy_cost_e(test_preds, c_i = ..1, c_d = 120, e = ..2)) %>%
  select(Reminder = c_i, Threshold = threshold, Flagged = pct_flagged,
    Prevented = dna_prevented, Cost = policy_cost, Saving = saving_vs_nothing) %>%
  gt() %>%
  tab_header(title = md("**Table 2. Cost-Sensitive Targeting at 30% Effectiveness (DNA £120)**")) %>%
  fmt_currency(c(Cost, Saving), currency = "GBP", decimals = 0) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_options(table.background.color = dissertation_colors[["background"]])

# ============================================================================
# STAGE 3 : Equity audit across the full threshold range.
# ============================================================================
# Disparity metrics between the two welfare groups at a single threshold t.
welfare_gap <- function(t) {
  test_preds %>%
    mutate(group = if_else(Scholarship == 1, "Welfare", "NotEnrolled"),
      flagged = .pred_no_show >= t,
      noshow = No_show == "no_show") %>%
    group_by(group) %>%
    summarise(
      flagged_rate = mean(flagged),
      TPR = sum(flagged & noshow) / sum(noshow),
      FPR = sum(flagged & !noshow) / sum(!noshow),
      .groups = "drop"
    ) %>%
    summarise(
      threshold = t,
      dp_gap = flagged_rate[group == "Welfare"] - flagged_rate[group ==
          "NotEnrolled"],
      tpr_gap = TPR[group == "Welfare"] - TPR[group == "NotEnrolled"],
      fpr_gap = FPR[group == "Welfare"] - FPR[group == "NotEnrolled"],
      pct_flagged_overall = mean(test_preds$.pred_no_show >= t) * 100
    )
}
threshold_grid <- seq(0.02, 0.60, by = 0.01)
equity_sweep <- purrr::map_dfr(threshold_grid, welfare_gap)
cat("\n== Welfare disparity across thresholds (gap = Welfare − NotEnrolled) ==\n")
equity_sweep %>%
  mutate(across(c(dp_gap, tpr_gap, fpr_gap), ~round(.x, 3)),
    pct_flagged_overall = round(pct_flagged_overall, 1)) %>%
  print(n = Inf)
equity_sweep %>%
  tidyr::pivot_longer(c(dp_gap, tpr_gap, fpr_gap),
    names_to = "metric", values_to = "gap") %>%
  mutate(metric = recode(metric,
    dp_gap = "Demographic parity gap",
    tpr_gap = "TPR gap (equal opportunity)",
    fpr_gap = "FPR gap")) %>%
  ggplot(aes(x = threshold, y = gap, colour = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed",
    colour = dissertation_colors[["text"]]) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = unname(dissertation_colors[c("primary","secondary","accent")])) +
  labs(
    title = "Group disparity depends on the operating threshold",
    subtitle = "Welfare-enrolled minus not-enrolled; 0 = parity. Final XGBoost, test set.",
    x = "Decision threshold", y = "Gap (proportion)",
    colour = NULL
  ) +
  theme_dissertation

# ============================================================================
# STAGE 3 : Equity audit, Step 1: group-wise rates by welfare status.
# Protected attribute: Scholarship (Bolsa Família welfare enrolment).
# ============================================================================
# Use a representative operating threshold from Stage 2 (£1 cost, 30% eff).
audit_threshold <- 0.027
equity_tbl_027 <- test_preds %>%
  mutate(
    group = if_else(Scholarship == 1, "Welfare-enrolled", "Not enrolled"),
    flagged = .pred_no_show >= audit_threshold,
    noshow = No_show == "no_show"
  ) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    base_rate = round(mean(noshow), 3), # actual no-show rate
    flagged_rate = round(mean(flagged), 3), # DEMOGRAPHIC PARITY
    # Equalised-odds components:
    TPR = round(sum(flagged & noshow) / sum(noshow), 3), # sensitivity
    FPR = round(sum(flagged & !noshow) / sum(!noshow), 3), # false alarm
    # Calibration proxy: mean predicted vs actual within group
    mean_pred = round(mean(.pred_no_show), 3),
    .groups = "drop"
  )
print(equity_tbl_027)
# Re-audit at the selective operating point (£5 intervention, 30% eff → t ≈ 0.12),
# where only ~65% are flagged and group disparities are not compressed by saturation.
audit_threshold <- 0.12
# (re-run the identical equity_tbl block above with this threshold)
equity_tbl_12 <- test_preds %>%
  mutate(
    group = if_else(Scholarship == 1, "Welfare-enrolled", "Not enrolled"),
    flagged = .pred_no_show >= audit_threshold,
    noshow = No_show == "no_show"
  ) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    base_rate = round(mean(noshow), 3), # actual no-show rate
    flagged_rate = round(mean(flagged), 3), # DEMOGRAPHIC PARITY
    # Equalised-odds components:
    TPR = round(sum(flagged & noshow) / sum(noshow), 3), # sensitivity
    FPR = round(sum(flagged & !noshow) / sum(!noshow), 3), # false alarm
    # Calibration proxy: mean predicted vs actual within group
    mean_pred = round(mean(.pred_no_show), 3),
    .groups = "drop"
  )
print(equity_tbl_12)
audit_threshold <- 0.29
# (re-run the identical equity_tbl block above with this threshold)
equity_tbl_29 <- test_preds %>%
  mutate(
    group = if_else(Scholarship == 1, "Welfare-enrolled", "Not enrolled"),
    flagged = .pred_no_show >= audit_threshold,
    noshow = No_show == "no_show"
  ) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    base_rate = round(mean(noshow), 3), # actual no-show rate
    flagged_rate = round(mean(flagged), 3), # DEMOGRAPHIC PARITY
    # Equalised-odds components:
    TPR = round(sum(flagged & noshow) / sum(noshow), 3), # sensitivity
    FPR = round(sum(flagged & !noshow) / sum(!noshow), 3), # false alarm
    # Calibration proxy: mean predicted vs actual within group
    mean_pred = round(mean(.pred_no_show), 3),
    .groups = "drop"
  )
print(equity_tbl_29)
# Threshold chosen for reporting
equity_tbl_29 %>%
  select(Group = group, n, Base_rate = base_rate,
    Flagged_rate = flagged_rate, TPR, FPR) %>%
  gt() %>%
  tab_header(title = md("**Table 3. Equity Audit at the Selective Operating Point (Threshold 0.29)**")) %>%
  fmt_number(c(Base_rate, Flagged_rate, TPR, FPR), decimals = 3) %>%
  fmt_number(n, decimals = 0, use_seps = TRUE) %>%
  tab_style(
    style = list(cell_fill(color = dissertation_colors[["primary"]]),
      cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels()) %>%
  tab_options(table.background.color = dissertation_colors[["background"]],
    data_row.padding = px(6))

# ============================================================================
# STAGE 3 : Equity audit Step 2: age subgroups (and welfare × age crossing)
# ============================================================================
audit_threshold <- 0.29 # same selective operating point
# --- 2a. Age bands alone ---
age_equity <- test_preds %>%
  mutate(flagged = .pred_no_show >= audit_threshold,
    noshow = No_show == "no_show") %>%
  group_by(AgeGroup) %>%
  summarise(
    n = n(),
    base_rate = round(mean(noshow), 3),
    flagged_rate = round(mean(flagged), 3),
    TPR = round(sum(flagged & noshow) / sum(noshow), 3),
    FPR = round(sum(flagged & !noshow) / sum(!noshow), 3),
    .groups = "drop"
  ) %>%
  mutate(reliable = if_else(n >= 300, "ok", "SMALL - interpret with caution"))
cat("\n== Age bands ==\n"); print(age_equity)
# --- 2b. Welfare × age crossing ---
cross_equity <- test_preds %>%
  mutate(group = if_else(Scholarship == 1, "Welfare", "NotEnrolled"),
    flagged = .pred_no_show >= audit_threshold,
    noshow = No_show == "no_show") %>%
  group_by(group, AgeGroup) %>%
  summarise(
    n = n(),
    n_flagged = sum(flagged),
    base_rate = round(mean(noshow), 3),
    flagged_rate = round(mean(flagged), 3),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Wilson (score) 95% CI on the flagged rate
    ci_low = round(prop.test(n_flagged, n, correct = FALSE)$conf.int[1], 3),
    ci_high = round(prop.test(n_flagged, n, correct = FALSE)$conf.int[2], 3),
    ci_halfwidth = round((ci_high - ci_low) / 2, 3),
    reliable = if_else(n >= 300, "ok", "SMALL : caution")
  ) %>%
  ungroup()
cat("\n== Welfare × age (Wilson 95% CIs; watch the small cells) ==\n")
print(cross_equity, n = 15)

# ============================================================================
# STAGE 4 : Comparison to NHS England (HES 2024-25 aggregate open data)
# ============================================================================
library(tidyverse)
library(readxl)
nhs_rep <- "hosp-epis-stat-outp-rep-tabs-2024-25-tab.xlsx"
# ---------------------------------------------------------------------------
# 4.1 HEADLINE DNA RATE : Summary Report 1 (appointments by type, 2014-25)
# ---------------------------------------------------------------------------
sr1 <- read_excel(nhs_rep, sheet = "Summary Report 1", skip = 5) %>%
  filter(!is.na(Year), str_detect(Year, "^\\d{4}-\\d{2}$"))
# NHS DNA rate on the Brazil data
nhs_headline <- sr1 %>%
  transmute(
    Year,
    Attendances = as.numeric(Attendances),
    DNAs = as.numeric(`Did not attends (DNAs)`),
    dna_rate = round(100 * DNAs / (Attendances + DNAs), 1)
  )
cat("\n== NHS outpatient DNA rate over time ==\n")
print(nhs_headline)
cat("\nNHS 2024-25 DNA rate:",
  nhs_headline %>% filter(Year == "2024-25") %>% pull(dna_rate), "%\n")
cat("Brazil (Vitoria 2016) no-show rate: ~20.2%\n")
# ---------------------------------------------------------------------------
# 4.2 AGE GRADIENT : Summary Report 3 (attendances by age & gender)
# ---------------------------------------------------------------------------
sr3_raw <- read_excel(nhs_rep, sheet = "Summary Report 3", skip = 5)
nhs_age <- sr3_raw %>%
  rename(Age = 1) %>%
  filter(!is.na(Age), str_detect(Age, "\\d")) %>%
  mutate(across(-Age, as.numeric)) %>%
  mutate(
    attended = rowSums(across(starts_with("Attended")), na.rm = TRUE),
    dna = rowSums(across(starts_with("Did Not Attend")), na.rm = TRUE),
    dna_rate = 100 * dna / (attended + dna)
  ) %>%
  mutate(Age = factor(Age, levels = Age)) %>%
  filter(!str_detect(Age, "Copyright")) %>%
  select(Age, attended, dna, dna_rate)
cat("\n== NHS DNA rate by age band ==\n")
print(nhs_age, n = Inf)
# --- Figure: NHS age gradient ---
ggplot(nhs_age, aes(x = Age, y = dna_rate, group = 1)) +
  geom_line(colour = dissertation_colors[["secondary"]], linewidth = 1) +
  geom_point(colour = dissertation_colors[["primary"]], size = 2) +
  labs(
    title = "NHS outpatient DNA rate by age band",
    subtitle = "HES England 2024-25",
    x = "Age band (years)", y = "DNA rate (%)"
  ) +
  theme_dissertation +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
