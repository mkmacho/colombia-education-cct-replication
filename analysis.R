rm(list = ls())

### Installs packages if not already installed, then loads packages 
list.of.packages <- c("SuperLearner", "glmnet", "xgboost", # "ggplot2"
                      "tidyverse", "haven", "foreign", 
                      "rpart", "rpart.plot", "remotes",
                      "randomForest", "knitr", "purrr", "caret",
                      "stargazer", "biostat3")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(
  new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

### Install causalTree package from Susan Athey's github
remotes::install_github("susanathey/causalTree", upgrade = F, quiet = T)
library(causalTree)

### Read in data. Change filepath accordingly.
work_dir <- '/Users/mcamacho/Desktop/Barrera_2011/Replication'
setwd(work_dir)

data <- read_dta('Public_Data_AEJApp_2010-0132.dta') %>%
  filter(suba == 0, survey_selected == 1, grade %in% c(6:10))

# NOTE: is it strange that e.g. `s_teneviv`, `s_estcivil` not FACTORS?

# Return data corresponding to default, savings, or tertiary experiments
treat_data <- function(dat, treat = 1) {
  if (treat == 2) return(filter(dat, T2 == 1))        # Default (basic)
  else if (treat == 3) return(filter(dat, T3 == 1))   # Savings
  return(filter(dat, T1 == 1))                        # Tertiary
}

my_data <- treat_data(data, treat = 1)
stopifnot(identical(my_data$treatment, my_data$T1_treat))

# Split data into folds
folds = createFolds(1:nrow(my_data), k = 2)

y1 <- my_data[folds[[1]], ]$at_msamean # Define outcome variable
y2 <- my_data[folds[[2]], ]$at_msamean

X1 <- my_data[folds[[1]], ]$treatment
X2 <- my_data[folds[[2]], ]$treatment

# List of covariates (controls)
covs <- c("s_teneviv", "s_utilities", "s_durables", "s_infraest_hh",
          "s_age_sorteo", "s_age_sorteo2", "s_years_back", "s_sexo", # Age + sex
          "s_estcivil", "s_single", "s_edadhead", "s_yrshead", # Martial + family
          "s_tpersona", "s_num18", "s_estrato", "s_puntaje",   # 
          "s_ingtotal", "grade", "suba", "s_over_age")
W1 <- my_data[folds[[1]], covs]
W2 <- my_data[folds[[2]], covs]
# `suba` and `grade` likely irrelevant, LM will automatically ignore?

### OLS with interaction terms (straightforward)
sl_lm = SuperLearner(Y = y1, 
                     X = data.frame(X=X1, W1, W1*X1), 
                     family = gaussian(), 
                     SL.library = "SL.lm", 
                     cvControl = list(V=0))

summary(sl_lm$fitLibrary$SL.lm_All$object)

# We can use our linear model to predict the outcome with treatment 
# and without treatment. Taking the difference between these predictions 
# gives us an estimate for the CATE where CATE=E(Y|X=1,W)−E(Y|X=0,W).

### Creates a vector of 0s and a vector of 1s of length n (hack for later usage)
zeros <- function(n) {
  return(integer(n))
}
ones <- function(n) {
  return(integer(n)+1)
}

ols_pred_0s <- predict(sl_lm, data.frame(X=zeros(nrow(W2)), W2, W2*zeros(nrow(W2))), onlySL = T)
ols_pred_1s <- predict(sl_lm, data.frame(X=ones(nrow(W2)), W2, W2*ones(nrow(W2))), onlySL = T)

cate_ols <- ols_pred_1s$pred - ols_pred_0s$pred


### Post-selection with Lasso

# Step 1: select variables using lasso.

lasso = create.Learner("SL.glmnet", params = list(alpha = 1), name_prefix="lasso")

get_lasso_coeffs <- function(sl_lasso) {
  return(coef(sl_lasso$fitLibrary$lasso_1_All$object, s="lambda.min")[-1,])
}  

predict_y_lasso <- SuperLearner(Y = y1,
                                X = data.frame(X=X1, W1, W1*X1), 
                                family = gaussian(),
                                SL.library = lasso$names, 
                                cvControl = list(V=0))

kept_variables <- which(get_lasso_coeffs(predict_y_lasso)!=0)

predict_x_lasso <- SuperLearner(Y = X1,
                                X = data.frame(W1), 
                                family = gaussian(),
                                SL.library = lasso$names, 
                                cvControl = list(V=0))

# +1 to include X
kept_variables2 <- which(get_lasso_coeffs(predict_x_lasso)!=0) + 1 

# Step 2: Apply OLS to the chosen variables 
# (also make sure X is included if not selected by the lasso). 
# If none of your interaction terms are selected, then lasso has not found 
# any treatment heterogeneity.

sl_post_lasso <- SuperLearner(Y = y1,
                              X = data.frame(
                                X=X1, W1, W1*X1
                              )[, c(kept_variables, kept_variables2)], 
                              family = gaussian(),
                              SL.library = "SL.lm", 
                              cvControl = list(V=0))

summary(sl_post_lasso$fitLibrary$SL.lm_All$object)

# CATE for post-selection Lasso
postlasso_pred_0s <- predict(sl_post_lasso, data.frame(X=zeros(nrow(W2)), 
    W2, W2*zeros(nrow(W2)))[, c(kept_variables, kept_variables2)], onlySL = T)
postlasso_pred_1s <- predict(sl_post_lasso, data.frame(X=ones(nrow(W2)), 
    W2, W2*ones(nrow(W2)))[, c(kept_variables, kept_variables2)], onlySL = T)

cate_postlasso <- postlasso_pred_1s$pred - postlasso_pred_0s$pred


###  Causal Trees ###

# Recall we grow a causal tree in order to mimimise −∑iτ̂ (Wi)2, where τ(W)=E(Y(1)−Y(0)|W=w)

# Get formula
tree_fml <- as.formula(paste("y", paste(names(W1), collapse = ' + '), sep = " ~ "))

# causal tree
causal_tree <- causalTree(formula = tree_fml,
                          data = data.frame(y=y1, W1),
                          treatment = X1,
                          split.Rule = "CT", # causal tree
                          split.Honest = F, 
                          split.alpha = 1, 
                          cv.option = "CT",
                          cv.Honest = F,
                          split.Bucket = T, 
                          # each bucket contains bucketNum treated, bucketNum control units
                          bucketNum = 5, 
                          bucketMax = 100, 
                          # number of observations in treatment and control on leaf
                          minsize = 100) 

rpart.plot(causal_tree, roundint = F)

# honest causal tree
honest_tree <- honest.causalTree(formula = tree_fml,
                                 data = data.frame(y=y1, W1),
                                 treatment = X1,
                                 est_data = data.frame(y=y2, W2),
                                 est_treatment = X2,
                                 split.alpha = 0.5,
                                 split.Rule = "CT",
                                 split.Honest = T,
                                 cv.alpha = 0.5,
                                 cv.option = "CT",
                                 cv.Honest = T,
                                 split.Bucket = T,
                                 bucketNum = 5,
                                 bucketMax = 100, 
                                 minsize = 100) 

rpart.plot(honest_tree, roundint = F)

opcpid <- which.min(honest_tree$cp[, 4]) 
opcp <- honest_tree$cp[opcpid, 1]
honest_tree_prune <- prune(honest_tree, cp = opcp)

rpart.plot(honest_tree_prune, roundint = F)

### Estimate SEs (error here as pruned tree has no leaves) ###
leaf1 <- as.factor(round(predict(honest_tree,
                                 newdata = data.frame(y=y2, W2),
                                 type = "vector"), 4))

leaf2 <- as.factor(round(predict(honest_tree_prune,
                                 newdata = data.frame(y=y2, W2),
                                 type = "vector"), 4))

# Run linear regression that estimate the treatment effect magnitudes and standard errors
honest_ols_1 <- lm(y ~ leaf + X * leaf - X -1, data = data.frame(y=y2, X=X2, leaf=leaf1, W2))
summary(honest_ols_1)

honest_ols_2 <- lm(y ~ leaf + X * leaf - X -1, data = data.frame(y=y2, X=X2, leaf=leaf2, W2))
summary(honest_ols_2)


### Test for sig. heterogeneity
# lincom(honest_ols_1, specification = "leaf0.0448:X - leaf0.0094:X", level=0.95)


### Produce table

stargazer(honest_ols_1, 
          align=TRUE, 
          dep.var.labels=c("Attendance"),
          no.space=TRUE, 
          title = "Estimating the Treatment Effect Magnitudes and Standard Errors"
)

# CATE for honest tree
cate_honesttree <- predict(honest_tree_prune, newdata = data.frame(y=y2, W2), type = "vector")


### Causal forest (essentially a random forest of honest causal trees)

causalforest <- causalForest(tree_fml,
                             data=data.frame(y=y1, W1), 
                             treatment=X1, 
                             split.Rule="CT", 
                             split.Honest=T,  
                             split.Bucket=T, 
                             bucketNum = 5,
                             bucketMax = 100, 
                             cv.option="CT", 
                             cv.Honest=T, 
                             minsize = 2, 
                             split.alpha = 0.5, 
                             cv.alpha = 0.5,
                             sample.size.total = floor(nrow(y1) / 2), 
                             sample.size.train.frac = .5,
                             mtry = ceiling(ncol(W1)/3), 
                             nodesize = 5, 
                             num.trees = 10, 
                             ncov_sample = ncol(W1), 
                             ncolx = ncol(W1))


# CATE for causal forests
cate_causalforest <- predict(causalforest, newdata = data.frame(y=y2, W2), type = "vector")

### We have estimated the CATE on split 2 using OLS, Post-selection Lasso, 
### Honest Trees and Causal Forests. For the former three, we have selected 
### variables W where there are heterogenous treatement effects. 
### For the latter, there is less interpretability.
### How do the estimates of the CATE compare?
  
## Compare Heterogeneity
het_effects <- data.frame(ols = cate_ols, 
                          post_selec_lasso = cate_postlasso, 
                          causal_tree = cate_honesttree, 
                          causal_forest = cate_causalforest)

# Set range of the x-axis
xrange <- range(c(het_effects[, 1], het_effects[, 2], het_effects[, 3], het_effects[, 4]))

# Set the margins (two rows, three columns)
par(mfrow = c(2, 4))

hist(het_effects[, 1], main = "OLS", xlim = xrange)
hist(het_effects[, 2], main = "Post-selection Lasso", xlim = xrange)
hist(het_effects[, 3], main = "Causal tree", xlim = xrange)
hist(het_effects[, 4], main = "Causal forest", xlim = xrange)

# Summary statistics
summary_stats <- do.call(data.frame, 
                         list(mean = apply(het_effects, 2, mean),
                              sd = apply(het_effects, 2, sd),
                              median = apply(het_effects, 2, median),
                              min = apply(het_effects, 2, min),
                              max = apply(het_effects, 2, max)))

summary_stats

####--------#####
###### BLP ######
####--------#####
### 1ST: PROPENSITY SCORE ON SET 1 & PREDICT ON SET 2.
prop_score_w1 = SuperLearner(Y = X1, 
                             X = W1, 
                             newX = W2, 
                             family = binomial(), 
                             SL.library = list("SL.xgboost", "SL.randomForest"), 
                             cvControl = list(V=0))

# Predict propensity score
p <- prop_score_w1$SL.predict

### 2ND: Define pseudo-treatment D = X2 - ^p(W2).
D <- X2-p

### 3RD: Obtain the CATE on set 1 & predict on set 2.
sl_y = SuperLearner(Y = y1, 
                    X = data.frame(X=X1, W1), 
                    family = gaussian(), 
                    SL.library = list("SL.xgboost"), 
                    cvControl = list(V=0))

pred_y1 = predict(sl_y, newdata=data.frame(X=ones(nrow(W2)), W2))

pred_0s <- predict(sl_y, data.frame(X=zeros(nrow(W2)), W2), onlySL = T)
pred_1s <- predict(sl_y, data.frame(X=ones(nrow(W2)), W2), onlySL = T)

cate <- pred_1s$pred - pred_0s$pred

### 4TH: Define the interaction between D and C. 
C = cate-mean(cate)

### 5TH: Merge the result & obtain the final model. 
df_blp <- data.frame(Y=y2, W2, D, C, p)

Wnames <- paste(colnames(W2), collapse="+")
fml <- paste("Y ~",Wnames,"+ D + D:C")
model <- lm(fml, df_blp, weights = 1/(p*(1-p)))

# Now, subset using a nice table.
table_from_blp <- function(model) {
  thetahat <- model %>% 
    .$coefficients %>%
    .[c("D","D:C")]
  
  # Confidence intervals
  cihat <- confint(model)[c("D","D:C"),]
  
  res <- tibble(coefficient = c("beta1","beta2"),
                estimates = thetahat,
                ci_lower_90 = cihat[,1],
                ci_upper_90 = cihat[,2])
  return(res) }

table_from_blp(model)

####--------#####
###### GATES ######
####--------#####
### 1ST: Obtain propensity score on set 1 and predict in set 2.
# Done before, but repeated for code readability
prop_score_w1_gates = SuperLearner(Y = X1, 
                                   X = W1, 
                                   newX = W2, 
                                   family = binomial(), 
                                   SL.library = list("SL.xgboost", "SL.randomForest"), 
                                   cvControl = list(V=0))

# Predict propensity score    
p_gates <- prop_score_w1_gates$SL.predict


### 2ND: obtain the residualized pseudo-treatment.
D_gates <- X2 - p_gates

### STEP 3RD: Get CATE on set 1 and predict on set 2. 
sl_y_gates = SuperLearner(Y = y1, 
                          X = data.frame(X=X1, W1), 
                          family = gaussian(), 
                          SL.library = "SL.xgboost", 
                          cvControl = list(V=0))

pred_y1_gates = predict(sl_y_gates, newdata=data.frame(X=ones(nrow(W2)), W2))

pred_0s_gates <- predict(sl_y_gates, data.frame(X=zeros(nrow(W2)), W2), onlySL = T)
pred_1s_gates <- predict(sl_y_gates, data.frame(X=ones(nrow(W2)), W2), onlySL = T)

cate_gates <- pred_1s_gates$pred - pred_0s_gates$pred

### 4TH: sort and divide the cate estimates into 10 tiles, and call this object G. 
# Divide observations into 10 tiles ##### 10 is arbitrarilty chosen for now
G <- data.frame(cate_gates) %>% # replace cate with the name of your predictions object
  ntile(10) %>%  # Divide observations into 10-tiles
  factor()

### STEP 5TH: Create a dataframe with y2, W2, D, G and p. Regress Y on group membership variables and covariates. 
df_gates <- data.frame(Y=y2, W2, D, G, p)

Wnames_gates <- paste(colnames(W2), collapse="+")
fml_gates <- paste("Y ~",Wnames_gates,"+ D:G")
model_gates <- lm(fml_gates, df_gates, weights = 1/(p*(1-p))) 
summary(model_gates)

# Return nice output for GATES
table_from_gates <-function(model_gates) {
  thetahat_gates <- model_gates %>% 
    .$coefficients %>%
    .[c("D:G1","D:G2","D:G3","D:G4", "D:G5", "D:G6", "D:G7", "D:G8", "D:G9", "D:G10")]
  
  # Confidence intervals
  cihat_gates <- confint(model_gates)[c("D:G1","D:G2","D:G3","D:G4", "D:G5", "D:G6", "D:G7", "D:G8", "D:G9", "D:G10"), ]
  
  res_gates <- tibble(coefficient = c("gamma1","gamma2","gamma3","gamma4","gamma5",
                                      "gamma6","gamma7","gamma8","gamma9","gamma10"),
                      estimates = thetahat_gates,
                      ci_lower_90 = cihat_gates[,1],
                      ci_upper_90 = cihat_gates[,2])
  
  return(res_gates)
}

table_from_gates(model_gates)
