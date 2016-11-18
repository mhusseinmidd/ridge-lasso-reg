#Shrikage Methods:
#11/2/2016 MH

# The goal of this r. script is to explore two prominant shrinkage methods,
# namely ridge and Lasso regressions. Specifically, we will compute them
# manually, choose an appropriate lamda, and compare their performance to
# that of an OLS. We use the OkCupid dataset to predict the sex of respondants.

# Load Packages and Data:
#Load Packages:
library(dplyr)
library(ggplot2)
library(glmnet)
select <- dplyr::select
library(plyr)

library(okcupiddata)
data(profiles)
View(profiles)

set.seed(76)

profiles <-
  profiles %>%
  sample_frac(0.1) %>%  #Necessary to convert data to matrix later
  select(sex, height, age, body_type, education) %>%
  na.omit() %>%
  # make row names explicit column
  tibble::rownames_to_column(var="id") %>%
  # convert to tibble format: data frame, but that limits the number of rows
  # displayed when entered into console
  tbl_df() %>%
  mutate (education= revalue(education, c("dropped out of law school"=
                                            "Bachelor's",
                                          "dropped out of masters program"=
                                            "Bachelor's",
                                          "dropped out of med school"=
                                            "Bachelor's",
                                          "dropped out of ph.d program"=
                                            "Bachelor's",
                                          "graduated from college/university"=
                                            "Bachelor's",
                                          "college/university"=
                                            "Bachelor's",
                                          "graduated from law school"=
                                            "Graduate or Professional Degree",
                                          "graduated from masters program"=
                                            "Graduate or Professional Degree",
                                          "graduated from med school"=
                                            "Graduate or Professional Degree",
                                          "graduated from ph.d program"=
                                            "Graduate or Professional Degree",
                                          "law school"=
                                            "Graduate or Professional Degree",
                                          "masters program" =
                                            "Graduate or Professional Degree",
                                          "med school" =
                                            "Graduate or Professional Degree",
                                          "ph.d program"=
                                            "Graduate or Professional Degree",
                                          "working on masters program" =
                                            "Graduate or Professional Degree",
                                          "working on ph.d program" =
                                            "Graduate or Professional Degree",
                                          "working on med school" =
                                            "Graduate or Professional Degree",
                                          "working on law school" =
                                            "Graduate or Professional Degree",
                                          "working on college/university"=
                                            "Some College",
                                          "graduated from two-year college"=
                                            "Some College",
                                          "dropped out of college/university"=
                                            "Some College",
                                          "working on two-year college"=
                                            "Some College",
                                          "two-year college" =
                                            "Some College",
                                          "dropped out of two-year college"=
                                            "Some College",
                                          "graduated from high school"=
                                            "High School",
                                          "high school"=
                                            "High School",
                                          "dropped out of high school"=
                                            "High School",
                                          "working on high school"=
                                            "High School",
                                          "graduated from space camp"=
                                            "Space Camp",
                                          "working on space camp"=
                                            "Space Camp",
                                          "dropped out of space camp"=
                                            "Space Camp",
                                          "space camp"=
                                            "Space Camp")
  ),
  smc=ifelse(education=="Some College", 1,0),
  grad=ifelse(education=="Graduate or Professional Degree", 1,0),
  spc=ifelse(education=="Space Camp", 1,0),
  ath=ifelse(body_type=="athletic", 1,0),
  cur=ifelse(body_type=="curvy", 1,0),
  fit=ifelse(body_type=="fit", 1,0),
  ff=ifelse(body_type=="full figured", 1,0),
  is_female=ifelse(sex=="f", 1,0)
  )

# 1) Standardizing Predictors:
# Standardize CONTINUOUS predictors using formula on p 217:
# Both methods we use require a standardizarion of predictors:

profiles <- profiles %>%
  mutate(
    height=scale(height),
    age=scale(age)
  )

# Split dataset for cross validation later on:
set.seed(76)
train <- sample_frac(profiles, 0.7)
test <- anti_join(profiles, train, by="id")

# 2) Ridge Regression;
# The glmnet function requires an x matrix as well as a y vector:
# The glmnet function has an alpha argument that determines what type
# of model is fit. If alpha=0, then a ridge regression model is fit,
# and if alpha=1 then a lasso model is fit. We first fit a ridge regression model:

# Create the matrix (all variables except the predicted)
train <- train %>%
  select(-id) %>%
  select(is_female, height, age)
x_ridge <- train %>%
  model.matrix(is_female~., data=.)

# Create the preicted variable's vector:
y_ridge <- train$is_female

# Vary the lambda values by creating seq ranging from 10^10 to 10^-2:
grid <- 10^seq(10,-2,length=100)

# Fit the ridge model:
ridge.mod <- glmnet(x_ridge,
                    y_ridge,
                    alpha=0,
                    lambda=grid,
                    standardize = FALSE ) #Since we did this manually

# Associated with each value of lambda is a vector of ridge regression coefficients,
# stored in a matrix that can be accessed by coef().

# 3) Choosing a Lamda:
# We move on to choosing a lambda that minimizes the training MSE:


# More conveniently, the glmnet conveniently has a cross validation function that
# fits ridge regression model on training data, returns lambda, and min Lambda.

cv.out <- cv.glmnet(x_ridge,
                    y_ridge,
                    alpha=0,
                    lambda = grid)

plot(ridge.mod)           # Draw plot of training MSE as a function of lambda
bestlam <- cv.out$lambda.min # Select lamda that minimizes training MSE

# Apply the lambda value to our testing dataset:
test <- test %>%
  select(-id) %>%
  select(is_female, height, age)
x_ridge_test <- test %>%
  model.matrix(is_female~., data=.)
y_test <- test$is_female

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x_ridge_test) # Use best lambda to predict test data

mean((ridge.pred-y_test)^2)                           # Calculate test MSE


# 4) Lasso Regression:

# 5) OLS:

# 6) Comparison:
