# Exploring and preparing the Data

credit <- read.csv("credit.csv")

credit$default <- factor(credit$default, labels = c("Yes", "No"))

str(credit)

# Default rate
table(credit$default)  

# Creating random training and test datasets

set.seed(123)

train_sample <- sample(1000, 900)

# Division of rows into test and train Data

credit_train <- credit[train_sample,]

credit_train$default <- factor(credit_train$default)

credit_test <- credit[-train_sample,]



install.packages("C50")

library(C50)

credit_model <- C5.0(credit_train[-17], credit_train$default)

summary(credit_model)

# Evaluating model performance

credit_pred <- predict(credit_model, credit_test)

require(gmodels)

CrossTable(credit_test$default, credit_pred, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE,
          dnn = c('actual default', 'predicted default'))

# Boosting tree accuracy

credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)

credit_boost_pred_10 <- predict(credit_boost10, credit_test)

CrossTable(credit_test$default, credit_boost_pred_10, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# Adding cost matrix 

matrix_dimensions <- list(c("No", "Yes"), c("No", "Yes"))
names(matrix_dimensions) <- c("predicted", "actual")

error_cost <- matrix(c(0,1,4,0), nrow = 2, dimnames = matrix_dimensions)

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)

credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))





