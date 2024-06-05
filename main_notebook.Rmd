```{r}
# Install readxl
install.packages("readxl")

# Install caret
install.packages("caret")

# Install randomForest
install.packages("randomForest")

# Install gbm
install.packages("gbm")

# Install e1071
install.packages("e1071")

# Install pROC
install.packages("pROC")

# Install ROCR
install.packages("ROCR")
```


```{r}
# Import Libraries
library(readxl)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(pROC)
```


```{r}
# Data Loading
df <- read_excel("Data.xls")
```


```{r}
# Select only the specified nine variables
selected_variables <- c(
  "ratingCeo",
  "ratingBusinessOutlook",
  "ratingWorkLifeBalance",
  "ratingCultureAndValues",
  "ratingDiversityAndInclusion",
  "ratingSeniorLeadership",
  "ratingRecommendToFriend",
  "ratingCareerOpportunities",
  "ratingCompensationAndBenefits"
)
```


```{r}
df <- df[, selected_variables]
```


```{r}
# Convert specified variables (categorical) to factors (numbers)
df$ratingCeo <- as.factor(df$ratingCeo)
df$ratingBusinessOutlook <- as.factor(df$ratingBusinessOutlook)
df$ratingRecommendToFriend <- as.factor(df$ratingRecommendToFriend)
```


```{r}
str(df)
```


```{r}
# Remove rows with any missing data in the selected columns
df <- df[complete.cases(df), ]
```


```{r}
str(df)
```


```{r}
# Set seed for reproducibility
set.seed(1337)
```


```{r}
# Split the data into training and testing sets
trainIndex <- createDataPartition(df$ratingRecommendToFriend, p = .7, list = FALSE)
train_set <- df[trainIndex, ]
test_set <- df[-trainIndex, ]
```


```{r}
# Display the structure of both sets
cat("Structure of the Training Set:\n")
str(train_set)
cat("\nStructure of the Testing Set:\n")
str(test_set)
```


```{r}
# Define parameter grid
rf_param_grid <- expand.grid(
  mtry = 1:10  # Number of variables randomly sampled as candidates at each split
)
```


```{r}
# Set up train control with kappa metric
train_control <- trainControl(
  method = "cv",    # Cross-validation
  number = 5,       # 5-fold cross-validation
  summaryFunction = multiClassSummary, # Use kappa for multi-class
  classProbs = TRUE # Needed for multiClassSummary
)
```



```{r}
# Train Random Forest model
set.seed(1337)
rf_model <- train(
  ratingRecommendToFriend ~ .,  # Predict ratingRecommendToFriend using all other variables
  data = train_set,
  method = "rf",
  tuneGrid = rf_param_grid, # Pass the parameter grid
  trControl = train_control, 
  metric = "Kappa",  # Evaluate using Kappa
  ntree = 1000       # Number of trees
)
```


```{r}
# Predict the ratingRecommendToFriend on the testing set
rf_predictions <- predict(rf_model, test_set)
```


```{r}
# Calculate the confusion matrix
conf_matrix <- confusionMatrix(rf_predictions, test_set$ratingRecommendToFriend)
print(conf_matrix)
```
As viewed from the above confusion matrix the model is performing relatively well with an accuracy of 90% and a Kappa score of 0.7622. The number of False Positives and False Negatives is also relatively low (104 & 109 respectively). And also with the confidence interval of 95% it's  believed that the model will perform well on unseen data.

```{r}
# Plot the confusion matrix
plot_confusion_matrix <- function(cm) {
  cm_table <- as.data.frame(cm$table)
  colnames(cm_table) <- c("Prediction", "Reference", "Freq")
  ggplot(data = cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue") +
    geom_text(aes(label = Freq), vjust = 1) +
    theme_minimal() +
    labs(title = "Confusion Matrix", x = "Reference", y = "Prediction")
}
```


```{r}
plot_confusion_matrix(conf_matrix)
```


```{r}
# Convert factor levels to numeric for AUC calculation
test_labels <- as.numeric(as.factor(test_set$ratingRecommendToFriend))
levels(test_set$ratingRecommendToFriend) <- 1:length(levels(test_set$ratingRecommendToFriend))
pred_prob <- predict(rf_model, test_set, type = "prob")
```


```{r}
# Compute ROC for each class
roc_list <- lapply(1:ncol(pred_prob), function(i) {
  roc(test_labels == i, pred_prob[, i])
})
```


```{r}
# Plot ROC curves
plot_roc_curves <- function(roc_list) {
  colors <- rainbow(length(roc_list))
  plot(roc_list[[1]], col = colors[1], main = "ROC Curves for Each Class", legacy.axes = TRUE)
  for (i in 2:length(roc_list)) {
    plot(roc_list[[i]], col = colors[i], add = TRUE, legacy.axes = TRUE)
  }
  legend("bottomright", legend = levels(test_set$ratingRecommendToFriend), col = colors, lty = 1)
}

plot_roc_curves(roc_list)
```


```{r}
# Calculate AUC for each class and print
auc_values <- sapply(roc_list, auc)
names(auc_values) <- levels(test_set$ratingRecommendToFriend)
print(auc_values)
```


```{r}
# Data Loading
df <- read_excel("Data.xls")
```


```{r}
# Select only the specified nine variables
selected_variables <- c(
  "ratingCeo",
  "ratingBusinessOutlook",
  "ratingWorkLifeBalance",
  "ratingCultureAndValues",
  "ratingDiversityAndInclusion",
  "ratingSeniorLeadership",
  "ratingRecommendToFriend",
  "ratingCareerOpportunities",
  "ratingCompensationAndBenefits"
)
```


```{r}
df <- df[, selected_variables]
```


```{r}
# Convert specified variables to factors
df$ratingCeo <- as.factor(df$ratingCeo)
df$ratingBusinessOutlook <- as.factor(df$ratingBusinessOutlook)
df$ratingRecommendToFriend <- as.factor(df$ratingRecommendToFriend)
```


```{r}
str(df)
```


```{r}
# Remove rows with any missing data in the selected columns
df <- df[complete.cases(df), ]
```


```{r}
# Set seed for reproducibility
set.seed(1337)
```


```{r}
# Split the data into training and testing sets
trainIndex <- createDataPartition(df$ratingRecommendToFriend, p = .7, list = FALSE)
train_set <- df[trainIndex, ]
test_set <- df[-trainIndex, ]
```



```{r}
selected_variables <- c(
  "ratingCeo",
  "ratingBusinessOutlook",
  "ratingWorkLifeBalance",
  "ratingCultureAndValues",
  "ratingDiversityAndInclusion",
  "ratingSeniorLeadership",
  "ratingCareerOpportunities",
  "ratingCompensationAndBenefits"
)
```



```{r}
X_train = data.matrix(train_set[,selected_variables])                  # independent variables for train
y_train = train_set[,7]
```


```{r}
X_test = data.matrix(test_set[,selected_variables])         # independent variables for test
y_test = test_set[,7]
```


```{r}
# Convert y_train to a numeric vector
y_train <- as.numeric(unlist(y_train))
```


```{r}
# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
```


```{r}
# Convert y_test to a numeric vector
y_test <- as.numeric(unlist(y_test))
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)
```






```{r}
# Set up the parameter grid
xgb_param_grid <- expand.grid(
  max_depth = c(3, 4, 5, 6),
  gamma = c(0, 1, 2, 3, 4, 5),
  eta = c(0.03, 0.06, 0.1, 0.2),
  nrounds = 100,  # Number of boosting rounds
  colsample_bytree = 0.8,  # Subsample ratio of columns when constructing each tree
  min_child_weight = 1,
  subsample = 0.8
)
```


```{r}
# Set up train control with cross-validation
# Set up train control with cross-validation
train_control <- trainControl(
  method = "cv", 
  number = 1, 
  verboseIter = TRUE
)
```


```{r}
# Convert y_train to a binary classification format
y_train_binary <- ifelse(y_train == 1, 0, 1)

# Convert the train data into xgboost matrix type
xgboost_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_binary)

```


```{r}
# Define parameter grid
param_grid <- expand.grid(max_depth = 3:6, gamma = seq(0, 5, by = 1), eta = c(0.03, 0.06, 0.1, 0.2))

# Train XGBoost model
best_model <- NULL
best_accuracy <- 0

for (i in 1:nrow(param_grid)) {
  param <- param_grid[i, ]
  xgb_model <- xgboost(data = xgboost_train,
                       max_depth = param$max_depth, gamma = param$gamma, eta = param$eta,
                       nrounds = 100, objective = "binary:logistic", verbose = FALSE)
  
  # Evaluate model using accuracy
  y_pred <- predict(xgb_model, xgboost_train)
  accuracy <- mean(as.numeric(y_pred > 0.5) == as.numeric(y_train_binary))
  
  # Update best model if current model performs better
  if (accuracy > best_accuracy) {
    best_model <- xgb_model
    best_accuracy <- accuracy
  }
}

# Summary of the trained model
summary(best_model)



```


```{r}
# Convert y_test to a binary classification format
y_test_binary <- ifelse(y_test == 1, 0, 1)

# Convert the test data into xgboost matrix type
xgboost_test <- xgb.DMatrix(data = as.matrix(X_test))

```


```{r}
# Predict ratingRecommendToFriend on the testing set
y_pred <- predict(best_model, xgboost_test)
```


```{r}
# Calculate AUC
predictions <- prediction(y_pred, y_test_binary)
auc_value <- performance(predictions, "auc")@y.values[[1]]
```


```{r}
# Predict ratingRecommendToFriend on the testing set
y_pred <- predict(best_model, xgboost_test)

# Create confusion matrix
conf_mat <- table(Actual = y_test, Predicted = ifelse(y_pred > 0.5, 2, 1))

# Calculate accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)

# Print confusion matrix and accuracy
print("Confusion Matrix:")
print(conf_mat)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
```
For this model the accuracy is 88.8% and the number of False positives and False Negatives is 175 & 109. So this model is performing well too. But if you compare the two models Random Forest is performing relatively well and will be preferred.

```{r}
library(ROCR)

# Create prediction object
pred <- prediction(y_pred, y_test_binary)

# Create performance object
perf <- performance(pred, "tpr", "fpr")

# Plot ROC curve
plot(perf, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate AUC
auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))

# Print AUC value
print(paste("AUC:", auc_value))
```



