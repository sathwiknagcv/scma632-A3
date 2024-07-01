# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

# Load the dataset
file_path <- 'C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\A3\\Bank Customer Churn Prediction.csv'
data <- read_csv(file_path)

# Display the first few rows of the dataset
head(data)

# Check the structure of the dataset
str(data)
summary(data)

# Drop rows with missing values
data <- na.omit(data)

# Identify categorical columns
categorical_columns <- names(data)[sapply(data, is.character)]

# Option 1: Label Encoding (for binary categorical data)
for (column in categorical_columns) {
  data[[column]] <- as.numeric(factor(data[[column]]))
}

# Assume 'churn' is the target variable and the rest are predictors
target <- 'churn'
predictors <- setdiff(names(data), target)

# Split the data into training and test sets
set.seed(810)
train_index <- createDataPartition(data$churn, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Fit the logistic regression model
model <- glm(churn ~ ., data = train_data, family = binomial)

# Predict on the test set
y_pred <- predict(model, newdata = test_data, type = "response")
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(y_pred_class), as.factor(test_data$churn))
roc_curve <- roc(test_data$churn, y_pred)
roc_auc <- auc(roc_curve)

# Print the model coefficients
coef_df <- data.frame(Variable = names(coef(model)), Coefficient = coef(model))
print(coef_df)

# Plot the ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = paste("ROC Curve (AUC = ", round(roc_auc, 2), ")", sep = ""))

# Display the confusion matrix
conf_matrix_table <- as.table(conf_matrix$table)
heatmap(conf_matrix_table, col = heat.colors(256), scale = "column", margins = c(5,10))

# Decision Tree
tree_model <- rpart(churn ~ ., data = train_data, method = "class", control = rpart.control(cp = 0.01))

# Predict on the test set
y_pred_tree <- predict(tree_model, newdata = test_data, type = "prob")[, 2]
y_pred_tree_class <- ifelse(y_pred_tree > 0.5, 1, 0)

# Evaluate the model
conf_matrix_tree <- confusionMatrix(as.factor(y_pred_tree_class), as.factor(test_data$churn))
roc_curve_tree <- roc(test_data$churn, y_pred_tree)
roc_auc_tree <- auc(roc_curve_tree)

# Plot the ROC curve for Decision Tree
plot(roc_curve_tree, col = "red", lwd = 2, main = paste("Decision Tree ROC Curve (AUC = ", round(roc_auc_tree, 2), ")", sep = ""))

# Display the confusion matrix for Decision Tree
conf_matrix_tree_table <- as.table(conf_matrix_tree$table)
heatmap(conf_matrix_tree_table, col = heat.colors(256), scale = "column", margins = c(5,10))

# Calculate ROC curve for decision tree
y_pred_proba_tree <- predict(tree_model, newdata = X_test, type = 'prob')[,2]
roc_curve_tree <- roc(y_test, y_pred_proba_tree)
plot(roc_curve_tree, main = 'ROC Curve for Decision Tree')
auc_value_tree <- auc(roc_curve_tree)
print(paste('AUC (Decision Tree):', auc_value_tree))

