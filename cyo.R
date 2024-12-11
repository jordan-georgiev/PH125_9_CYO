##########################################################
# 1. Setup  and data loading
##########################################################
# Install additional packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(glmnet)
library(kableExtra)
library(knitr)
library(psych)
library(ROCR)
library(scales)
library(tidyverse)

# League of Legends (LoL) Diamond Ranked Games (10 min) dataset:
# https://www.key2stats.com/League_of_Legends_Diamond_Ranked_Games__10_min__1596_1.csv

# Suppress scientific notations of numbers
options(scipen=999)

# Read the LoL datasetet
game_data <- read.csv("https://www.key2stats.com/League_of_Legends_Diamond_Ranked_Games__10_min__1596_1.csv")

# Inspect the data
t(head(game_data, 3))

str(game_data)

# Columns X is just row numbers, gameId will not be needed, as it is unique.
# Remove columns "X" and "gameId" from the data frame
game_data <- game_data[ , !(names(game_data) %in% c("X", "gameId"))]

# Summary statistics
tab1 <- kable(describe(game_data)[, c(3:4, 8:10, 13)], booktabs = TRUE, escape = FALSE, caption = "Dataset summary") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab1

# Check for missing values to tidy data if necessary
if (anyNA(game_data)) {
  colSums(is.na(game_data))
} else {
  print("No NA values found in game_data.")
}

# Some columns come in pairs, inspect them
paired_cols <- game_data[, grepl("Diff$", names(game_data))]
tab2 <- kable(head(paired_cols, 5), booktabs = TRUE, escape = FALSE, caption = "Training dataset summary") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab2

# Check for connected columns - same values in blue and red columns
connected_cols <- head(game_data[ , c("blueKills", "redDeaths", "redKills", "blueDeaths")], 5)
tab3 <- kable(connected_cols, booktabs = TRUE, escape = FALSE, caption = "Mirrored columns") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab3

# Other columns are mutually exclusive 1 in one column leads to 0 in other. Both can be 0 though.
mut_excl_cols <- head(game_data[ , c("blueFirstBlood", "redFirstBlood", "blueDragons", "redDragons", "blueHeralds", "redHeralds")])
tab4 <- kable(mut_excl_cols, booktabs = TRUE, escape = FALSE, caption = "Mutually exclusive columns") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab4

# Data seems tidy now, let us continue to EDA.

##########################################################
# 2. Exploratory data analysis
##########################################################

# Win ratio from blueWins column
win_loss_table <- table(game_data$blueWins)
percentages <- round(win_loss_table / sum(win_loss_table) * 100, 1)
pie(win_loss_table, 
    labels = paste(c("Loss", "Win"), "(", percentages, "%)"),
    col = c("indianred1", "lightblue"),
    main = "Blue Wins vs Losses")

# Correlations between variables
# Calculate a correlation matrix
correlation_matrix <- cor(game_data)

# Convert the correlation matrix to a tidy format
correlation_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Corr")

# Remove self-correlations where Var1 == Var2
correlation_df <- correlation_df %>%
  filter(Var1 != Var2)

# Check how blue vars correlate to red vars and > 0.4
blue_red_abs_05 <- correlation_df %>%
  filter(str_starts(Var1, "blue") & str_starts(Var2, "red") & abs(Corr) > 0.5 & abs(Corr) < 1) %>%
  arrange(desc(Corr))
print(blue_red_abs_05, n = 37)

# Check how blueVars corr with redDeaths var
blue_redDeaths <- correlation_df %>%
  filter(str_starts(Var1, "blue") & str_starts(Var2, "redDeaths") & Corr > 0.2) %>%
  arrange(desc(Corr))
print(blue_redDeaths)

# Convert the correlation_df back to wide format for heatmap
correlation_wide <- correlation_df %>%
  pivot_wider(names_from = Var2, values_from = Corr)

# Create the heatmap
ggplot(correlation_df, aes(x = Var1, y = Var2, fill = Corr)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "red", high = "blue", mid = "white") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 5))

# Now correlate all columns except blueWins to blueWins
# Calculate correlations of all columns to blueWins
correlations <- sapply(game_data %>% select(-blueWins), function(col) cor(game_data$blueWins, col))

# Create a data frame for the correlations
correlations_df <- data.frame(Variable = names(correlations), Correlation = correlations)

# Create a bar plot
ggplot(correlations_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() + # Flip the coordinates for a horizontal bar plot
  theme_minimal() +
  labs(title = "Correlation of Variables with blueWins",
       x = "",
       y = "Correlation") +
  theme(axis.text.y = element_text(size = 7))

# Check how blue vars which correlate high to redDeaths correlate with blueWins
# Extract only the necessary columns from game_data
subset_data <- game_data %>% select(all_of(c("blueWins", blue_redDeaths$Var1)))

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)

# Convert the correlation matrix into a tidy format
correlation_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Corr")

# Create the heatmap
ggplot(correlation_df, aes(x = Var1, y = Var2, fill = Corr)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "red", high = "blue", mid = "white") +
  geom_text(aes(label = round(Corr, 3)), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Correlation Heatmap (with blueWins)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################
# 3. Data preparation
#############################################################
# Partition the data to create test (10%) and training sets (90%)
set.seed(9, sample.kind="Rounding")

# Col blueWin is the one we will predict - partition the data (test_set 10% of game_data)
test_index <- createDataPartition(y = game_data$blueWins, times = 1, p = 0.1, list = FALSE)
train_set <- game_data[-test_index,]
test_set <- game_data[test_index,]
rm(test_index)

# Create an overview table with row and column count
dataset_overview <- data.frame(
  Dataset = c("train_set", "test_set"),
  Rows = c(nrow(train_set), nrow(test_set)),
  Columns = c(ncol(train_set), ncol(test_set))
)
tab5 <- kable(dataset_overview, booktabs = TRUE, escape = FALSE) %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab5

# Mean of blueWins
mean(train_set$blueWins)
mean(test_set$blueWins)

# Convert blueWins to a factor
train_set$blueWins <- as.factor(train_set$blueWins)
test_set$blueWins <- as.factor(test_set$blueWins)

#############################################################
# 4. Model development
#############################################################

# Set a seed for reproducibility
set.seed(123)

# Define a formula for the model
formula <- blueWins ~ .

# Logistic Regression
logistic_model <- train(
  blueWins ~ ., data = train_set,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)
)

# Decision Tree
decision_tree_model <- train(
  blueWins ~ ., data = train_set,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)

# Random Forest
random_forest_model <- train(
  blueWins ~ ., data = train_set,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

# K-Nearest Neighbors (KNN)
knn_model <- train(
  blueWins ~ ., data = train_set,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5)
)
print(max(knn_model$results["Accuracy"]))

# Collecting performance metrics for all models
model_results <- data.frame(Model = character(),
                            Accuracy = numeric(),
                            Sensitivity = numeric(),
                            Specificity = numeric(),
                            Precision = numeric(),
                            F1 = numeric(),
                            stringsAsFactors = FALSE)

# List of models to evaluate
models <- list(
  Logistic = logistic_model,
  DecisionTree = decision_tree_model,
  RandomForest = random_forest_model,
  KNN = knn_model
)

# Extract confusion matrix results for each model
for (model_name in names(models)) {
  model <- models[[model_name]]
  preds <- predict(model, test_set)
  cm <- confusionMatrix(preds, test_set$blueWins)
  
  # Extract relevant metrics
  accuracy <- cm$overall["Accuracy"]
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  precision <- cm$byClass["Precision"]
  f1 <- cm$byClass["F1"]
  
  # Add to results table
  model_results <- rbind(model_results, data.frame(
    Model = model_name,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = f1
  ))
}

# Print the results table
tab6 <- kable(model_results, booktabs = TRUE, escape = FALSE, caption = "Model results") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab6
#############################################################
# 5. Model fine tuning
#############################################################

# Fine-tuning Logistic Regression using glmnet
logistic_tune_grid <- expand.grid(
  alpha = c(0, 0.5, 1),    # Elastic net mixing parameter
  lambda = seq(0.001, 0.1, length = 10)  # Regularization parameter
)

logistic_model_tuned <- train(
  blueWins ~ ., data = train_set,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = logistic_tune_grid
)

# Predict, create a confusion matrix and extract metrics
model <- logistic_model_tuned
preds <- predict(logistic_model_tuned, test_set)
cm <- confusionMatrix(preds, test_set$blueWins)

# Extract relevant metrics
accuracy <- cm$overall["Accuracy"]
sensitivity <- cm$byClass["Sensitivity"]
specificity <- cm$byClass["Specificity"]
precision <- cm$byClass["Precision"]
f1 <- cm$byClass["F1"]

# Add to results table
model_results <- rbind(model_results, data.frame(
  Model = "Logistic tuned",
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision,
  F1 = f1
))
# Print the final results
model_results

# Check the importance of the variables in the model
variable_importance <- varImp(logistic_model_tuned, scale = TRUE)

# Convert varImp to data.frame
variable_importance_df <- as.data.frame(variable_importance$importance)

# Sort by importance in descending order
variable_importance_df <- variable_importance_df[order(-variable_importance_df$Overall), ]

tab7 <- kable(variable_importance_df, booktabs = TRUE, escape = FALSE, caption = "Variable Importance") %>%
  kable_styling(position = "center", latex_options = c("striped"))
tab7