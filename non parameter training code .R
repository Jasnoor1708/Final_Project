```{r}
library(randomForest)
library(dplyr)

df <- merged_df
df$dropout <- as.factor(df$dropout)
df$childid <- NULL

# Remove problematic columns
df$enrolled <- NULL
df$mother_edu <- NULL
df$father_edu <- NULL

cat_vars <- c("country", "sex", "region", "typesite", "chlang_gr", "chsex", "ppvtlang2", "egralang2", "mathlang2", "readci", "writeci")
df[cat_vars] <- lapply(df[cat_vars], factor)

print(table(df$dropout))

set.seed(1234)
train_rows <- sample(nrow(df), 0.7 * nrow(df))
train <- df[train_rows, ]
test  <- df[-train_rows, ]

# Convert -Inf to NA then replace with median
train <- train %>%
  mutate(across(where(is.numeric), 
                ~ifelse(is.infinite(.), NA, .)))

test <- test %>%
  mutate(across(where(is.numeric), 
                ~ifelse(is.infinite(.), NA, .)))

for (col in colnames(train)) {
  if (is.numeric(train[[col]])) {
    med <- median(train[[col]], na.rm = TRUE)
    train[[col]][is.na(train[[col]])] <- med
    test[[col]][is.na(test[[col]])] <- med
  }
}

# Fit model
rf_model <- randomForest(
  dropout ~ .,
  data = train,
  ntree = 500,
  importance = TRUE
)

rf_pred <- predict(rf_model, test)
accuracy <- mean(rf_pred == test$dropout)

print(paste("Accuracy:", round(accuracy, 3)))

# Variable Importance
imp <- importance(rf_model)
print("Top 20 Variable Importance:")
print(imp[order(imp[,2], decreasing=TRUE), ][1:20, ])

varImpPlot(rf_model)
```
