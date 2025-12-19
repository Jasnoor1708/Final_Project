library(randomForest)
library(dplyr)

# 1. Read data
df <- read.csv("/Users/sanaakashif/Desktop/ds/young_lives_dropout_r3_r4.csv")


# 2. Drop variables not to be used

# Outcome
df$dropout <- factor(df$dropout)

# Drop enrollment variables (not included as predictors)
df$enrolled_r3 <- NULL
df$enrolled_r4 <- NULL

# Drop ID if present
df$childid <- NULL


# 3. Set categorical predictors
cat_vars <- c("country", "sex", "typesite", "region")
df[cat_vars] <- lapply(df[cat_vars], factor)

# 4. Train / test split
set.seed(1234)
idx <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train <- df[idx, ]
test  <- df[-idx, ]

# 5. Handle missing values
train <- na.omit(train)
test  <- na.omit(test)

# 6. Fit Random Forest
rf_model <- randomForest(
  dropout ~ .,
  data = train,
  ntree = 500,
  importance = TRUE
)

# 7. Predict & evaluate
pred <- predict(rf_model, test)
accuracy <- mean(pred == test$dropout)
print(paste("Accuracy:", round(accuracy, 3)))

# 8. Variable importance
imp <- importance(rf_model)

imp_sorted <- imp[order(imp[, 2], decreasing = TRUE), ]

print(imp_sorted)


#code to put output into a table 
# Extract importance
imp <- importance(rf_model)

# Convert to data frame
imp_table <- data.frame(
  Variable = rownames(imp),
  MeanDecreaseAccuracy = imp[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = imp[, "MeanDecreaseGini"]
)

# Sort by Gini importance (most intuitive)
imp_table <- imp_table[order(imp_table$MeanDecreaseGini, decreasing = TRUE), ]

# Reset row names
rownames(imp_table) <- NULL

# View table
imp_table


