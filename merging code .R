#merging the two data sets together
# this code combines the two latest data sets and merges them into one comprehensive data set to work on 
# in order for this code to work, you will need to have the previously combines codes working ( expenditure and dropout)

library(dplyr)
library(readr)

# Load data
df1 <- read_csv("/Users/sanaakashif/Desktop/ds/combined_expenditure_subset.csv")
df2 <- read_csv("/Users/sanaakashif/Desktop/ds/young_lives_dropout_analysis.csv")

# Merge
merged_df <- df2 %>%
  left_join(df1, by = c("childid", "country"))

# ===== IMPUTE NAs IMMEDIATELY AFTER MERGE ===== 
#this is because random forest gets messed up with NA 
# For numeric columns: replace NA with median
numeric_cols <- names(merged_df)[sapply(merged_df, is.numeric)]
for (col in numeric_cols) {
  med <- median(merged_df[[col]], na.rm = TRUE)
  merged_df[[col]][is.na(merged_df[[col]])] <- med
}

# For character/factor columns: replace NA with "Unknown"
char_cols <- names(merged_df)[sapply(merged_df, is.character)]
for (col in char_cols) {
  merged_df[[col]][is.na(merged_df[[col]])] <- "Unknown"
}

# Check no NAs remain
print(colSums(is.na(merged_df)))

# Save
write.csv(merged_df, "merged_young_lives_data.csv", row.names = FALSE) 