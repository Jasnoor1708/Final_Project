library(dplyr)
library(readr)
library(ggplot2)
library(randomForest)


data <- read_csv("/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/young_lives_dropout_r3_r4.csv")



# remocing all rows with missing dropout or key variables
model_data <- data %>%
  filter(!is.na(dropout), 
         !is.na(bmi), 
         !is.na(hhsize),
         !is.na(wi),
         !is.na(math),
         !is.na(zhfa),
         !is.na(sex))

# need to convert dropout to factor for modeling
model_data$dropout <- as.factor(model_data$dropout)

cat("Clean data for modeling:", nrow(model_data), "observations\n")
cat("Dropout cases:", sum(model_data$dropout == 1), "\n")
cat("Non-dropout cases:", sum(model_data$dropout == 0), "\n\n")


# BASELINE MODEL


# Using balanced random forest to handle class imbalance
set.seed(123)

baseline_model <- randomForest(
  dropout ~ bmi + hhsize + wi + math + zhfa + sex + country,
  data = model_data,
  ntree = 500,
  classwt = c("0" = 1, "1" = 50),  # here i am givig more weight to dropout class
  importance = TRUE
)

# baseline predictions
baseline_pred <- predict(baseline_model, model_data, type = "prob")[,2]
baseline_dropout_rate <- mean(baseline_pred)

cat("========== BASELINE MODEL ==========\n")
cat("Baseline predicted dropout rate:", round(baseline_dropout_rate * 100, 2), "%\n\n")


# SENSITIVITY ANALYSIS: BMI


cat("========== BMI SENSITIVITY ANALYSIS ==========\n\n")

# Scenario 1: BMI increases by 5%
scenario_bmi_5 <- model_data %>%
  mutate(bmi = bmi * 1.05)

pred_bmi_5 <- predict(baseline_model, scenario_bmi_5, type = "prob")[,2]
dropout_rate_bmi_5 <- mean(pred_bmi_5)

cat("Scenario 1: BMI increases by 5%\n")
cat("  Predicted dropout rate:", round(dropout_rate_bmi_5 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_bmi_5 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")

# Scenario 2: BMI increases by 10%
scenario_bmi_10 <- model_data %>%
  mutate(bmi = bmi * 1.10)

pred_bmi_10 <- predict(baseline_model, scenario_bmi_10, type = "prob")[,2]
dropout_rate_bmi_10 <- mean(pred_bmi_10)

cat("Scenario 2: BMI increases by 10%\n")
cat("  Predicted dropout rate:", round(dropout_rate_bmi_10 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_bmi_10 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")

# Scenario 3: BMI decreases by 10% (malnutrition worsens)
scenario_bmi_neg10 <- model_data %>%
  mutate(bmi = bmi * 0.90)

pred_bmi_neg10 <- predict(baseline_model, scenario_bmi_neg10, type = "prob")[,2]
dropout_rate_bmi_neg10 <- mean(pred_bmi_neg10)

cat("Scenario 3: BMI decreases by 10% (nutrition worsens)\n")
cat("  Predicted dropout rate:", round(dropout_rate_bmi_neg10 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_bmi_neg10 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")


# SENSITIVITY ANALYSIS: HOUSEHOLD SIZE


cat("========== HOUSEHOLD SIZE SENSITIVITY ANALYSIS ==========\n\n")

# Scenario 1: Household size decreases by 1 person
scenario_hhsize_minus1 <- model_data %>%
  mutate(hhsize = pmax(1, hhsize - 1))  # Don't go below 1

pred_hhsize_minus1 <- predict(baseline_model, scenario_hhsize_minus1, type = "prob")[,2]
dropout_rate_hhsize_minus1 <- mean(pred_hhsize_minus1)

cat("Scenario 1: Household size decreases by 1 person\n")
cat("  Predicted dropout rate:", round(dropout_rate_hhsize_minus1 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_hhsize_minus1 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")

# Scenario 2: Household size decreases by 2 people
scenario_hhsize_minus2 <- model_data %>%
  mutate(hhsize = pmax(1, hhsize - 2))

pred_hhsize_minus2 <- predict(baseline_model, scenario_hhsize_minus2, type = "prob")[,2]
dropout_rate_hhsize_minus2 <- mean(pred_hhsize_minus2)

cat("Scenario 2: Household size decreases by 2 people\n")
cat("  Predicted dropout rate:", round(dropout_rate_hhsize_minus2 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_hhsize_minus2 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")

# Scenario 3: Household size increases by 1 person
scenario_hhsize_plus1 <- model_data %>%
  mutate(hhsize = hhsize + 1)

pred_hhsize_plus1 <- predict(baseline_model, scenario_hhsize_plus1, type = "prob")[,2]
dropout_rate_hhsize_plus1 <- mean(pred_hhsize_plus1)

cat("Scenario 3: Household size increases by 1 person\n")
cat("  Predicted dropout rate:", round(dropout_rate_hhsize_plus1 * 100, 2), "%\n")
cat("  Change from baseline:", round((dropout_rate_hhsize_plus1 - baseline_dropout_rate) * 100, 2), 
    "percentage points\n\n")


# VISUALIZATION


# first need to complie our results 
sensitivity_results <- data.frame(
  Scenario = c(
    "Baseline",
    "BMI +5%", "BMI +10%", "BMI -10%",
    "HH size -1", "HH size -2", "HH size +1"
  ),
  Dropout_Rate = c(
    baseline_dropout_rate,
    dropout_rate_bmi_5, dropout_rate_bmi_10, dropout_rate_bmi_neg10,
    dropout_rate_hhsize_minus1, dropout_rate_hhsize_minus2, dropout_rate_hhsize_plus1
  ),
  Variable = c(
    "Baseline",
    "BMI", "BMI", "BMI",
    "Household Size", "Household Size", "Household Size"
  )
)

sensitivity_results$Change_from_Baseline <- 
  (sensitivity_results$Dropout_Rate - baseline_dropout_rate) * 100

# this is our visualization
ggplot(sensitivity_results %>% filter(Scenario != "Baseline"), 
       aes(x = reorder(Scenario, Change_from_Baseline), 
           y = Change_from_Baseline, 
           fill = Variable)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    title = "Sensitivity Analysis: Impact on Predicted Dropout Rate",
    subtitle = "Change from baseline (percentage points)",
    x = "Scenario",
    y = "Change in Dropout Rate (percentage points)",
    fill = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/sensitivity_analysis_plot.png",
       width = 10, height = 6, dpi = 300)

cat("\nVisualization saved as sensitivity_analysis_plot.png\n")


# RESULTS TABLE


write_csv(sensitivity_results, 
          "/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/sensitivity_results.csv")

cat("Results saved as sensitivity_results.csv\n")

#just summary table
cat("\n========== SUMMARY TABLE ==========\n")
print(sensitivity_results, row.names = FALSE)