library(haven)
library(dplyr)
library(readr)



child_in <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/india/in_yc_childlevel.dta")
hh_in    <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/india/in_yc_householdlevel.dta")

child_pe <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/peru/pe_yc_childlevel.dta")
hh_pe    <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/peru/pe_yc_householdlevel.dta")


# STANDARDIZING CHILD IDS


child_in$childid <- toupper(as.character(child_in$CHILDID))
hh_in$childid    <- toupper(as.character(hh_in$childid))

child_pe$childid <- toupper(as.character(child_pe$childid))
hh_pe$childid    <- toupper(as.character(hh_pe$childid))


# MERGING THE ENTRIE DATA


india <- child_in %>%
  left_join(hh_in, by = "childid") %>%
  mutate(country = "India")

peru <- child_pe %>%
  left_join(hh_pe, by = "childid") %>%
  mutate(country = "Peru")


# HARMONIZING ALL KEY VARIABLES


# Sex - removing duplicate first
if("sex" %in% names(india)) india <- india %>% select(-sex)
if("sex" %in% names(peru))  peru  <- peru  %>% select(-sex)

india <- india %>% rename(sex = chsex)
peru  <- peru  %>% rename(sex = chsex)

# Enrollment
india <- india %>% rename(enrolled = ENRSCHR3)
peru  <- peru  %>% rename(enrolled = enrschr3)


#DROPOUT INDICATOPR

dropout_codes <- c(0, 77, 88, 99, NA)

india <- india %>% 
  mutate(dropout = ifelse(is.na(enrolled) | enrolled %in% dropout_codes, 1, 0))

peru <- peru %>% 
  mutate(dropout = ifelse(is.na(enrolled) | enrolled %in% dropout_codes, 1, 0))


# SELECTING OUR FINAL VARIABLES


final_vars <- c(
  "childid", 
  "country", 
  "sex", 
  "enrolled", 
  "dropout",
  # KEY VARIABLES FOR SENSITIVITY ANALYSIS (5 VARIABLES!)
  "bmi",           # BMI
  "hhsize",        # Household size
  "wi",            # Wealth index (our income proxy!)
  "math",          # Math score
  "zhfa",          # Z-score height for age
  # Other useful variables
  "ppvt",          # Cognitive score
  "zwfa",          # Z-score weight for age
  "zbfa",          # Z-score BMI for age
  "typesite",      # Urban/rural
  "region"         # Region
)

india_final <- india %>% select(any_of(final_vars))
peru_final  <- peru  %>% select(any_of(final_vars))


# FIXING ANY TYPE MISMATCHES


india_final$typesite <- as.character(india_final$typesite)
peru_final$typesite  <- as.character(peru_final$typesite)

india_final$region <- as.character(india_final$region)
peru_final$region  <- as.character(peru_final$region)

india_final$sex <- as.numeric(india_final$sex)
peru_final$sex  <- as.numeric(peru_final$sex)

india_final$enrolled <- as.numeric(india_final$enrolled)
peru_final$enrolled  <- as.numeric(peru_final$enrolled)


# COMBINING BOTH COUNTRIES

combined <- bind_rows(india_final, peru_final)


# KEY VARIABLES


cat("\n========== DATA SUMMARY ==========\n")
cat("Total observations:", nrow(combined), "\n")
cat("India observations:", nrow(india_final), "\n")
cat("Peru observations:", nrow(peru_final), "\n\n")

cat("Dropout rates:\n")
cat("India:", round(mean(india_final$dropout, na.rm = TRUE) * 100, 2), "%\n")
cat("Peru:", round(mean(peru_final$dropout, na.rm = TRUE) * 100, 2), "%\n")
cat("Combined:", round(mean(combined$dropout, na.rm = TRUE) * 100, 2), "%\n\n")

cat("Missing data for SENSITIVITY ANALYSIS variables:\n")
cat("BMI:", sum(is.na(combined$bmi)), "out of", nrow(combined), 
    "(", round(sum(is.na(combined$bmi))/nrow(combined)*100, 1), "%)\n")
cat("Household size:", sum(is.na(combined$hhsize)), "out of", nrow(combined),
    "(", round(sum(is.na(combined$hhsize))/nrow(combined)*100, 1), "%)\n")
cat("Wealth index:", sum(is.na(combined$wi)), "out of", nrow(combined),
    "(", round(sum(is.na(combined$wi))/nrow(combined)*100, 1), "%)\n")
cat("Math score:", sum(is.na(combined$math)), "out of", nrow(combined),
    "(", round(sum(is.na(combined$math))/nrow(combined)*100, 1), "%)\n")
cat("zhfa:", sum(is.na(combined$zhfa)), "out of", nrow(combined),
    "(", round(sum(is.na(combined$zhfa))/nrow(combined)*100, 1), "%)\n")


#FINAL DATASET

write_csv(combined, "young_lives_dropout_sensitivity.csv")
cat("\nSUCCESS! Saved as young_lives_dropout_sensitivity.csv\n")
cat("\nYour 5 sensitivity analysis variables:\n")
cat("1. BMI (nutrition/health)\n")
cat("2. Household size (family structure)\n")
cat("3. Wealth index (economic status)\n")
cat("4. Math score (education quality)\n")
cat("5. Height-for-age z-score (early childhood health)\n")


write_csv(combined, "/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/young_lives_dropout_sensitivity.csv")
write_csv(combined, "/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/young_lives_dropout_sensitivity.csv")

