rm(list = ls())

library(haven)
library(dplyr)
library(readr)


# ROUND 3 FILES


child_r3_in <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/india/in_yc_childlevel.dta")
hh_r3_in    <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/india/in_yc_householdlevel.dta")

child_r3_pe <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/peru/pe_yc_childlevel.dta")
hh_r3_pe    <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/Round 3/r3_yc/peru/pe_yc_householdlevel.dta")


# ROUND 4 FILES


child_r4_in <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/in_r4_ycch_youngerchild.dta")
child_r4_pe <- read_dta("/Users/jasnooranand/Documents/datascience_finalproj/pe_r4_ycch_youngerchild.dta")


# STANDARDIZING CHILD IDS


# Round 3
child_r3_in$childid <- toupper(as.character(child_r3_in$CHILDID))
hh_r3_in$childid    <- toupper(as.character(hh_r3_in$childid))
child_r3_pe$childid <- toupper(as.character(child_r3_pe$childid))
hh_r3_pe$childid    <- toupper(as.character(hh_r3_pe$childid))

# Round 4 - FIX: Add country prefix to match Round 3 format
child_r4_in$childid <- paste0("IN0", child_r4_in$CHILDCODE)
child_r4_pe$childid <- paste0("PE0", child_r4_pe$CHILDCODE)

child_r4_in$childid <- toupper(child_r4_in$childid)
child_r4_pe$childid <- toupper(child_r4_pe$childid)


# CREATINMG ENROLLMENT VARIABLES FOR EACH ROUND


# India Round 3 enrollment
india_r3_enroll <- child_r3_in %>%
  select(childid, ENRSCHR3) %>%
  rename(enrolled_r3 = ENRSCHR3) %>%
  mutate(enrolled_r3 = as.numeric(enrolled_r3))

# India Round 4 enrollment
india_r4_enroll <- child_r4_in %>%
  select(childid, ENRSCHR4) %>%
  rename(enrolled_r4 = ENRSCHR4) %>%
  mutate(enrolled_r4 = as.numeric(enrolled_r4))

# Peru Round 3 enrollment
peru_r3_enroll <- child_r3_pe %>%
  select(childid, enrschr3) %>%
  rename(enrolled_r3 = enrschr3) %>%
  mutate(enrolled_r3 = as.numeric(enrolled_r3))

# Peru Round 4 enrollment
peru_r4_enroll <- child_r4_pe %>%
  select(childid, ENRSCHR4) %>%
  rename(enrolled_r4 = ENRSCHR4) %>%
  mutate(enrolled_r4 = as.numeric(enrolled_r4))


# MERGING TO CREATE PROPER DROPOUT VARIABLE


# India
india_panel <- child_r3_in %>%
  left_join(hh_r3_in, by = "childid") %>%
  left_join(india_r3_enroll, by = "childid") %>%
  left_join(india_r4_enroll, by = "childid") %>%
  mutate(
    country = "India",
    # PROPER DROPOUT: enrolled in R3 but NOT enrolled in R4
    dropout = case_when(
      enrolled_r3 == 1 & enrolled_r4 == 0 ~ 1,
      enrolled_r3 == 1 & enrolled_r4 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Peru
peru_panel <- child_r3_pe %>%
  left_join(hh_r3_pe, by = "childid") %>%
  left_join(peru_r3_enroll, by = "childid") %>%
  left_join(peru_r4_enroll, by = "childid") %>%
  mutate(
    country = "Peru",
    # PROPER DROPOUT: enrolled in R3 but NOT enrolled in R4
    dropout = case_when(
      enrolled_r3 == 1 & enrolled_r4 == 0 ~ 1,
      enrolled_r3 == 1 & enrolled_r4 == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


# HARMONIZING VARIABLES


# Sex
if("sex" %in% names(india_panel)) india_panel <- india_panel %>% select(-sex)
if("sex" %in% names(peru_panel))  peru_panel  <- peru_panel  %>% select(-sex)

india_panel <- india_panel %>% rename(sex = chsex)
peru_panel  <- peru_panel  %>% rename(sex = chsex)


# SELECTING ONLY OUR  FINAL VARIABLES


final_vars <- c(
  "childid", 
  "country", 
  "sex", 
  "enrolled_r3",
  "enrolled_r4",
  "dropout",
  # Sensitivity analysis variables from R3
  "bmi",
  "hhsize",
  "wi",
  "math",
  "zhfa",
  # Other useful variables
  "ppvt",
  "zwfa",
  "zbfa",
  "typesite",
  "region"
)

india_final <- india_panel %>% select(any_of(final_vars))
peru_final  <- peru_panel  %>% select(any_of(final_vars))

# FIXING TYPE MISMATCHES


india_final$typesite <- as.character(india_final$typesite)
peru_final$typesite  <- as.character(peru_final$typesite)

india_final$region <- as.character(india_final$region)
peru_final$region  <- as.character(peru_final$region)

india_final$sex <- as.numeric(india_final$sex)
peru_final$sex  <- as.numeric(peru_final$sex)


# COMBINING


combined <- bind_rows(india_final, peru_final)


# 10. CHECKING DROPOUT RATES


cat("\n========== PROPER DROPOUT ANALYSIS (R3 to R4) ==========\n")
cat("Total observations:", nrow(combined), "\n\n")

cat("India:\n")
cat("  Enrolled in R3:", sum(india_final$enrolled_r3 == 1, na.rm = TRUE), "\n")
cat("  Enrolled in R4:", sum(india_final$enrolled_r4 == 1, na.rm = TRUE), "\n")
cat("  Dropped out by R4:", sum(india_final$dropout == 1, na.rm = TRUE), "\n")
cat("  Dropout rate:", round(mean(india_final$dropout, na.rm = TRUE) * 100, 2), "%\n\n")

cat("Peru:\n")
cat("  Enrolled in R3:", sum(peru_final$enrolled_r3 == 1, na.rm = TRUE), "\n")
cat("  Enrolled in R4:", sum(peru_final$enrolled_r4 == 1, na.rm = TRUE), "\n")
cat("  Dropped out by R4:", sum(peru_final$dropout == 1, na.rm = TRUE), "\n")
cat("  Dropout rate:", round(mean(peru_final$dropout, na.rm = TRUE) * 100, 2), "%\n\n")

cat("Combined dropout rate:", round(mean(combined$dropout, na.rm = TRUE) * 100, 2), "%\n")

cat("\nMissing data for sensitivity analysis variables:\n")
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


write_csv(combined, "/Users/jasnooranand/Documents/datascience_finalproj/Final_Project/young_lives_dropout_r3_r4.csv")
cat("\nSaved as young_lives_dropout_r3_r4.csv\n")


# Checking how many kids have R4 data
cat("India kids with R4 data:", sum(!is.na(india_final$enrolled_r4)), "\n")
cat("India kids missing R4 data:", sum(is.na(india_final$enrolled_r4)), "\n\n")

cat("Peru kids with R4 data:", sum(!is.na(peru_final$enrolled_r4)), "\n")
cat("Peru kids missing R4 data:", sum(is.na(peru_final$enrolled_r4)), "\n")