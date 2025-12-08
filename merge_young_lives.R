rm(list = ls())

library(haven)
library(dplyr)
library(stringr)

# ============================================================
# 1. SET PATHS
# ============================================================
path_india <- "/Users/jasnooranand/Documents/Round 3/r3_yc/india"
path_peru  <- "/Users/jasnooranand/Documents/Round 3/r3_yc/peru"

# ============================================================
# 2. LOAD INDIA FILES
# ============================================================
child_in <- read_dta(file.path(path_india, "in_yc_childlevel.dta"))
hh_in    <- read_dta(file.path(path_india, "in_yc_householdlevel.dta"))
memb_in  <- read_dta(file.path(path_india, "in_yc_householdmemberlevel.dta"))

child_in <- child_in %>% mutate(childid = toupper(as.character(CHILDID)))
hh_in    <- hh_in    %>% mutate(childid = toupper(as.character(childid)))
memb_in  <- memb_in  %>% mutate(childid = toupper(as.character(childid)))

# ============================================================
# 3. LOAD PERU FILES
# ============================================================
child_pe <- read_dta(file.path(path_peru, "pe_yc_childlevel.dta"))
hh_pe    <- read_dta(file.path(path_peru, "pe_yc_householdlevel.dta"))
memb_pe  <- read_dta(file.path(path_peru, "pe_yc_householdmemberlevel.dta"))

child_pe <- child_pe %>% mutate(childid = toupper(as.character(childid)))
hh_pe    <- hh_pe    %>% mutate(childid = toupper(as.character(childid)))
memb_pe  <- memb_pe  %>% mutate(childid = toupper(as.character(childid)))

# ============================================================
# 4. INDIA — CREATE PARENT EDUCATION FROM MEMBER FILE
# ============================================================
parent_in <- memb_in %>%
  mutate(
    mother = ifelse(relate %in% c(2, "Mother"), 1, 0),
    father = ifelse(relate %in% c(1, "Father"), 1, 0)
  ) %>%
  group_by(childid) %>%
  summarise(
    mother_edu = ifelse(any(mother == 1), max(perfr3[mother == 1], na.rm = TRUE), NA),
    father_edu = ifelse(any(father == 1), max(perfr3[father == 1], na.rm = TRUE), NA)
  )

india <- child_in %>%
  left_join(hh_in, by = "childid") %>%
  left_join(parent_in, by = "childid")

india$country <- "India"

# ============================================================
# 5. PERU — CREATE PARENT FLAGS BUT NO EDUCATION VARIABLE
# ============================================================
parent_pe <- memb_pe %>%
  mutate(
    mother = ifelse(relate %in% c(2, "Mother"), 1, 0),
    father = ifelse(relate %in% c(1, "Father"), 1, 0)
  ) %>%
  group_by(childid) %>%
  summarise(
    mother_present = any(mother == 1),
    father_present = any(father == 1),
    mother_edu = NA,
    father_edu = NA
  )

peru <- child_pe %>%
  left_join(hh_pe, by = "childid") %>%
  left_join(parent_pe, by = "childid")

peru$country <- "Peru"

# ============================================================
# 6. HARMONIZE VARIABLE NAMES
# ============================================================

# SEX — remove incorrect duplicate first
if ("sex" %in% names(india)) india <- india %>% select(-sex)
if ("sex" %in% names(peru))  peru  <- peru  %>% select(-sex)

# Rename correct child sex variable
india <- india %>% rename(sex = chsex)
peru  <- peru  %>% rename(sex = chsex)

# Enrollment
india <- india %>% rename(enrolled = ENRSCHR3)
peru  <- peru  %>% rename(enrolled = enrschr3)

# PPVT score
india <- india %>% rename(ppvt_score = ppvt)
peru  <- peru  %>% rename(ppvt_score = ppvt)

# Household size (set to NA if missing)
if (!"hhsize" %in% names(india)) india$hhsize <- NA
if (!"hhsize" %in% names(peru))  peru$hhsize  <- NA

# ============================================================
# 7. CREATE DROPOUT INDICATOR
# ============================================================
india <- india %>%
  mutate(dropout = ifelse(enrolled == 0 | is.na(enrolled), 1, 0))

peru <- peru %>%
  mutate(dropout = ifelse(enrolled == 0 | is.na(enrolled), 1, 0))

# ============================================================
# 8. SELECT FINAL VARIABLES
# ============================================================
india_final <- india %>%
  select(childid, country, sex, enrolled, dropout,
         mother_edu, father_edu, ppvt_score,
         hhsize, region, typesite)

peru_final <- peru %>%
  select(childid, country, sex, enrolled, dropout,
         mother_edu, father_edu, ppvt_score,
         hhsize, region, typesite)

# ============================================================
# 9. FIX TYPE MISMATCHES BEFORE MERGING
# ============================================================

# typesite: make both character
india_final$typesite <- as.character(india_final$typesite)
peru_final$typesite  <- as.character(peru_final$typesite)

# region: make both character (sometimes Peru is numeric)
india_final$region <- as.character(india_final$region)
peru_final$region  <- as.character(peru_final$region)

# sex variable: force numeric so labels don't conflict
india_final$sex <- as.numeric(india_final$sex)
peru_final$sex  <- as.numeric(peru_final$sex)

# enrolled variable: force numeric for consistency
india_final$enrolled <- as.numeric(india_final$enrolled)
peru_final$enrolled  <- as.numeric(peru_final$enrolled)

# ============================================================
# OPTIONAL BUT GOOD PRACTICE: clean weird codes
# 77 / 88 / 99 usually mean "don't know / missing"
# ============================================================
india_final$enrolled[india_final$enrolled %in% c(77, 88, 99)] <- NA
peru_final$enrolled[peru_final$enrolled %in% c(77, 88, 99)] <- NA

# ============================================================
# 10. COMBINE BOTH COUNTRIES
# ============================================================
combined <- bind_rows(india_final, peru_final)

# ============================================================
# 11. SAVE FINAL CLEAN FILE
# ============================================================
write.csv(combined, "young_lives_dropout_analysis.csv", row.names = FALSE)

cat("\nSUCCESS! Final dataset saved as young_lives_dropout_analysis.csv\n")
