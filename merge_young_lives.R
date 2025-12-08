rm(list = ls())

library(haven)
library(dplyr)

path_peru <- "/Users/jasnooranand/Documents/Round 3/r3_yc/peru"


child_pe <- read_dta(file.path(path_peru, "pe_yc_childlevel.dta"))
hh_pe    <- read_dta(file.path(path_peru, "pe_yc_householdlevel.dta"))
memb_pe  <- read_dta(file.path(path_peru, "pe_yc_householdmemberlevel.dta"))

child_pe$childid <- as.character(child_pe$childid)
hh_pe$childid    <- as.character(hh_pe$childid)
memb_pe$childid  <- as.character(memb_pe$childid)

pe_base <- child_pe %>%
  left_join(hh_pe,   by = "childid") %>%
  left_join(memb_pe, by = "childid")

sect8 <- read_dta(file.path(path_peru, "pe_yc_stblhhsec8childwork.dta"))
sect8$childid <- as.character(sect8$childid)

pe_base <- pe_base %>% left_join(sect8, by = "childid")

rm(sect8); gc()


section3_files <- list.files(path_peru, pattern = "stblhhsec3", full.names = TRUE)

for (f in section3_files) {
  
  message("Merging Section 3 file: ", basename(f))
  
  # Load ONLY CHILDID column — safe
  df <- read_dta(f)[, "childid", drop = FALSE]
  df$childid <- as.character(df$childid)
  
  pe_base$childid <- as.character(pe_base$childid)
  
  # Memory-efficient base R merge
  pe_base <- merge(pe_base, df, by = "childid", all.x = TRUE, sort = FALSE)
  
  rm(df)
  gc()
}


write.csv(pe_base, "peru_safe_merged.csv", row.names = FALSE)
message("PERU MERGE COMPLETED SAFELY ")














library(haven)
library(dplyr)
library(purrr)

path_india <- "/Users/jasnooranand/Documents/Round 3/r3_yc/india"
path_peru  <- "/Users/jasnooranand/Documents/Round 3/r3_yc/peru"

# ================================================================
# FUNCTION: Load and merge ALL .dta files in a folder by childid
# ================================================================

merge_all_country <- function(path, country_name) {
  
  files <- list.files(path, pattern = "\\.dta$", full.names = TRUE)
  
  message("\n📂 Loading datasets for: ", country_name, "\n")
  
  data_list <- map(files, function(f) {
    df <- read_dta(f)
    
    # identify childid variable
    id_candidates <- names(df)[tolower(names(df)) %in%
                                 c("childid", "child_id", "idchild", "chldid")]
    
    if (length(id_candidates) == 0) {
      message("⚠ Skipping (no childid): ", basename(f))
      return(NULL)
    }
    
    idname <- id_candidates[1]
    
    # Standardize to "childid"
    names(df)[names(df) == idname] <- "childid"
    
    df$childid <- as.character(df$childid)
    
    message("✔ Loaded: ", basename(f))
    
    return(df)
  })
  
  # Remove files without childid
  data_list <- data_list[!sapply(data_list, is.null)]
  
  message("\n🔗 Merging all datasets for ", country_name, " ...")
  
  merged <- reduce(data_list, left_join, by = "childid")
  
  merged$country <- country_name
  
  message("\n🎉 MERGE COMPLETED FOR ", country_name)
  message("Rows: ", nrow(merged), " | Columns: ", ncol(merged), "\n")
  
  return(merged)
}

# ================================================================
# RUN MERGE FOR INDIA
# ================================================================
message("\n================ INDIA MERGE START ================")
india_final <- merge_all_country(path_india, "India")

write.csv(india_final, "india_merged_everything.csv", row.names = FALSE)
message("🇮🇳 Saved: india_merged_everything.csv\n")

# ================================================================
# RUN MERGE FOR PERU
# ================================================================
message("\n================ PERU MERGE START ================")
peru_final <- merge_all_country(path_peru, "Peru")

write.csv(peru_final, "peru_merged_everything.csv", row.names = FALSE)
message("🇵🇪 Saved: peru_merged_everything.csv\n")

# ================================================================
# COMBINE INDIA + PERU
# ================================================================
message("\n================ COMBINING ALL ================")

combined <- bind_rows(india_final, peru_final)

write.csv(combined, "young_lives_combined_everything.csv", row.names = FALSE)

message("\n🌍 FINAL COMBINED DATASET CREATED!")
message("➡ Saved as young_lives_combined_everything.csv\n")
