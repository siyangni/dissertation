library(haven)
library(dplyr)

data_dir <- "/home/siyang/dissertation_folder/data"
dta_path <- file.path(data_dir, "mcs4_cm_self_completion_final.dta")
rds_path <- file.path(data_dir, "merged1203.rds")
out_path <- file.path(data_dir, "merged0914.rds")

sc <- read_dta(dta_path)
sc <- haven::zap_labels(sc)
if (!"dccnum00" %in% names(sc)) stop("dccnum00 not found in .dta")
if (!"mcsid" %in% names(sc)) stop("mcsid not found in .dta")

if (inherits(sc$dccnum00, "haven_labelled")) sc$dccnum00 <- as.numeric(sc$dccnum00)
if (inherits(sc$mcsid, "haven_labelled"))   sc$mcsid   <- as.character(sc$mcsid)

sc <- dplyr::filter(sc, dccnum00 == 1)

using_df <- readRDS(rds_path)
if (!"mcsid" %in% names(using_df)) stop("mcsid not found in merged1203.rds")

sc <- dplyr::mutate(sc, mcsid = as.character(mcsid))
using_df <- dplyr::mutate(using_df, mcsid = as.character(mcsid))

dup_sc <- sc %>% count(mcsid, name = "n") %>% filter(n > 1)
dup_using <- using_df %>% count(mcsid, name = "n") %>% filter(n > 1)
if (nrow(dup_sc) > 0) stop(sprintf("1:1 merge violation: %d duplicate mcsid in .dta (master)", nrow(dup_sc)))
if (nrow(dup_using) > 0) stop(sprintf("1:1 merge violation: %d duplicate mcsid in .rds (using)", nrow(dup_using)))

# FULL OUTER JOIN - equivalent to Stata's default merge behavior
# This keeps ALL participants from BOTH datasets (union of all mcsids)
merged <- dplyr::full_join(sc, using_df, by = "mcsid")

# Create merge indicator variable (similar to Stata's _merge)
merged$merge_indicator <- case_when(
  merged$mcsid %in% sc$mcsid & merged$mcsid %in% using_df$mcsid ~ "both",
  merged$mcsid %in% sc$mcsid & !merged$mcsid %in% using_df$mcsid ~ "self_completion_only", 
  !merged$mcsid %in% sc$mcsid & merged$mcsid %in% using_df$mcsid ~ "main_data_only"
)

saveRDS(merged, out_path)
cat("=== MERGE RESULTS (FULL OUTER JOIN) ===\n")
cat("Saved merged dataset to:", out_path, "\n")
cat("Rows in self-completion data (filtered):", nrow(sc), "\n")
cat("Rows in main longitudinal data:", nrow(using_df), "\n")
cat("Rows in merged dataset:", nrow(merged), "\n")
cat("\n=== MERGE BREAKDOWN ===\n")
merge_table <- table(merged$merge_indicator)
print(merge_table)
cat("\nExplanation:\n")
cat("- both: Participants in BOTH datasets\n")
cat("- self_completion_only: Participants ONLY in self-completion data\n") 
cat("- main_data_only: Participants ONLY in main longitudinal data\n")
cat("\nThis matches Stata's default merge behavior (full outer join).\n")
