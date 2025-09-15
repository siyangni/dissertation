# Test the fixed script
library(pacman)
p_load(lavaan, semTools)

utils::globalVariables(c("merged_data", "recoded_parenting_age7"))

if (!exists("merged_data")) {
  data_path <- "/home/siyang/dissertation_folder/data"
  merged_data <- readRDS(file.path(data_path, "merged1203.rds"))
}

# Check if self-control variables are recoded, if not, run recoding script
if (sum(grepl("^sc(3|5|7|11|14|17)_", names(merged_data))) == 0) {
  cat("Running self-control recoding script...\n")
  source("/home/siyang/dissertation_folder/dissertation/scripts/recode_self_control.R")
}

# Now test the indicator check
ages <- c(3, 5, 7, 11, 14, 17)
item_suffixes <- c("task_completion","distracted","fidgeting","think_act","restless","temper","obedient","lying")

exists_at_all <- function(suf) all(paste0("sc", ages, "_", suf) %in% names(merged_data))
common_suf <- item_suffixes[sapply(item_suffixes, exists_at_all)]

cat("Common suffixes found:", length(common_suf), "\n")
cat("Suffixes:", paste(common_suf, collapse=", "), "\n")

if (length(common_suf) < 3L) {
  stop("Need at least 3 common indicators across all ages.")
} else {
  cat("SUCCESS: Found", length(common_suf), "common indicators across all ages\n")
}
