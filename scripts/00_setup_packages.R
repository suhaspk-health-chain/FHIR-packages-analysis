pkgs <- c(
  "jsonlite","dplyr","tidyr","stringr","lubridate","readr","arrow",
  "ggplot2","scales","forcats","DataExplorer","cowplot","magick", "grid"
)
new <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))


