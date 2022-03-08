# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packaged
pacman::p_load(conflicted,
               dotenv,
               targets,
               tarchetypes,
               tidyverse,
               here,
               ggthemes,
               openxlsx,
               magrittr,
               lme4,
               emmeans,
               readxl)

conflict_prefer("filter", "dplyr")

pacman::p_load_gh("jhgille2/snfR")
