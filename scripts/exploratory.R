library(tidyverse)
library(glue)

data_folder <- "C:/Users/Subhayan/NUS Dropbox/Subhayan Mukerjee/Research Projects/Bihar Floods/"

survey_dat <- read_csv(glue("{data_folder}/data/survey_clean.csv"))

column_names <- read_csv(glue("{data_folder}data/column-names.csv"))

dat <- survey_dat |>
  rename_with(~ column_names$RenamedColumn)

glimpse(dat)
