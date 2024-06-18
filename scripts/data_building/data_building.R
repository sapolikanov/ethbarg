library(here)
library(haven)
library(purr)
library(writexl)
library(kableExtra)
library(tidyverse)

data <- list.files(path = here("GitHub", "nondef", "ICRSR35355"),
                   pattern = "\\.$",
                   full.names = T)

data_list <- list.files(path = here("GitHub", "nondef", "ICPSR_35355"),
                        recursive = TRUE,
                        pattern = "\\.sav$",
                        full.names = TRUE)

data <- data_list |> 
  map(read_sav) |> 
  reduce(merge, by = c("ID", "YEAR", "REGION", "NAME"), all = F)
  map_dfr(read_sav)

kable(unique(data$NAME), format = "html")

data |> 
  distinct(NAME) |> 
  write_xlsx(here("GitHub", "nondef", "ICPSR_35355", "names.xlsx"))

