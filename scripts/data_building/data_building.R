library(here)
library(haven)
library(purrr)
library(writexl)
library(readxl)
library(kableExtra)
library(tidyverse)

# Data

# Hand and merge
hm <- read_excel(here("data", "data_raw", "hand_and_merge.xlsx"), na = "NA")

# ROR
data_list <- list.files(path = here("data", "data_raw", "ICPSR_35355"),
                        recursive = TRUE,
                        pattern = "\\.sav$",
                        full.names = TRUE)

data <- data_list |> 
  map(read_sav) |> 
  reduce(merge, by = c("ID", "YEAR", "REGION", "NAME"), all = F)

# Protests and riots
beissinger_protest <- read_xls(here("data", "data_raw", "PROTEST.xls"))
  
# data |> distinct(NAME) |>  write_xlsx(here("GitHub", "nondef", "ICPSR_35355", "names.xlsx"))

# Main data
data_select <- data |> 
  select(ID, YEAR, REGION, NAME, REPUBLIC, BORDER, AREA, ECONOMZONE,
         DISTANCE, FEDDISTRICT, 
         AVSAVINGS_FC, AVSAVINGS_RUR, BTRANSFERS, GRPPC, GRPGROWTH, 
         GINI, UNEMP, MIGR, ALCO_M, DOCTORS, THEATRES, PROD_OIL, POP, POP_GRW, 
         BEDUCATION, BINDUSTRY, BHEALTHCARE, BREVENUE, BNATRESTAX, RUSPOPUL,
         RUSSHARE, GRP_AGRI = CHAPTER_A, GRP_EDUC = CHAPTER_F,
         GRP_IND = CHAPTER_M, GRP_FIN = CHAPTER_J, GRP_SOC = CHAPTER_N, 
         GRP_HOTELS = CHAPTER_H, LOSSMAKERSMANUF, LOSSMAKERSAGRI, 
         LOSSMAKERSBUILD, LOSSMAKERS, GRP_MANUF = CHAPTER_D, 
         GRP_MINE = CHAPTER_C, EMPL_HIGH, NATRESTAXSHARE2, NATRESTAXSHARE1,
         CITY, LOSSMANUF, LOSSNATRES, FPMANUF, FPNATRES, ROADDENSITY) 

data_agg <- data_select |> 
  group_by(ID, REGION, NAME, ECONOMZONE, BORDER, DISTANCE, 
           FEDDISTRICT) |> 
  summarize(AREA = mean(AREA[YEAR <= 1998]),
            GRPPC = mean(GRPPC[YEAR <= 1998 & YEAR >= 1995]),
            GINI = mean(GINI[YEAR <= 1998 & YEAR >= 1995]),
            UNEMP = mean(UNEMP[YEAR <= 1998 & YEAR >= 1993], na.rm = T),
            MIGR = mean(MIGR[YEAR <= 1998], na.rm = T),
            MIGR_DUMMY = if_else(MIGR > 0, "Positive", "Negative"),
            DOCTORS = mean(DOCTORS[YEAR <= 1998]),
            THEATRES = mean(THEATRES[YEAR <= 1998], na.rm = T),
            PROD_OIL = if_else(all(is.na(PROD_OIL[YEAR <= 1998])), 0, 
                               mean(PROD_OIL[YEAR <= 1998], na.rm = T)),
            PROD_OIL_DUMMY = if_else(PROD_OIL != 0, "Oil extractor", 
                                     "Not an oil extractor"),
            POP = mean(POP[YEAR <= 1998]),
            POP_1990 = POP[YEAR == 1990],
            POP_10000 = POP/10,
            POP_LOG = log(POP),
            POP_DENSITY = POP*1000 / AREA,
            BREVENUE = mean(BREVENUE[YEAR <= 1998], na.rm = T),
            BREVENUE_LOG = log(BREVENUE),
            RUSSHARE = RUSSHARE[YEAR == 1990],
            LOSSMAKERSMANUF = LOSSMAKERSMANUF[YEAR == 1995],
            LOSSMAKERSAGRI = LOSSMAKERSAGRI[YEAR == 1995],
            LOSSMAKERS = mean(LOSSMAKERS[YEAR <= 1995], na.rm = T),
            EMPL_HIGH = mean(EMPL_HIGH[YEAR <= 1998], na.rm = T),
            CITY = mean(CITY[YEAR <= 1998], na.rm = T),
            ROAD_DENSITY = mean(ROADDENSITY[YEAR <= 1998], na.rm = T),
            .groups = "drop") |>
  mutate(across(-c(ID, REGION, NAME), ~ if_else(is.nan(.), NA, .)),
         DISTANCE_1000 = DISTANCE/1000)

data_select |> 
  group_by(YEAR) |> 
  select_if(function(x) any(is.na(x))) |> 
  summarise_all(funs(sum(is.na(.)))) -> NA_mydf

ggplot(data_select, aes(x = YEAR, y = CITY, color = REGION)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1990, 2010, 2)) +
  theme(legend.position = "none")

ggplot(data_select, aes(x = PROD_OIL, y = REGION, color = REGION)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")

# Beisenger 
bprotest_sum <- beissinger_protest |> 
  filter(REPUBLIC == "RSFSR") |> 
  group_by(PROVINCE, PROVNUMBER) |> 
  summarize(sum_ethno = sum(ETHNOFLAG == "Y"),
            sum_secession = sum(SECFLAG == "Y"),
            sum_otherter = sum(OTHTERFLAG == "Y"),
            sum_cult = sum(LINGFLAG == "Y"),
            sum_allprotest = sum(sum_ethno + sum_secession + sum_otherter 
                                 + sum_cult))

# Building...
  
hm.date <- hm |> 
  drop_na(TREATY_DATE) |> 
  mutate(TREATY_MONTH = month(as_date(TREATY_DATE, format = "%d.%m.%Y")),
         TREATY_YEAR = year(as_date(TREATY_DATE, format = "%d.%m.%Y")))

built <- hm |> 
  mutate(TREATY_DATE = as_date(TREATY_DATE, format = "%d.%m.%Y"),
         TREATY_DUMMY = if_else(!is.na(TREATY_DATE), 1, 0),
         TREATY_RANK = rank(TREATY_DATE),
         TREATY_DISCRETE = as.numeric(TREATY_DATE - min(TREATY_DATE, na.rm = T)),
         TREATY_CAT3 = case_when(is.na(TREATY_DATE) ~ "No Treaty",
                                 TREATY_DATE < 1996 ~ "Before 1996",
                                 TREATY_DATE >= 1996 ~ "After 1996"),
         TREATY_CAT2 = if_else(TREATY_CAT3 != "No Treaty", TREATY_CAT3, NA),
         TREATY_CAT2_NUM = case_when(TREATY_CAT3 == "Before 1996" ~ 0,
                                     TREATY_CAT3 == "After 1996" ~ 1,
                                     TREATY_CAT3 == "No Treaty" ~ NA)) |>
  full_join(bprotest_sum, by = c("PROVNUM_PROTESTS" = "PROVNUMBER")) |> 
  full_join(data_agg, by = c("NAME" = "NAME")) |> 
  mutate(pc_ethno = sum_ethno/(POP_1990/1000),
         pc_secession = sum_secession/(POP_1990/1000),
         pc_otherter = sum_otherter/(POP_1990/1000),
         pc_cult = sum_cult/(POP_1990/1000),
         pc_allprotest = sum_allprotest/(POP_1990/1000)) 

write_rds(built, here("data", "data_built", "built.rds"))

# Plot
treaties_date <- data.frame("month" = rep(seq(1, 12, 1), 5),
           "year" = rep(seq(from = 1994, to = 1998, 1), 12)) |> 
  arrange(year, month) |> 
  left_join(hm.date, by = join_by("month" == "TREATY_MONTH", 
                                  "year" == "TREATY_YEAR")) |> 
  transmute(my = make_date(year, month),
            treaty = if_else(!is.na(TREATY_DATE), 1, 0)) |> 
  group_by(my) |> 
  summarize(sum_treaty = sum(treaty)) |> 
  ungroup() |> 
  mutate(treaty_cs = cumsum(sum_treaty)) |> 
  ggplot(aes(x = my, y = treaty_cs, fill = sum_treaty)) +
    geom_bar(stat = "identity", color = "#00204d") + 
    scale_x_date(limits = as_date(c("1994-01-01", "1998-06-01"))) +
    scale_fill_viridis_c(option = "E") +
    labs(x = NULL, y = "Cumulative number of treaties\n", fill = "Treaties\nsigned\nin month\n") +
    theme_minimal()

# ggsave(plot = treaties_date, filename = here("paper", "td.png"), width = 10, height = 6, dpi = 300)

# Plot 2

protests_dist <- bprotest_sum |> 
  pivot_longer(cols = -PROVINCE, names_to = "type", values_to = "sum") |> 
  mutate(type = case_when(type == "sum_ethno" ~ "Ethno-nationalist", 
                          type == "sum_secession" ~ "Secessionist",
                          type == "sum_otherter" ~ "Other territorial",
                          type == "sum_cult" ~ "Language and culture",
                          type == "sum_national" ~ "Support for efforts\nto nationalize")) |> 
  ggplot(aes(y = reorder(PROVINCE, sum), x = sum, fill = type)) +
  geom_bar(stat = "identity") +
  labs(x = "\nNumber of protests", y = NULL,
       fill = "Type of protest:") +
  scale_fill_manual(values = c("#542E71", "#F9ED69", "#FB3640", "#FDCA40", "#A799B7")) +
  theme_minimal() +
  theme(legend.position = "inside", 
        legend.position.inside = c(.8, .8))

# ggsave(plot = protests_dist, filename = here("paper", "pd.png"), width = 10, height = 8, dpi = 300)
