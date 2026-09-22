install.packages(c("tidyverse"))
library(tidyverse)
lc <- read_csv("/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/lung cancer.csv")
head(lc)
names(lc)
glimpse(lc)

world_standard <- tibble(
  age_grp = c(
    "0-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85+"
  ),
  
  world_weight = c(
    12000,
    10000,
    9000,
    9000,
    8000,
    8000,
    6000,
    6000,
    6000,
    6000,
    5000,
    4000,
    4000,
    3000,
    2000,
    1000,
    500,
    500
  )
)
sum(world_standard$world_weight)

lc_clean <- lc %>%
  filter(!is.na(age_grp))

unique(lc_clean$age_grp)

lc_asr <- lc_clean %>%
  left_join(world_standard, by = "age_grp")

head(lc_asr)

lc_asr <- lc_asr %>%
  mutate(
    age_specific_rate = (cases / py) * 100000
  )

lc_asr %>%
  select(
    country,
    year,
    sex,
    type,
    age_grp,
    cases,
    py,
    age_specific_rate
  ) %>%
  head(20)

lc_asr <- lc_asr %>%
  mutate(
    weighted_rate =
      age_specific_rate * (world_weight / 100000)
  )

asr_results <- lc_asr %>%
  
  group_by(
    type,
    country,
    iso3Code,
    year,
    sex,
    cancer,
    icd
  ) %>%
  
  summarise(
    
    n_age_groups = n_distinct(age_grp),
    
    valid_py = all(!is.na(py) & py > 0),
    
    asr_raw = sum(weighted_rate),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    complete_age_groups = n_age_groups == 18,
    
    ASR = if_else(
      complete_age_groups & valid_py,
      asr_raw,
      NA_real_
    )
  ) %>%
  
  select(
    type,
    country,
    iso3Code,
    year,
    sex,
    cancer,
    icd,
    ASR,
    n_age_groups,
    complete_age_groups,
    valid_py
  )

View(asr_results)
write.csv(asr_results, "~/Desktop/asr_results.csv", row.names = FALSE)
write_csv(lc_asr,"~/Desktop/asr_results.csv", row.names = FALSE)