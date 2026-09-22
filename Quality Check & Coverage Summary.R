# QC: See how many valid observations #
library(tidyverse)

asr <- read_csv("/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/asr_results.csv",
show_col_types = FALSE)
glimpse(asr)

asr %>%
  mutate(ASR_missing = is.na(ASR)) %>%
  count(complete_age_groups,valid_py,ASR_missing)

# Coverage Summary: availability / coverage #
coverage_summary <- asr %>%
  
  group_by(country,type,sex) %>%
  
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    observed_years = n_distinct(year),
    valid_ASR_years =
      sum(
        complete_age_groups &
          valid_py &
          !is.na(ASR)
      ),
    
    invalid_ASR_years =
      sum(
      !complete_age_groups |
      !valid_py |
      is.na(ASR)),
    .groups = "drop") %>%
  
  mutate(
    expected_years =
    last_year - first_year + 1,
    missing_calendar_years =
    expected_years - observed_years,
    valid_coverage_percent =
      100 * valid_ASR_years / expected_years)

View(coverage_summary)
write.csv(coverage_summary, "~/Desktop/coverage_summary.csv", row.names = FALSE)


# Temporal Coverage Table #
library(tidyverse)

asr <- read_csv("/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/asr_results.csv",
                show_col_types = FALSE)
asr <- asr %>%
  mutate(valid_ASR =
           !is.na(ASR) &
           complete_age_groups == TRUE &
           valid_py == TRUE)

longest_run <- function(years)
{
  years <- sort(unique(years))
  if (length(years) == 0) {return(0)}
  groups <- cumsum(c(1,diff(years) != 1))
  max(as.numeric(table(groups)))
}

# country × type × sex temporal coverage #
temporal_coverage <- asr %>%
  group_by(country, iso3Code, type, sex) %>%
  summarise(observed_start =min(year), observed_end =max(year),observed_years =n_distinct(year),
            valid_start =
              if_else(
                any(valid_ASR),
                min(year[valid_ASR]),
                NA_integer_
              ),
            valid_end =
              if_else(
                any(valid_ASR),
                max(year[valid_ASR]),
                NA_integer_
              ),
            valid_years =sum(valid_ASR),
            invalid_years =sum(!valid_ASR),
            longest_valid_run =longest_run(year[valid_ASR]),
            valid_percent =100 *valid_years /observed_years,
            .groups = "drop")

View(temporal_coverage)
write.csv(temporal_coverage, "~/Desktop/temporal_coverage_By_Sex.csv", row.names = FALSE)

# Sex Combine By Years#
library(tidyverse)

asr <- read_csv(
  "/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/asr_results.csv",
  show_col_types = FALSE)

asr <- asr %>%
mutate(
valid_ASR =
!is.na(ASR) &
complete_age_groups == TRUE &
valid_py == TRUE
)

both_sex_years <- asr %>%
group_by(country, iso3Code, type, year) %>%
summarise(
male_valid =any(sex == "Male" & valid_ASR),
female_valid =any(sex == "Female" & valid_ASR),
both_sexes_valid =male_valid & female_valid,
.groups = "drop")

head(both_sex_years)

# Data Coverage Table #
# To calculate longest consecutive run of valid years #
longest_run <- function(years) {
  years <- sort(unique(years))
  if (length(years) == 0) {
  return(0)
  }
  groups <- cumsum(
  c(1, diff(years) != 1))
  max(as.numeric(table(groups)))
  }

type_coverage <- both_sex_years %>%
group_by(country,iso3Code,type) %>%
summarise(usable_start =
      if_else(
      any(both_sexes_valid),
      min(year[both_sexes_valid]),
      NA_integer_),
      usable_end =
      if_else(
        any(both_sexes_valid),
        max(year[both_sexes_valid]),
        NA_integer_),
      usable_years =
      sum(both_sexes_valid),
      longest_continuous_run =
      longest_run(year[both_sexes_valid]),
      both_sexes_available =
      any(both_sexes_valid),
      .groups = "drop")

coverage_table <- type_coverage %>%
pivot_wider(names_from = type,
            values_from = c(usable_start,usable_end,usable_years,longest_continuous_run,both_sexes_available),
            names_glue ="{type}_{.value}")

View(coverage_table)
write_csv(coverage_table,
"~/Desktop/lung_cancer_temporal_coverage_table.csv")