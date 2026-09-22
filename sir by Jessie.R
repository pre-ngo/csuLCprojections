library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# ============================================================
# USER SETTINGS
# ============================================================
LC_FILE  <- "/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/lung cancer.csv"
REF_FILE <- "/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/sir_refs.xlsx"

# USA for prototype
COUNTRY_CODE <- "USA"

# Projection horizon
FORECAST_END <- 2050

# Smoking -> lung cancer lag
LAG_YEARS <- 25

# Output folders
dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("fig/projection", showWarnings = FALSE, recursive = TRUE)


# ============================================================
# FUNCTIONS 
# ============================================================

# Convert mortality rate -> SIR
get_sir <- function(rate, rate_min, rate_max) {(rate - rate_min) / (rate_max - rate_min)}

# Convert SIR -> mortality rate
sir_to_rate <- function(sir, rate_max, rate_min) {sir * rate_max + (1 - sir) * rate_min}


# ============================================================
# READ DATA
# ============================================================

lc_data <- read.csv(LC_FILE)
sir_refs <- readxl::read_xlsx(REF_FILE)

# Quick inspection
print(head(lc_data))
print(sir_refs)


# ============================================================
# PREPARE OBSERVED LUNG CANCER MORTALITY DATA
# ============================================================

d_all <- lc_data %>%
# Mortality only
filter(type == "mortality") %>%

# Reference rates begin at age 35
filter(age != 19L, age >= 8) %>%
  
# CPS-II reference has only 80+, so collapse 80-84 and 85+ into 80+
mutate(age = case_when (age == 18 ~ 17, TRUE ~ age), 
       age_grp = case_when (age_grp == "80-84" ~ "80+", age_grp == "85+" ~ "80+", TRUE ~ age_grp)) %>%
  
# Sum cases and person-years after collapsing 80+
group_by (type, country, iso3Code, year, sex, cancer, icd, age, age_grp) %>%
summarise(cases = sum (cases, na.rm = TRUE), py = sum(py, na.rm = TRUE), .groups = "drop") %>%
  
# Attach never-smoker and current-smoker reference rates
left_join (sir_refs, by = c("age", "age_grp", "sex")) %>%
  
# Age-specific observed mortality per 100,000
mutate (rate_obs = 1e5 * cases / py) %>%
  
# Observed raw SIR
mutate (sir_raw = get_sir (rate = rate_obs, rate_min = rate_never, rate_max = rate_current))

head(d_all)


# ============================================================
# QC VARIABLES
# ============================================================

d_all <- d_all %>%
# Current-smoker mortality should conceptually be higher than never-smoker mortality.
mutate (ref_order_issue =rate_current <= rate_never,
        
# Raw observed SIR outside intended 0-1 scale
sir_below_zero = sir_raw < 0,
sir_above_one = sir_raw > 1,
sir_out_of_range = sir_raw < 0 | sir_raw > 1,
    
# Is SIR suitable for scenario manipulation?
projection_eligible =
!is.na(sir_raw) &
is.finite(sir_raw) &
sir_raw >= 0 &
sir_raw <= 1 &
!ref_order_issue)


# ============================================================
# SAVE QC TABLE
# ============================================================

d_qc <- d_all %>%
filter (ref_order_issue | sir_out_of_range | !is.finite(sir_raw)) %>%
select (country, iso3Code, year, sex, age, age_grp,
        rate_obs, rate_never, rate_current,
        sir_raw,
        ref_order_issue, sir_below_zero, sir_above_one)

write.csv (d_qc, "~/Desktop/SIR_QC_all_countries.csv", row.names = FALSE)


# ============================================================
# SELECT ONE COUNTRY
# ============================================================

d_country <- d_all %>%
filter (iso3Code == COUNTRY_CODE)

# Check country
print(unique(d_country$country))


# ============================================================
# OBSERVED AGE-SPECIFIC MORTALITY PLOT
# ============================================================

p_observed_mortality <- d_country %>%

# Remove noisier younger age groups for plotting
filter (age > 10) %>%
ggplot (aes ( x = year, y = rate_obs, colour = age_grp)) +
geom_line () + facet_wrap (~ sex, scales = "free_y") +
theme_bw () + labs ( title = paste0 (unique (d_country$country),": observed lung cancer mortality"),
x = "Year", y ="Age-specific LC mortality per 100,000", colour = "Age group")
ggsave ("~/Desktop/01_observed_mortality.png", p_observed_mortality,
width = 9, height = 5, dpi = 300)

# ============================================================
# OBSERVED SIR PLOT
# ============================================================

p_observed_sir <- d_country %>%
filter (age > 10) %>%
ggplot (aes (x = year, y = sir_raw, colour = age_grp)) +
geom_hline (yintercept = 0, linetype = "dashed") +
geom_hline (yintercept = 1, linetype = "dashed") +
geom_line () + facet_wrap (~ sex) + theme_bw() +
labs (title = paste0 (unique(d_country$country),": observed SIR"),
      x = "Outcome year", y = "Smoking Impact Ratio", colour = "Age group")
ggsave ("~/Desktop/02_observed_SIR.png", p_observed_sir, width = 9, height = 5, dpi = 300)


# ============================================================
# LAGGED SIR PLOT
# ============================================================

# A mortality observation is interpreted as reflecting smoking exposure approximately LAG_YEARS earlier.

p_lagged_sir <- d_country %>%
filter (age > 10) %>%
mutate (exposure_year = year - LAG_YEARS) %>%
ggplot (aes (x = exposure_year, y = sir_raw, colour = age_grp)) +
geom_line() + facet_wrap (~ sex) + theme_bw() + labs (title =paste0 (unique(d_country$country),": SIR shifted by ",LAG_YEARS," years"),
                    x = "Approximate smoking exposure year", y = "SIR", colour = "Age group")
ggsave ("~/Desktop/03_lagged_SIR.png", p_lagged_sir, width = 9, height = 5, dpi = 300)


# ============================================================
# FIND LAST COMPLETE OBSERVED YEAR
# ============================================================

# Number of age groups expected for each sex (based on the SIR reference table)
expected_age_groups <- sir_refs %>%
distinct (sex, age, age_grp) %>%
count (sex, name = "n_expected")

# Determine years with complete age coverage
complete_years <- d_country %>%
filter (!is.na(rate_obs), is.finite(rate_obs), !is.na(sir_raw), is.finite(sir_raw)) %>%
distinct (sex, year, age, age_grp) %>%
count (sex, year, name = "n_available") %>%
left_join (expected_age_groups, by = "sex") %>%
filter (n_available == n_expected)

# Last complete observed year separately for each sex

last_year_by_sex <- complete_years %>%
group_by (sex) %>%
summarise (last_obs_year = max(year), .groups = "drop")
print(last_year_by_sex)

# Female: 2022; Male: 2022 #

# ============================================================
# CREATE BASELINE DATASET
# ============================================================

d_baseline <- d_country %>%
inner_join (last_year_by_sex, by = "sex") %>%
filter (year == last_obs_year) %>%
select (country, iso3Code, sex, cancer, icd,
        age, age_grp, last_obs_year, cases, py,
        rate_obs, rate_never, rate_current, sir_raw,
        ref_order_issue, sir_out_of_range, projection_eligible) %>%
rename (sir_start = sir_raw, rate_start = rate_obs, py_start = py)

# Check baseline
print(d_baseline)


# ============================================================
# EXTEND EACH AGE GROUP TO 2050
# ============================================================

d_projection_base <- d_baseline %>%
rowwise() %>%
mutate (outcome_year = list (seq (from = last_obs_year, to = FORECAST_END, by = 1))) %>%
ungroup() %>%
unnest (outcome_year) %>%
# Approximate exposure year corresponding to each cancer outcome year
mutate (exposure_year = outcome_year - LAG_YEARS,
        baseline_exposure_year = last_obs_year - LAG_YEARS,
        final_exposure_year = FORECAST_END - LAG_YEARS)


# ============================================================
# FUNCTION TO GENERATE SIR SCENARIOS
# ============================================================

make_sir_scenario <- function (data, scenario_name, target_multiplier = 1, target_zero = FALSE) 
{data %>% mutate (scenario = scenario_name,
# Progress from baseline to end of projection
progress = (outcome_year - last_obs_year) / (FORECAST_END - last_obs_year),
progress = pmin (pmax(progress, 0), 1),
# Target SIR
target_sir = case_when (target_zero ~0, TRUE ~ sir_start * target_multiplier),
# Only manipulate SIR if:
  # - raw SIR is between 0 and 1
  # - current-smoker reference > never-smoker reference
# Otherwise, hold it constant for the prototype to avoid silently inventing a correction for problematic reference data.
sir_projected = case_when (!projection_eligible ~ sir_start, TRUE ~ sir_start + progress * (target_sir - sir_start)))}


# ============================================================
# BUILD 3 PROTOTYPE SCENARIOS
# ============================================================

# Scenario A: SIR stays exactly where it was at the last observed year
scenario_constant <-make_sir_scenario (d_projection_base, scenario_name = "Constant SIR", target_multiplier = 1)

# Scenario B: SIR reaches half of baseline by 2050
scenario_half <-make_sir_scenario (d_projection_base, scenario_name ="50% of baseline SIR by 2050", target_multiplier = 0.5)

# Scenario C: SIR reaches zero by 2050
scenario_zero <- make_sir_scenario (d_projection_base, scenario_name = "SIR = 0 by 2050", target_zero = TRUE)

# Combine scenarios
d_projection <- bind_rows (scenario_constant, scenario_half, scenario_zero)


# ============================================================
# CONVERT PROJECTED SIR -> MORTALITY RATE
# ============================================================

d_projection <- d_projection %>%
mutate (mortality_projected = sir_to_rate (sir = sir_projected, rate_max = rate_current, rate_min =rate_never))


# ============================================================
# TEMPORARY POPULATION / PERSON-YEARS
# ============================================================
# For now: Carry the final observed person-years forward; later replace this column with UN WPP Medium population.

d_projection <- d_projection %>%
mutate (population =py_start, population_source ="Placeholder: final observed person-years")


# ============================================================
# PROJECTED DEATHS
# ============================================================

d_projection <- d_projection %>%
mutate (deaths_projected = mortality_projected / 100000 * population)


# ============================================================
# WORLD STANDARD POPULATION FOR AGES 35+
# ============================================================

# Same Standard weights used previously for calculating ASR
# 80+ combines: 80-84 = 500, 85+ = 500 -> therefore 80+ = 1000

world_standard_35plus <- tibble (age_grp = c ("35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"),
world_weight = c (6000,6000,6000,5000,4000,4000,3000,2000,1000,1000))


# Normalize weights over 35+ ONLY.
# This produces a truncated 35+ ASR, not the conventional all-age ASR.

world_standard_35plus <-world_standard_35plus %>%
mutate (weight_35plus = world_weight /sum(world_weight))
print (world_standard_35plus)
print (sum (world_standard_35plus$weight_35plus))


# ============================================================
# ADD STANDARD WEIGHTS
# ============================================================

d_projection <- d_projection %>%
left_join (world_standard_35plus,by = "age_grp")


# ============================================================
# CALCULATE CRUDE RATE + ASR 35+
# ============================================================

d_summary <- d_projection %>%
group_by (country, iso3Code, sex, scenario, outcome_year) %>%
summarise (
# Total projected deaths
deaths_projected =sum (deaths_projected, na.rm = TRUE),
# Total population
population = sum (population, na.rm = TRUE),
# Crude mortality
crude_mortality = 100000 *deaths_projected /population,
# Provisional truncated ASR 35+
ASR_35plus = sum (mortality_projected *weight_35plus, na.rm = TRUE),
# QC indicators
n_age_groups = n_distinct (age_grp), n_reference_order_issues =sum (ref_order_issue, na.rm = TRUE),
n_baseline_SIR_out_of_range = sum (sir_out_of_range, na.rm = TRUE), .groups = "drop")


# ============================================================
# 22. SAVE RESULTS
# ============================================================

write.csv (d_projection, paste0 ("~/Desktop/", COUNTRY_CODE, "_SIR_projection_age_specific.csv"), row.names = FALSE)
write.csv (d_summary, paste0 ("~/Desktop/", COUNTRY_CODE, "_SIR_projection_summary.csv"), row.names = FALSE)
write.csv (d_country, paste0 ("~/Desktop/", COUNTRY_CODE, "_observed_SIR.csv"), row.names = FALSE)


# ============================================================
# PLOT PROJECTED SIR
# ============================================================
p_projected_sir <- d_projection %>%
filter (age > 10) %>%
ggplot (aes (x = outcome_year, y = sir_projected, colour = age_grp))+
geom_line() + facet_grid (sex ~ scenario) + theme_bw() +
labs (title = paste0 (unique(d_country$country), ": projected SIR"),
      x ="Lung cancer outcome year", y ="Projected SIR", colour ="Age group")
ggsave ("~/Desktop/04_projected_SIR.png", p_projected_sir, width = 13, height = 7, dpi = 300)


# ============================================================
# PLOT PROJECTED AGE-SPECIFIC MORTALITY
# ============================================================

p_projected_mortality <- d_projection %>%
filter (age > 10) %>%
ggplot (aes (x =outcome_year, y =mortality_projected, colour =age_grp))+
geom_line() +facet_grid (sex ~ scenario, scales = "free_y") + theme_bw() +
labs (title = paste0 (unique (d_country$country), ": projected lung cancer mortality"),
      x ="Year", y ="Age-specific mortality per 100,000", colour ="Age group")
ggsave ("~/Desktop/05_projected_mortality.png", p_projected_mortality, width = 13, height = 7, dpi = 300)


# ============================================================
# PLOT PROJECTED DEATHS
# ============================================================

p_deaths <- d_summary %>%
ggplot (aes (x =outcome_year, y =deaths_projected, colour =scenario)) +
geom_line (linewidth = 1) + facet_wrap (~ sex, scales = "free_y") + theme_bw() +
labs (title = paste0 (unique(d_country$country), ": projected lung cancer deaths"), subtitle ="Population currently uses placeholder final observed person-years",
      x ="Year", y ="Projected deaths", colour ="Scenario")
ggsave ("~/Desktop/06_projected_deaths.png", p_deaths, width = 9, height = 5, dpi = 300)


# ============================================================
# PLOT CRUDE MORTALITY
# ============================================================

p_crude <- d_summary %>%
ggplot (aes (x =outcome_year, y =crude_mortality, colour = scenario)) +
geom_line (linewidth = 1) + facet_wrap(~ sex) + theme_bw() +
labs (title = paste0 (unique(d_country$country), ": projected crude lung cancer mortality"),
      x ="Year", y ="Crude mortality per 100,000", colour = "Scenario")
ggsave ("~/Desktop/07_projected_crude_rate.png", p_crude, width = 9, height = 5, dpi = 300)


# ============================================================
# PLOT PROVISIONAL ASR 35+
# ============================================================

p_asr <- d_summary %>%
ggplot (aes (x =outcome_year, y =ASR_35plus, colour =scenario)) +
geom_line (linewidth = 1) + facet_wrap (~ sex) + theme_bw() +
labs (title = paste0 (unique(d_country$country), ": projected ASR (35+ prototype)"), subtitle = "World Standard Population weights renormalised over ages 35+",
      x ="Year", y ="ASR 35+ per 100,000",  colour ="Scenario")
ggsave ("~/Desktop/08_projected_ASR35plus.png", p_asr, width = 9, height = 5, dpi = 300)


# ============================================================
# BASIC CHECKS
# ============================================================

# Projection should reproduce the final observed mortality at the baseline year.
baseline_check <- d_projection %>%
filter (outcome_year == last_obs_year, scenario =="Constant SIR") %>%
mutate (difference = mortality_projected - rate_start) %>%
select (sex, age_grp, rate_start, mortality_projected, difference)
print (baseline_check)

# Difference should be approximately zero
print (summary (baseline_check$difference))

write.csv (baseline_check, "~/Desktop/baseline_check.csv", row.names = FALSE)

# ============================================================
# FINAL CONSOLE SUMMARY
# ============================================================

cat ("\n========================================\n")
cat ("Projection complete\n")
cat ("Country:", COUNTRY_CODE, "\n")
cat ("Forecast end:", FORECAST_END, "\n")
cat ("Lag:", LAG_YEARS, "years\n")
cat ("Population: PLACEHOLDER ONLY\n")
cat ("ASR: provisional 35+ ASR only\n")
cat ("Check ~/Desktop/ for the exported CSV and PNG files.\n")
cat ("========================================\n")