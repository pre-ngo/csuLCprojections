# Calculate SIRs 

# Set up 
library(dplyr)
library(ggplot2)

# Functions ####
get_sir <- function(rate, rate_min, rate_max) {
  (rate - rate_min) / (rate_max - rate_min)
}

sir_to_rate <- function(sir, rate_max, rate_min) {
  sir * rate_max + (1 - sir) * rate_min
}


# Analyse #### 
lc_data <- read.csv("lc_data/lc_data.csv")

sir_refs <- readxl::read_xlsx("sir_refs/sir_refs.xlsx")

# Calculate SIRs 
d_all <- lc_data %>%
  # Strip down data
  filter(type == "mortality") %>%
  filter(age != 19L & age >= 8) %>%
  # Collapse 80+ category
  mutate(
    age = case_when(age == 18 ~ 17, .default = age),
    age_grp = case_when(
      age_grp == "85+" ~ "80+",
      age_grp == "80-84" ~ "80+",
      .default = age_grp
    )
  ) %>%
  summarise(
    cases = sum(cases), py = sum(py),
    .by = c(type, country, iso3Code, year, sex, cancer, icd, age, age_grp)
  ) %>%
  # Add reference rates (from CPS-II; Peto et al. 1992)
  left_join( sir_refs, by = join_by(age, age_grp, sex) ) %>%
  # Calculate true rates
  mutate(rate_obs = 1e5 * cases/py) %>%
  # Calculate SIRs
  mutate(sir = get_sir(rate_obs, rate_never, rate_current)) %>%
  relocate(cases:py, .after = sir) %>%
  relocate(py_never:py_current, .after = py) %>%
  dplyr::select(-type)

d_sample <- d_all %>%
  filter(grepl("USA|FRA|ZAF|UZB",iso3Code))

d_sample2 <- d_sample %>%
  # Get crude SIR and mortality
  mutate(
    cases_never = (rate_never/1e5) * py_never,
    cases_current = (rate_current/1e5) * py_current,
    .by = c(country, iso3Code, year, sex, cancer, icd)
  ) %>%
  summarise(
    crude_obs = 1e5 * sum(cases) / sum(py),
    crude_sir = get_sir(
      rate = crude_obs, 
      rate_min = 1e5 * sum(cases_never) / sum(py_never),
      rate_max = 1e5 * sum(cases_current) / sum(py_current)
    ),
    .by = c(country, iso3Code, year, sex, cancer, icd)
  )


# Plot #### 

# Crude plots

mortality_plot <- d_sample2 %>%
  ggplot(aes(x = year, y = crude_obs, colour = country)) +
  geom_line() +
  facet_wrap(. ~ sex) +
  labs(
    x = "Year", y = "Crude LC Mortality",
    colour = NULL,
    caption = "Excluding ages under 35"
  ) +
  theme_bw() +
  theme(legend.position = "top")

sir_plot <- d_sample2 %>%
  ggplot(aes(x = year, y = crude_sir, colour = country)) +
  geom_line() +
  facet_wrap(. ~ sex) +
  labs(
    x = "Year", y = "Crude SIR",
    colour = NULL,
    caption = "Excluding ages under 35"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave(
  "fig/sir/mortality_plot.svg",
  mortality_plot,
  width = 4, height = 2, scale = 1.5
)

ggsave(
  "fig/sir/sir_plot.svg",
  sir_plot,
  width = 4, height = 2, scale = 1.5
)


# Age-specific plots

mortality_age_plot <- d_sample %>%
  filter(iso3Code == "USA") %>%
  filter(age > 10) %>%
  ggplot(aes(x = year, y = rate_obs, colour = age_grp)) +
  geom_line() +
  facet_wrap(. ~ sex) +
  labs(
    x = "Year", y = "LC Mortality",
    colour = "Age"
  ) +
  theme_bw() +
  theme(legend.position = "right")

sir_age_plot <- d_sample %>%
  filter(iso3Code == "USA") %>%
  filter(age > 10) %>% # remove noisy age groups
  ggplot(aes(x = year, y = sir, colour = age_grp)) +
  geom_line() +
  facet_wrap(. ~ sex) +
  labs(
    x = "Year", y = "SIR",
    colour = "Age"
  ) +
  theme_bw() +
  theme(legend.position = "right")

sir_lagged_plot <- d_sample %>%
  filter(iso3Code == "USA") %>%
  filter(age > 10) %>% # remove noisy age groups
  ggplot(aes(x = year - 25, y = sir, colour = age_grp)) +
  geom_line() +
  facet_wrap(. ~ sex) +
  coord_cartesian(xlim = c(1951,2022)) +
  labs(x = "Year", y = "SIR - 25 years", colour = "Age") +
  theme_bw() +
  theme(legend.position = "right")


ggsave(
  "fig/sir/mortality_age_plot.svg",
  mortality_age_plot,
  width = 4.5, height = 2, scale = 1.5
)

ggsave(
  "fig/sir/sir_age_plot.svg",
  sir_age_plot,
  width = 4.5, height = 2, scale = 1.5
)

ggsave(
  "fig/sir/sir_lagged_plot.svg",
  sir_lagged_plot,
  width = 4.5, height = 2, scale = 1.5
)



# Mini forecast (doesn't work but can get you started) ####

# Template table for forecasts
d_template <- d_sample %>%
  # Extend years
  group_by(country, iso3Code, sex, cancer, icd, age, age_grp) %>%
  tidyr::complete(year = min(year):2050) %>%
  ungroup() %>%
  dplyr::select(-py,-cases,-py_never,-py_current) %>%
  # Extend SIR reference rates
  mutate(
    rate_never = unique(rate_never[!is.na(rate_never)]),
    rate_current = unique(rate_current[!is.na(rate_current)]),
    .by = c(sex, age, age_grp)
  ) %>%
  # Add label
  mutate(
    forecast = case_when(
      between(year,min(year),max(year)) ~ "Observed",
      .default = "Forecast"
    ),
    .by = c(country, sex)
  )





