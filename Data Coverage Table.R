library(tidyverse)

id_dict <- read_csv(
  "/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/overtime_data/overtime_id_dict.csv",
  show_col_types = FALSE)

names(id_dict)
glimpse(id_dict)
head(id_dict, 20)

id_country <- id_dict %>%
mutate(
country_code = as.integer(country_code),
id_code = as.integer(id_code)) %>%
filter(id_code == country_code)

View(id_country)

coverage_table <- read_csv("/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/lung_cancer_temporal_coverage_table.csv",
show_col_types = FALSE)

install.packages("countrycode")
library(countrycode)
coverage_table <- coverage_table %>%
mutate(country_code =countrycode(iso3Code, origin = "iso3c",destination = "iso3n"))
coverage_table %>%
select(country,iso3Code,country_code) %>%
head(20)

coverage_with_registry <- coverage_table %>%
left_join(id_country %>%
select(country_code,id_label,incidence,inc_cov,inc_period,national,mortality,mort_cov,mort_period,inc_source),
by = "country_code")

View(coverage_with_registry)

meeting_coverage <- coverage_with_registry %>%
mutate(incidence_period =case_when(incidence_usable_years > 0 ~
paste0(incidence_usable_start,"–",incidence_usable_end),
incidence_usable_years == 0 ~"No valid ASR", TRUE ~"Not available"),
mortality_period =case_when(mortality_usable_years > 0 ~paste0(mortality_usable_start,"–",mortality_usable_end),
mortality_usable_years == 0 ~"No valid ASR",TRUE ~"Not available"),
    
incidence_temporal_quality =case_when(
incidence_longest_continuous_run >= 20 ~"Good",
incidence_longest_continuous_run >= 10 ~"Moderate",
incidence_longest_continuous_run > 0 ~"Limited",
incidence_longest_continuous_run == 0 ~"No usable ASR",
TRUE ~"Not available"),
    
mortality_temporal_quality =case_when(
mortality_longest_continuous_run >= 20 ~"Good",
mortality_longest_continuous_run >= 10 ~"Moderate",
mortality_longest_continuous_run > 0 ~"Limited",
mortality_longest_continuous_run == 0 ~"No usable ASR",
TRUE ~"Not available"))

meeting_coverage_table <- meeting_coverage %>%
select(country,iso3Code,
incidence_period,incidence_usable_years,incidence_longest_continuous_run,incidence_temporal_quality,
inc_cov,national,inc_source,
mortality_period,mortality_usable_years,mortality_longest_continuous_run,mortality_temporal_quality,
mort_cov
) %>%
  
arrange(country)

View(meeting_coverage_table)

write_csv(meeting_coverage_table,"~/Desktop/lung_cancer_data_coverage_table_FINAL.csv")