# Trend Plots #
library(tidyverse)

asr <- read_csv(
  "/Users/yichieh0112/Desktop/LMU/實習Internship/WHO Lyon/Dataset/asr_results.csv",
  show_col_types = FALSE)
glimpse(asr)
summary(asr$ASR)

asr %>%
  summarise(
    total_rows = n(),
    valid_ASR = sum(!is.na(ASR)),
    missing_ASR = sum(is.na(ASR)))

#Testing: Australia#
asr %>%
  
  filter(
  country == "Australia",
  type == "incidence") %>%
  
  ggplot(
  aes(x = year, y = ASR, color = sex, group = sex)
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Lung Cancer Incidence in Australia",
    x = "Year",
    y = "ASR per 100,000",
    color = "Sex"
  ) +
  theme_bw()

#Plot data#
plot_data <- asr %>%
mutate(
year = as.integer(year),
ASR = as.numeric(ASR),
sex = factor(sex,
levels = c("Male", "Female")),
type = factor(type,
levels = c("incidence", "mortality"))
) %>%
  
filter(
!is.na(country), !is.na(year)
) %>%

arrange(country, type, sex, year) %>%
group_by(country, type, sex) %>%
complete(year = seq(
min(year, na.rm = TRUE),
max(year, na.rm = TRUE),
by = 1)
) %>%

ungroup()

incidence_data <- plot_data %>%
filter(type == "incidence")
incidence_data %>%
summarise(
n_rows = n(),
n_valid_ASR =
sum(!is.na(ASR)),
min_ASR =
min(ASR, na.rm = TRUE),
max_ASR =
max(ASR, na.rm = TRUE),
n_countries =
n_distinct(country)
)

# See how many pages#
install.packages("ggforce")
library(ggforce)

nrow_page <- 3
ncol_page <- 4
countries_per_page <- nrow_page * ncol_page
n_countries <- n_distinct(incidence_data$country)

n_pages <- ceiling(
n_countries / countries_per_page)

n_pages

# Testing: Page 1#
incidence_plot_page1 <- ggplot(
incidence_data,
aes(
x = year,
y = ASR,
color = sex,
linetype = sex,
group = sex)
) +
geom_line(
linewidth = 0.7,
na.rm = FALSE
) +
geom_point(
size = 1.2,
na.rm = TRUE
) +
ggforce::facet_wrap_paginate(
~ country,
nrow = 3,
ncol = 4,
scales = "free",
page = 1
) +
labs(
title ="Lung Cancer Incidence Trends by Country and Sex",
subtitle ="Age-standardized rates using the World Standard Population",
x ="Year",
y ="Age-standardized incidence rate per 100,000",
color ="Sex",
linetype ="Sex") +
theme_bw(base_size = 11) +
theme(legend.position ="top",
strip.text =
element_text(
face = "bold",
size = 10),
axis.text.x =
element_text(
angle = 45,
hjust = 1),
plot.title =
element_text(
face = "bold",
size = 15
),
panel.spacing =unit(1, "lines")
)

incidence_plot_page1

# Incidence_pdf #
pdf("~/Desktop/lung_cancer_incidence_ASR_all_countries.pdf",
width = 14,
height = 9
)

for (i in 1:n_pages) {
p <- ggplot(incidence_data,
aes(
x = year,
y = ASR,
color = sex,
linetype = sex,
group = sex
)) +
geom_line(
linewidth = 0.7,
na.rm = FALSE
) +
geom_point(
size = 1.2,
na.rm = TRUE
) +
ggforce::facet_wrap_paginate(
~ country,
nrow = 3,
ncol = 4,
scales = "free",
page = i) +
labs(
title = "Lung Cancer Incidence Trends by Country and Sex",
subtitle = paste("Age-standardized rates using the World Standard Population | Page",
i,"of",n_pages),
x = "Year",
y = "Age-standardized incidence rate per 100,000",
color = "Sex",
linetype = "Sex"
) +
theme_bw(base_size = 11) +
theme(
legend.position = "top",
strip.text = element_text(
face = "bold",
size = 10),
axis.text.x = element_text(
angle = 45,
hjust = 1
),
plot.title = element_text(
face = "bold",
size = 15),
panel.spacing = unit(1, "lines"))
print(p)
}

dev.off()


# Mortality_pdf #
mortality_data <- plot_data %>%
filter(type == "mortality")

n_countries_mortality <-
n_distinct(mortality_data$country)

n_pages_mortality <-
ceiling(n_countries_mortality / 12)

pdf("~/Desktop/lung_cancer_mortality_ASR_all_countries.pdf",
width = 14,
height = 9)

for (i in 1:n_pages_mortality) {
p <- ggplot(mortality_data,
aes(
x = year,
y = ASR,
color = sex,
linetype = sex,
group = sex)) +
geom_line(
linewidth = 0.7,
na.rm = FALSE) +
geom_point(
size = 1.2,
na.rm = TRUE) +
ggforce::facet_wrap_paginate(
~ country,
nrow = 3,
ncol = 4,
scales = "free",
page = i
) +
labs(
title ="Lung Cancer Mortality Trends by Country and Sex",
subtitle =
paste("Age-standardized rates using the World Standard Population | Page",
i,
"of",
n_pages_mortality),
x ="Year",
y ="Age-standardized mortality rate per 100,000",
color ="Sex",
linetype ="Sex") +
theme_bw(base_size = 11) +
theme(legend.position ="top",
strip.text =element_text(face = "bold",size = 10),
axis.text.x =element_text(angle = 45,hjust = 1),
plot.title =element_text(face = "bold",size = 15),
panel.spacing =unit(1, "lines"))

print(p)
}

dev.off()