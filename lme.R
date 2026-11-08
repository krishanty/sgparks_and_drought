# ==============================================================================
# BL5233 Group Project: Data Analysis R script
# Project Title: Investigating the effects of weather conditions on Singapore’s nature parks
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load Required R Libraries
# ------------------------------------------------------------------------------
library(lmerTest)       
library(lattice)     
library(tidyverse)
library(tsibble)
library(DHARMa)
library(emmeans)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Run if script same folder as data

# ------------------------------------------------------------------------------
# 2. Data Ingestion, Inspection & Standardization
# ------------------------------------------------------------------------------
# Load dataset (adjust file path as necessary)
urban_parks <- c("ADMIRALTY PK", "BUKIT BATOK NATURE PARK", "BUKIT BATOK TOWN PARK",
                 "MOUNT FABER PARK", "TAMPINES ECO GREEN PK")
buffer_parks <- c("CHESTNUT NATURE PARK", "RIFLE RANGE NATURE PARK", "WINDSOR NATURE PARK",
                  "THOMSON NATURE PARK", "DAIRY FARM NATURE PARK")

parks <- read_csv("parks_all_years.csv", col_names=TRUE) |> # Change csv here if different name
  mutate(
    year_month = tsibble::make_yearmonth(year = year, month = month),
    time = as.numeric(year_month - min(year_month))
  ) 
wider_data_ndvi <- parks |>
  filter(variable != "ndmi")|>
  mutate(type = if_else(name %in% urban_parks, 1, 0)) %>%                
  pivot_wider(id_cols = c(name, year, month, type, year_month),
              names_from = stats,
              values_from = value) |>
  mutate(across(c(name, type, year, month), \(x)as.factor(x))) |>
  select(name, month, year, year_month, type, mean_ndvi, cumulative_monthly_precip,mean_monthly_max_temp,mean_monthly_mean_temp, n_pixels) |>
  arrange(name, year_month) |>
  group_by(name) |>
  mutate(precip_lag1 = dplyr::lag(cumulative_monthly_precip, 1),
         precip_lag2 = dplyr::lag(cumulative_monthly_precip, 2),
         max_temp_lag1 = dplyr::lag(mean_monthly_max_temp, 1),
         max_temp_lag2 = dplyr::lag(mean_monthly_max_temp, 2),
         mean_temp_lag1 = dplyr::lag(mean_monthly_mean_temp, 1),
         mean_temp_lag2 = dplyr::lag(mean_monthly_mean_temp, 2)) |>
  filter(!is.na(mean_ndvi)) |>
  ungroup() |>
  mutate(across(where(is.numeric) & !mean_ndvi &!n_pixels, \(x) as.numeric(scale(x))))

wider_data_ndmi <- parks |>
  filter(variable != "ndvi")|>
  mutate(type = if_else(name %in% urban_parks, 1, 0)) %>%                
  pivot_wider(id_cols = c(name, year, month, type, year_month),
              names_from = stats,
              values_from = value) |>
  mutate(across(c(name, type, year, month), \(x)as.factor(x))) |>
  select(name, month, year, type, year_month, mean_ndmi, cumulative_monthly_precip,mean_monthly_max_temp,mean_monthly_mean_temp,n_pixels) |>
  arrange(name, year_month) |>
  group_by(name) |>
  mutate(precip_lag1 = dplyr::lag(cumulative_monthly_precip, 1),
         precip_lag2 = dplyr::lag(cumulative_monthly_precip, 2),
         max_temp_lag1 = dplyr::lag(mean_monthly_max_temp, 1),
         max_temp_lag2 = dplyr::lag(mean_monthly_max_temp, 2),
         mean_temp_lag1 = dplyr::lag(mean_monthly_mean_temp, 1),
         mean_temp_lag2 = dplyr::lag(mean_monthly_mean_temp, 2)) |>
  filter(!is.na(mean_ndmi)) |>
  ungroup() |>
  mutate(across(where(is.numeric) & !mean_ndmi &!n_pixels, \(x) as.numeric(scale(x)))) 

ndvi_lme_data <- wider_data_ndvi |>
  drop_na(mean_ndvi, type, cumulative_monthly_precip, mean_monthly_max_temp, mean_monthly_mean_temp, precip_lag1, precip_lag2, max_temp_lag1, max_temp_lag2, mean_temp_lag1, mean_temp_lag2)

# ------------------------------------------------------------------------------
# 3. Model fitting and checks
# ------------------------------------------------------------------------------
## Model fitting ##
# Inital
ndvi_temp_precip <- lmer(mean_ndvi ~ type * (cumulative_monthly_precip + precip_lag1 + precip_lag2 + mean_monthly_max_temp + mean_monthly_mean_temp) +
                           (1|month) + (1|name),
                         data = ndvi_lme_data, REML = FALSE)
final_model_temp_ndvi <- lmerTest::step(ndvi_temp_precip) # Final model is ~ type + precip_lag1 + + (1 | month) + (1 | name) + type:precip_lag1

# Final
final_model_ndvi <- lmer(mean_ndvi ~ type + precip_lag1 + (1 | month) + (1 | name) + type:precip_lag1,
                         data = ndvi_lme_data, REML = FALSE)

# Test significance precipitation slope with emtrends
emtrends(final_model_ndvi, ~ type, var = "precip_lag1")
test(emtrends(final_model_ndvi, ~ type, var = "precip_lag1"))

## Plots for results ##
# 1. Boxplot of NDVI by Park Type
ggplot(ndvi_lme_data, aes(x = type, y = mean_ndvi, fill = type)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Buffer", "Urban")) +
  labs(title = "Mean Monthly NDVI by Park Type" , x = "Park type", y = "Mean NDVI") +
  theme_classic() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# 1. Interaction plot
# Generate prediction data
pred_data <- expand.grid(precip_lag1 = seq(
    min(ndvi_lme_data$precip_lag1),
    max(ndvi_lme_data$precip_lag1),
    length.out = 100), type = levels(ndvi_lme_data$type))

# Predict NDVI using fixed effects only
pred_data$predicted_ndvi <- predict(
  final_model_ndvi,
  newdata = pred_data,
  re.form = NA)

# Plot
ggplot() +
  geom_point(data = ndvi_lme_data,
    aes(x = precip_lag1, y = mean_ndvi, colour = type),
    alpha = 0.2) +
  geom_line(data = pred_data,
    aes(x = precip_lag1, y = predicted_ndvi, colour = type),
    linewidth = 1.2) +
  scale_colour_discrete(labels = c("Buffer", "Urban")) +
  labs(title = "Effect of Rainfall 1-month Lag and Mean Monthly NDVI by Park Type",
       x = "Rainfall 1-month Lag", y = "Mean NDVI", colour = "Park type") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


## Diagnostic plots ##
# Linearity and homoscedasticity of overall data
plot(ndvi_temp_precip, main = "Residuals vs Fitted Values") 

# Normality of overall dataset
qqnorm(resid(ndvi_temp_precip), main = "Normal Q-Q Plot of Residuals")
qqline(resid(ndvi_temp_precip))

# Normality of residuals within levels of random effects
lattice::qqmath(ranef(ndvi_temp_precip))$month                  
lattice::qqmath(ranef(ndvi_temp_precip))$name

# Temporal autocorrelation
acf(resid(ndvi_temp_precip)[order(ndvi_lme_data$name, ndvi_lme_data$year_month)]) 

