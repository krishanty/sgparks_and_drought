# ==============================================================================
# BL5233 Group Project: Complete LMM & Drought Resilience Analysis Script
# Project Title: NDVI Response to Drought Stress Across Singapore Park Types
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load Required R Libraries
# ------------------------------------------------------------------------------
library(lme4)        # Linear Mixed-Effects Models (lmer)
library(lattice)     # Variance for levels of random effect
library(ggplot2)     # Presentation-ready visualizations
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 2. Data Ingestion, Inspection & Standardization
# ------------------------------------------------------------------------------
# Load dataset (adjust file path as necessary)
urban_parks <- c("ADMIRALTY PK", "BUKIT BATOK NATURE PARK", "BUKIT BATOK TOWN PARK",
                 "MOUNT FABER PARK", "TAMPINES ECO GREEN PK")
buffer_parks <- c("CHESTNUT NATURE PARK", "RIFLE RANGE NATURE PARK", "WINDSOR NATURE PARK",
                  "THOMSON NATURE PARK", "DAIRY FARM NATURE PARK")

data <- read.csv("data/csv/parks_all_years.csv")

data <- data %>% filter(stats %in% c("mean_ndvi", "mean_ndmi", "mean_monthly_mean_temp", "cumulative_monthly_precip")) %>%
  mutate(type = if_else(name %in% urban_parks, 1, 0)) %>%                # Add column for type information
  reshape(idvar = c("name", "year", "month", "type"), timevar = "stats", # Reformatting for fitting
                    v.names = "value", direction = "wide") %>%
  rename(park = name, NDVI = value.mean_ndvi, NDMI = value.mean_ndmi,
         temp = value.mean_monthly_mean_temp, precip = value.cumulative_monthly_precip) %>%
  select(park, type, year, month, NDVI, NDMI, temp, precip) %>%
  filter(!is.na(NDMI), !is.na(precip), !is.na(temp),                     # Remove entries with NAs
         !is.na(type), !is.na(month), !is.na(park))                      # 720 input, 638 output

# Ensure factor data types
data$park <- as.factor(data$park)
data$type <- as.factor(data$type)
data$month <- as.factor(data$month)

# Scaling
data$precip_sc <- as.numeric(scale(data$precip))
data$temp_sc <- as.numeric(scale(data$temp))
par(mfrow = c(2,2))
ggplot(data=data, aes(x=temp_sc,y=NDVI))+ geom_point()
ggplot(data=data, aes(x=precip_sc,y=NDVI))+ geom_point()
ggplot(data=data, aes(x=temp_sc,y=NDMI))+ geom_point()
ggplot(data=data, aes(x=precip_sc,y=NDMI))+ geom_point()
# Feel like there's no clear pattern for this

# ------------------------------------------------------------------------------
# 3. Model fitting and checks
# ------------------------------------------------------------------------------
# Fitting
mod1 <- lmer(NDVI ~ precip_sc*temp_sc*type + (1 | month) + (1 | park), data = data)
mod1_step <- step(mod1) # All exp variables removed

# Manual NDVI
mod1.1 <- update(mod1,~.-precip_sc:temp_sc:type)
summary(mod1.1)
mod1.2 <- update(mod1.1,~.-precip_sc:type)
summary(mod1.2)
mod1.3 <- update(mod1.2,~.-temp_sc:type)
summary(mod1.3)
mod1.4 <- update(mod1.3,~.-temp_sc:precip_sc)
summary(mod1.4)
mod1.5 <- update(mod1.4,~.-precip_sc)
summary(mod1.5)
mod1.6 <- update(mod1.5,~.-temp_sc)
summary(mod1.6) # Only type significant

mod2 <- lmer(NDMI ~ precip_sc*temp_sc*type + (1 | month) + (1 | park), data = data)
mod2_step <- step(mod2) # All exp variables removed

# Manual NDMI
mod2.1 <- update(mod2,~.-precip_sc:temp_sc:type)
summary(mod2.1)
mod2.2 <- update(mod2.1,~.-precip_sc:temp_sc)
summary(mod2.2)
mod2.3 <- update(mod2.2,~.-temp_sc:type)
summary(mod2.3)
mod2.4 <- update(mod2.3,~.-type:precip_sc)
summary(mod2.4)
mod2.5 <- update(mod2.4,~.-temp_sc)
summary(mod2.5)
mod2.6 <- update(mod2.5,~.-type)
summary(mod2.6) # Only precip significant

# Checks
#Variance for levels of random effect
plot(mod1.6,resid(.,scaled=T)~fitted(.)
     |month,abline=0,lty=2)

plot(mod1.6,resid(.,scaled=T)~fitted(.)
     |park,abline=0,lty=2)

#Normality for whole dataset
qqmath(mod1.6,lty=2)

#Normality for levels of random effect
qqmath(~ resid(mod1.6, scaled = T) | park,
  data = data, abline = c(0, 1))
qqmath(~ resid(mod1.6, scaled = T) | month,
  data = data, abline = c(0, 1))[3]

