# Original Code
# Copyright (c) 2020, kini
# https://kuduz.tistory.com/1202

ASOS_bacteria_infection <- read.csv('ASOS_bacteria_infection.csv')

head(ASOS_bacteria_infection)

ASOS_bacteria_infection <- na.omit(ASOS_bacteria_infection)


ASOS_bacteria_infection <- ASOS_bacteria_infection[,-1]
ASOS_bacteria_infection <- ASOS_bacteria_infection[,-1]
colnames(ASOS_bacteria_infection) #º¯¼öÈ®ÀÎ

library('tidyverse')
library('tidymodels')

ASOS_bacteria_infection %>%
  initial_split(prop = 0.7) -> ASOS_bacteria_infection_split


train_ASOS_bacteria_infection <- ASOS_bacteria_infection_split%>%
  training()

test_ASOS_bacteria_infection <- ASOS_bacteria_infection_split%>%
  testing()

library(randomForest)

ASOS_bacteria_infection_split%>% training()%>%
  recipe(bacteria_foodpathogen_infection~ 
           average_temperature+avarage_highest_temperature+
         avarage_lowest_temperature+highest_temperature+
           lowest_temperature+avarage_atmospheric_pressure+
           avarage_sealevel_pressure+highest_sealevel_pressure
         +lowest_sealevel_pressure+avarge_water_vapor_pressure
         +highest_water_vapor_pressure+lowest_water_vapor_pressure+
           dew_point_temperature+avarage_relative_humidity+
           lowest_relative_humidity+month_rainfall+day_rainfall+
           avarage_wind_speed+highest_wind_speed+solar_radiation_quantity
         +avarage_ground_temperature)


# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome          1
# predictor         21


ASOS_bacteria_infection_split%>% training()%>%
  recipe(bacteria_foodpathogen_infection~ 
           average_temperature+avarage_highest_temperature+
           avarage_lowest_temperature+highest_temperature+
           lowest_temperature+avarage_atmospheric_pressure+
           avarage_sealevel_pressure+highest_sealevel_pressure
         +lowest_sealevel_pressure+avarge_water_vapor_pressure
         +highest_water_vapor_pressure+lowest_water_vapor_pressure+
           dew_point_temperature+avarage_relative_humidity+
           lowest_relative_humidity+month_rainfall+day_rainfall+
           avarage_wind_speed+highest_wind_speed+solar_radiation_quantity
         +avarage_ground_temperature)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> ASOS_bacteria_infection_recipe

ASOS_bacteria_infection_recipe
# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome          1
# predictor         21
# 
# Training data contained 8 data points and no missing data.
# 
# Operations:
#   
#   Correlation filter removed 16 items [trained]
# Centering for 5 items [trained]
# Scaling for 5 items [trained]


ASOS_bacteria_infection_recipe%>%
  bake(ASOS_bacteria_infection_split%>%testing())-> ASOS_bacteria_infection_testing


ASOS_bacteria_infection_testing


ASOS_bacteria_infection_recipe%>%
  juice() -> ASOS_bacteria_infection_training

ASOS_bacteria_infection_training  



ASOS_bacteria_infection_training$bacteria_foodpathogen_infection <- as.character(ASOS_bacteria_infection_training$bacteria_foodpathogen_infection)
ASOS_bacteria_infection_training$bacteria_foodpathogen_infection <- as.factor(ASOS_bacteria_infection_training$bacteria_foodpathogen_infection)



rand_forest(trees=100, mode='classification') %>% 
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_infection~lowest_sealevel_pressure+
        lowest_relative_humidity+month_rainfall+avarage_wind_speed+
        highest_wind_speed
        , data = ASOS_bacteria_infection_training)->bacteria_infection_rf


bacteria_infection_rf



library('randomForestExplainer') 
measure_importance(bacteria_infection_rf$fit)


measure_importance(bacteria_infection_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)

# # A tibble: 5 x 2
# variable                    imp
# <fct>                     <dbl>
# 1 lowest_sealevel_pressure 100   
# 2 month_rainfall            67.5 
# 3 avarage_wind_speed        13.3 
# 4 highest_wind_speed        10.5 
# 5 lowest_relative_humidity   3.71 ->ë¹¼ê¸° 
