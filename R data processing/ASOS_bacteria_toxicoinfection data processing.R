#세균성 - 독소감염형


ASOS_bacteria_toxicoinfection <- read.csv('ASOS_bacteria_toxicoinfection.csv')


ASOS_bacteria_toxicoinfection <- na.omit(ASOS_bacteria_toxicoinfection)


ASOS_bacteria_toxicoinfection <- ASOS_bacteria_toxicoinfection[,-1]
ASOS_bacteria_toxicoinfection <- ASOS_bacteria_toxicoinfection[,-1]

colnames(ASOS_bacteria_toxicoinfection)

ASOS_bacteria_toxicoinfection %>%
  initial_split(prop = 0.7) -> ASOS_bacteria_toxicoinfection_split




train_ASOS_bacteria_toxicoinfection <- ASOS_bacteria_toxicoinfection_split%>%
  training()

test_ASOS_bacteria_toxicoinfection <- ASOS_bacteria_toxicoinfection_split%>%
  testing()



ASOS_bacteria_toxicoinfection_split%>% training()%>%
  recipe(bacteria_foodpathogen_toxicoinfection~ 
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





ASOS_bacteria_toxicoinfection_split%>% training()%>%
  recipe(bacteria_foodpathogen_toxicoinfection~ 
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
  prep() -> ASOS_bacteria_toxicoinfection_recipe


ASOS_bacteria_toxicoinfection_recipe


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
#   Correlation filter removed 14 items [trained]
# Centering for 7 items [trained]
# Scaling for 7 items [trained]


ASOS_bacteria_toxicoinfection_recipe%>%
  bake(ASOS_bacteria_toxicoinfection_split%>%testing())-> ASOS_bacteria_toxicoinfection_testing


ASOS_bacteria_toxicoinfection_testing


ASOS_bacteria_toxicoinfection_recipe%>%
  juice() -> ASOS_bacteria_toxicoinfection_training

ASOS_bacteria_toxicoinfection_training



ASOS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection <- as.character(ASOS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection)
ASOS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection <- as.factor(ASOS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection)



rand_forest(trees=100, mode='classification') %>% #y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_toxicoinfection~lowest_sealevel_pressure+
        lowest_sealevel_pressure+ lowest_relative_humidity+
        month_rainfall+avarage_wind_speed+
        highest_wind_speed+solar_radiation_quantity
      , data = ASOS_bacteria_toxicoinfection_training)->bacteria_toxicoinfection_rf


bacteria_toxicoinfection_rf




measure_importance(bacteria_toxicoinfection_rf$fit)


measure_importance(bacteria_toxicoinfection_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)

# # A tibble: 6 x 2
# variable                   imp
# <fct>                    <dbl>
# 1 month_rainfall           100  
# 2 solar_radiation_quantity  83.4
# 3 lowest_sealevel_pressure  51.3
# 4 highest_wind_speed        32.5
# 5 lowest_relative_humidity  32.3
# 6 avarage_wind_speed        19.2
