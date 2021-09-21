#세균성 - 독소형

ASOS_bacteria_intoxication <- read.csv('ASOS_bacteria_intoxication.csv')


ASOS_bacteria_intoxication <- na.omit(ASOS_bacteria_intoxication)

ASOS_bacteria_intoxication <- ASOS_bacteria_intoxication[,-1]
ASOS_bacteria_intoxication <- ASOS_bacteria_intoxication[,-1]

colnames(ASOS_bacteria_intoxication)


ASOS_bacteria_intoxication %>%
  initial_split(prop = 0.7) -> ASOS_bacteria_intoxication_split



train_ASOS_bacteria_intoxication <- ASOS_bacteria_intoxication_split%>%
  training()



test_ASOS_bacteria_intoxication <- ASOS_bacteria_intoxication_split%>%
  testing()




ASOS_bacteria_intoxication_split%>% training()%>%
  recipe(bacteria_foodpathogen_intoxication~ 
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





ASOS_bacteria_intoxication_split%>% training()%>%
  recipe(bacteria_foodpathogen_intoxication~ 
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
  prep() -> ASOS_bacteria_intoxication_recipe


ASOS_bacteria_intoxication_recipe


#Data Recipe

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
#   Correlation filter removed 15 items [trained]
# Centering for 6 items [trained]
# Scaling for 6 items [trained]




ASOS_bacteria_intoxication_recipe%>%
  bake(ASOS_bacteria_intoxication_split%>%testing())-> ASOS_bacteria_intoxication_testing


ASOS_bacteria_intoxication_testing


ASOS_bacteria_intoxication_recipe%>%
  juice() -> ASOS_bacteria_intoxication_training

ASOS_bacteria_infection_training  




ASOS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication <- as.character(ASOS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication)
ASOS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication <- as.factor(ASOS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication)



rand_forest(trees=100, mode='classification') %>% #y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_intoxication~lowest_sealevel_pressure+
        lowest_relative_humidity+day_rainfall+avarage_wind_speed+
        highest_wind_speed+solar_radiation_quantity
      ,data = ASOS_bacteria_intoxication_training)->bacteria_intoxication_rf


bacteria_intoxication_rf



measure_importance(bacteria_intoxication_rf$fit)

measure_importance(bacteria_intoxication_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)




# # A tibble: 6 x 2
# variable                   imp
# <fct>                    <dbl>
# 1 day_rainfall             100  
# 2 lowest_relative_humidity  93.6
# 3 lowest_sealevel_pressure  68.9
# 4 solar_radiation_quantity  68.7
# 5 avarage_wind_speed        14.8-> 뺴기
# 6 highest_wind_speed        12.8-> 빼기 
