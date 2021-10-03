# Original Code
# Copyright (c) 2020, kini
# https://kuduz.tistory.com/1202

AWS_bacteria_toxicoinfection <- read.csv('AWS_2018_bacteria_toxicoinfection.csv')

AWS_bacteria_toxcoinfection <- na.omit(AWS_bacteria_toxicoinfection)

AWS_bacteria_toxicoinfection <- AWS_bacteria_toxicoinfection[,-1]

colnames(AWS_bacteria_toxicoinfection)

AWS_bacteria_toxicoinfection%>%
  initial_split(prop = 0.7) -> AWS_bacteria_toxicoinfection_split


train_AWS_bacteria_toxicoinfection <- AWS_bacteria_toxicoinfection_split%>%
  training()

test_AWS_bacteria_toxicoinfection <- AWS_bacteria_toxicoinfection_split%>%
  testing()



AWS_bacteria_toxicoinfection_split%>% training()%>%
  recipe(bacteria_foodpathogen_toxicoinfection~ 
           aT+aHT+aLT+HT+LT+aWV+MR)


AWS_bacteria_toxicoinfection_split%>% training()%>%
  recipe(bacteria_foodpathogen_toxicoinfection~ 
           aT+aH+aLT+HT+LT+aWV+MR)%>% #aH -> aHT
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> AWS_bacteria_toxicoinfection_recipe


AWS_bacteria_toxicoinfection_recipe



AWS_bacteria_toxicoinfection_recipe%>%
  bake(AWS_bacteria_toxicoinfection_split%>%testing())-> AWS_bacteria_toxicoinfection_testing


AWS_bacteria_toxicoinfection_testing


AWS_bacteria_toxicoinfection_recipe%>%
  juice() -> AWS_bacteria_toxicoinfection_training

AWS_bacteria_toxicoinfection_training



AWS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection <- as.character(AWS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection)
AWS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection <- as.factor(AWS_bacteria_toxicoinfection_training$bacteria_foodpathogen_toxicoinfection)



rand_forest(trees=100, mode='classification') %>% 
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_toxicoinfection~LT+aWV+MR
      , data = AWS_bacteria_toxicoinfection_training)->bacteria_toxicoinfection_rf2


bacteria_toxicoinfection_rf2

measure_importance(bacteria_toxicoinfection_rf2$fit)

measure_importance(bacteria_toxicoinfection_rf2$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)


# # A tibble: 3 x 2
# variable   imp
# <fct>    <dbl>
#   1 LT       100  
# 2 MR        81.6
# 3 aWV       33.2

