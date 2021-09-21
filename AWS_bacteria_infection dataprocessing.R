AWS_bacteria_infection <- read.csv('AWS_2018_bacteria_infection.csv')

AWS_bacteria_infection <- na.omit(AWS_bacteria_infection)

AWS_bacteria_infection <- AWS_bacteria_infection[,-1]

colnames(AWS_bacteria_infection)

AWS_bacteria_infection%>%
  initial_split(prop = 0.7) -> AWS_bacteria_infection_split


train_AWS_bacteria_infection <- AWS_bacteria_infection_split%>%
  training()

test_AWS_bacteria_infection <- AWS_bacteria_infection_split%>%
  testing()



AWS_bacteria_infection_split%>% training()%>%
  recipe(bacteria_foodpathogen_infection~ 
           aT+aHT+aLT+HT+LT+aWV+MR)


AWS_bacteria_infection_split%>% training()%>%
  recipe(bacteria_foodpathogen_infection~ 
           aT+aHT+aLT+HT+LT+aWV+MR)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> AWS_bacteria_infection_recipe


AWS_bacteria_infection_recipe



AWS_bacteria_infection_recipe%>%
  bake(AWS_bacteria_infection_split%>%testing())-> AWS_bacteria_infection_testing


AWS_bacteria_infection_testing


AWS_bacteria_infection_recipe%>%
  juice() -> AWS_bacteria_infection_training

AWS_bacteria_infection_training



AWS_bacteria_infection_training$bacteria_foodpathogen_infection <- as.character(AWS_bacteria_infection_training$bacteria_foodpathogen_infection)
AWS_bacteria_infection_training$bacteria_foodpathogen_infection <- as.factor(AWS_bacteria_infection_training$bacteria_foodpathogen_infection)



rand_forest(trees=100, mode='classification') %>% #y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_infection~aLT+aWV+MR
      , data = AWS_bacteria_infection_training)->bacteria_infection_rf2


bacteria_infection_rf2

measure_importance(bacteria_infection_rf2$fit)

measure_importance(bacteria_infection_rf2$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)


# A tibble: 3 x 2
# variable   imp
# <fct>    <dbl>
#   1 MR       100  
# 2 aLT       84.9
# 3 aWV       41.8)



