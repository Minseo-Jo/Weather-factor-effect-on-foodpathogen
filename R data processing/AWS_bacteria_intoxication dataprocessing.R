AWS_bacteria_intoxication <- read.csv('AWS_2018_bacteria_intoxication.csv')

AWS_bacteria_intoxication<- na.omit(AWS_bacteria_intoxication)

AWS_bacteria_intoxication <- AWS_bacteria_intoxication[,-1]

colnames(AWS_bacteria_intoxication)

AWS_bacteria_intoxication%>%
  initial_split(prop = 0.7) -> AWS_bacteria_intoxication_split


train_AWS_bacteria_intoxication <- AWS_bacteria_intoxication_split%>%
  training()

test_AWS_bacteria_intoxication <- AWS_bacteria_intoxication_split%>%
  testing()



AWS_bacteria_intoxication_split%>% training()%>%
  recipe(bacteria_foodpathogen_intoxication~ 
           aT+aHT+aLT+HT+LT+aWV+MR)


AWS_bacteria_intoxication_split%>% training()%>%
  recipe(bacteria_foodpathogen_intoxication~ 
           aT+aHT+aLT+HT+LT+aWV+MR)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> AWS_bacteria_intoxication_recipe


AWS_bacteria_intoxication_recipe



AWS_bacteria_intoxication_recipe%>%
  bake(AWS_bacteria_intoxication_split%>%testing())-> AWS_bacteria_intoxication_testing


AWS_bacteria_intoxication_testing


AWS_bacteria_intoxication_recipe%>%
  juice() -> AWS_bacteria_intoxication_training

AWS_bacteria_intoxication_training



AWS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication <- as.character(AWS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication)
AWS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication <- as.factor(AWS_bacteria_intoxication_training$bacteria_foodpathogen_intoxication)



rand_forest(trees=100, mode='classification') %>%#y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen_intoxication~aLT+aWV+MR
      , data = AWS_bacteria_intoxication_training)->bacteria_intoxication_rf2


bacteria_intoxication_rf2

measure_importance(bacteria_intoxication_rf2$fit)

measure_importance(bacteria_intoxication_rf2$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)

# # A tibble: 3 x 2
# variable   imp
# <fct>    <dbl>
# 1 MR       100  
# 2 aLT       88.0
# 3 aWV       38.0

