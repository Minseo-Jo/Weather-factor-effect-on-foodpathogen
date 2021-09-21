
AWS_virus <-read.csv('AWS_2018_virus.csv')

AWS_virus <- na.omit(AWS_virus)

AWS_virus2 <- AWS_virus[,-1]
AWS_virus2 <- AWS_virus2[,-1]


colnames(AWS_virus2)

cor(AWS_virus2)
library('tidyverse')
library('tidymodels')

AWS_virus2 %>%
  initial_split(prop=0.7) -> AWS_virus_split


train_AWS_virus<- AWS_virus_split%>%
  training()

test_AWS_virus <- AWS_virus_split %>%
  testing()

library(randomForest)

AWS_virus_split%>% training()%>%
  recipe(virus_foodpathogen~aT+aHT+aLT+HT+LT+aWV+MR)

# Data Recipe
# 
# Inputs:
#   
# role #variables
# outcome          1
# predictor       7

AWS_virus_split%>% training()%>%
  recipe(virus_foodpathogen~aT+aHT+aLT+HT+LT+aWV+MR)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> virus_AWS_recipe

virus_AWS_recipe
# Data Recipe
# 
# Inputs:
#   
# role #variables
# outcome          1
# predictor       7
# 
# Training data contained 234 data points and no missing data.
# 
# Operations:
#   
# Correlation filter removed aHT, HT, aT, aLT [trained]
# Centering for LT, aWV, MR [trained]
# Scaling for LT, aWV, MR [trained]


virus_AWS_recipe%>%
  bake(AWS_virus_split%>% testing())->virus_AWS_testing

virus_AWS_testing


virus_AWS_recipe%>%
  juice()->virus_AWS_training

virus_AWS_training



virus_AWS_training$virus_foodpathogen <- as.character(virus_AWS_training$virus_foodpathogen)
virus_AWS_training$virus_foodpathogen <- as.factor(virus_AWS_training$virus_foodpathogen)


library('randomForestExplainer')

rand_forest(trees=100, mode='classification') %>% #y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(virus_foodpathogen~LT+aWV+MR
      ,data = virus_AWS_training) ->virus_AWS_rf



measure_importance(virus_AWS_rf$fit)

measure_importance(virus_AWS_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)
# # A tibble: 3 x 2
# variable   imp
# <fct>    <dbl>
#   1 LT       100  
# 2 MR        97.4
# 3 aWV       54.2



