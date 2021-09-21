AWS_bacteria <- read.csv("AWS_2018_bacteria.csv", header = T)

head(AWS_bacteria)

colnames(AWS_bacteria)

AWS_bacteria <- na.omit(AWS_bacteria)

AWS_bacteria

AWS_bacteria2 = AWS_bacteria[,-1]
AWS_bacteria2 = AWS_bacteria2[,-1]
AWS_bacteria2


cor_AWS <- cor(AWS_bacteria2)
cor_AWS



library('tidyverse')
library('tidymodels')



AWS_bacteria2 %>%
  initial_split(prop = 0.7) -> AWS_split


train2 <- AWS_split %>%
  training()

test2 <- AWS_split %>%
  testing()


library(randomForest)
AWS_split %>% training()%>%
  recipe(bacteria_foodpathogen~aT..C.+aHT..C.+aLT..C.+HT..C.
  +LT..C.+aWV.m.s.+MR.mm.)
# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome   1
# predictor 7



AWS_split %>% training()%>%
  recipe(bacteria_foodpathogen~aT..C.+aHT..C.+aLT..C.+HT..C.
         +LT..C.+aWV.m.s.+MR.mm.)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> bacteria_AWS_recipe

bacteria_AWS_recipe


# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome          1
# predictor          7
# 
# Training data contained 234 data points and no missing data.
# 
# Operations:
#   
#   Correlation filter removed aHT..C., HT..C., LT..C., aT..C. [trained]
# Centering for aLT..C., aWV.m.s., MR.mm. [trained]
# Scaling for aLT..C., aWV.m.s., MR.mm. [trained]


bacteria_AWS_recipe %>%
  bake(AWS_split %>% testing()) -> bacteria_AWS_testing


bacteria_AWS_testing

#  aLT..C. aWV.m.s. MR.mm. bacteria_foodpathogen
#   <dbl>    <dbl>  <dbl>                 <int>
# 1 -1.70      3.56  -1.13                    0
# 2 -0.780     2.42  -0.529                   0
# 3  0.425     2.61  -0.384                   0
# 4 -0.650     2.23  -0.196                   0
# 5  0.462    -0.229  1.54                    0
# 6  0.0725   -0.607  0.287                   0
# 7 -0.372    -0.986 -0.371                   0
# 8  1.35     -0.797  1.11                    1
# 9 -0.520    -0.986 -0.359                   0
# 10 -1.57     -1.18  -1.27                   0
# # ... with 91 more rows

bacteria_AWS_recipe %>%
  juice() -> bacteria_AWS_training

bacteria_AWS_training

# # A tibble: 234 x 4
# aLT..C. aWV.m.s. MR.mm. bacteria_foodpathogen
# <dbl>    <dbl>  <dbl>                 <int>
# 1  -1.61    1.47   -1.27                0
# 2   1.46    1.66    0.764               0
# 3  -0.641  -0.797  -0.535               0
# 4  -1.23    1.85   -1.14                0
# 5   0.869  -0.797  -0.637               0
# 6  -0.669  -1.55   -0.353               0
# 7  -0.141   0.717  -0.438               0
# 8   1.52    0.717   0.668               0
# 9  -1.43   -0.0396 -1.27                0
# 10   0.462   0.907   1.18               0
# # ... with 224 more rows




bacteria_AWS_training$bacteria_foodpathogen <- as.character(bacteria_AWS_training$bacteria_foodpathogen)
bacteria_AWS_training$bacteria_foodpathogen <- as.factor(bacteria_AWS_training$bacteria_foodpathogen)



rand_forest(trees=100, mode='classification') %>% #y가 범주형일때는 분류형 random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(bacteria_foodpathogen~aLT..C.+aWV.m.s.+MR.mm.
      ,data = bacteria_AWS_training) -> bacteria_AWS_rf


bacteria_AWS_rf



#변수별 중요도
library('randomForestExplainer')  
measure_importance(bacteria_AWS_rf$fit)



measure_importance(bacteria_AWS_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)

# # A tibble: 3 x 2
# variable   imp
# <fct>    <dbl>
# 1 MR.mm.  100  
# 2 aLT..C. 98.9
# 3 aWV.m.s.47.2
