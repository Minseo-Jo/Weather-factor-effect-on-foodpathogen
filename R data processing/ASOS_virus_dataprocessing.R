# Original Code
# Copyright (c) 2020, kini
# https://kuduz.tistory.com/1202

#바이러스성

ASOS_virus <- read.csv('ASOS_2018_virus.csv')

head(ASOS_virus)

ASOS_virus <- na.omit(ASOS_virus)
 
ASOS_virus2 <- ASOS_virus[,-1]
ASOS_virus2 <- ASOS_virus2[,-1]
ASOS_virus2

colnames(ASOS_virus2)

cor(ASOS_virus2)


library('tidyverse')
library('tidymodels')


ASOS_virus2 %>%
  initial_split(prop = 0.7) -> ASOS_virus_split


train_ASOS_virus <- ASOS_virus_split %>%
  training()

test_ASOS_virus <- ASOS_virus_split %>%
  testing()


library(randomForest)

ASOS_virus_split%>% training()%>%
  recipe(virus_foodpathogen~ aT+aHT+aLT+HT+LT+aAP+aSP+HSP+LSP+
           aWVP+HWVP+LWVP+aDT+aRT+LRT+MR+DR+aWS+HWS+SRQ+aGT)
# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome       1
# predictor     21


ASOS_virus_split%>% training()%>%
  recipe(virus_foodpathogen~ aT+aHT+aLT+HT+LT+aAP+aSP+HSP+LSP+
           aWVP+HWVP+LWVP+aDT+aRT+LRT+MR+DR+aWS+HWS+SRQ+aGT)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep() -> virus_ASOS_recipe



virus_ASOS_recipe
# Data Recipe
# 
# Inputs:
#   
#   role #variables
# outcome     1
# predictor   21
# 
# Training data contained 8 data points and no missing data.
# Operations:
#   
# Correlation filter removed aLT, LT, aSP, aWVP, HWVP, LWVP, aDT, ... [trained]
# Centering for LSP, aRT, LRT, DR, aWS, HWS, SRQ [trained]
#Scaling for LSP, aRT, LRT, DR, aWS, HWS, SRQ [trained]

virus_ASOS_recipe %>%
  bake(ASOS_virus_split%>% testing()) ->virus_ASOS_testing


virus_ASOS_testing
# # A tibble: 4 x 8
# LSP    aRT    LRT     DR     aWS    HWS    SRQ    virus_foodpathogen
# <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>         <int>
# 1 0.285  0.615  0.531  1.01  -0.490   1.11   0.780              1
# 2 0.321  0.123 -0.318  0.168 -1.36   -0.285 -0.302              1
# 3 0.970  0     -0.885  0.429 -1.80   -0.654 -1.25               1
# 4 2.55  -1.48  -0.318 -1.25  -0.0545 -0.433 -1.33               



virus_ASOS_recipe%>%
  juice()-> virus_ASOS_training

virus_ASOS_training
# A tibble: 8 x 8
# LSP    aRT    LRT     DR    aWS    HWS      SRQ virus_foodpathogen
# <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>              <int>
# 1  0.549 -1.60  -0.743 -0.770  1.69  -0.138 -0.947                  1
# 2 -1.59   0.862  0.955  1.43  -0.490  0.451  0.478                    0
# 3 -0.382  0.615  0.106  0.660 -0.490 -0.580  1.06                     0
# 4 -0.698  1.23   1.52   1.03  -1.36  -1.68   0.779                    0
# 5  1.46  -1.23  -0.318 -1.44   0.381  0.525 -1.98                     1
# 6 -0.681  0.369  0.672 -0.478 -0.926 -0.580  0.161                    1
# 7  0.566  0.123 -0.743 -0.708  0.381  0.304 -0.00720                  1
# 8  0.777 -0.369 -1.45   0.276  0.817  1.70   0.450                    1



virus_ASOS_training$virus_foodpathogen <- as.character(virus_ASOS_training$virus_foodpathogen)
virus_ASOS_training$virus_foodpathogen <- as.factor(virus_ASOS_training$virus_foodpathogen)



rand_forest(trees=100, mode='classification') %>% #y??? ?????????????????? ????????? random forest
  set_engine('randomForest', localImp = TRUE) %>%
  fit(virus_foodpathogen~LSP+aRT+LRT+DR+aWS+HWS+SRQ
      ,data = virus_ASOS_training) ->virus_ASOS_rf


virus_ASOS_rf



library('randomForestExplainer')  
measure_importance(virus_ASOS_rf$fit)



measure_importance(virus_ASOS_rf$fit)%>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)

# # A tibble: 7 x 2
# variable    imp
# <fct>     <dbl>
# 1 aRT      100   
# 2 SRQ       72.9 
# 3 DR        50.0 
# 4 LSP       37.5 
# 5 LRT       30.4 
# 6 aWS       29.3 
# 7 HWS       9.30 ->HWS?????? 
