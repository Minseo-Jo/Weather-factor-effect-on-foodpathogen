library('arm')

result3 <- bayesglm(bacteria_foodpathogen_toxicoinfection~LT+aWV+MR
                   , data = AWS_bacteria_toxicoinfection_training
                   , family = binomial())
summary(result3)
