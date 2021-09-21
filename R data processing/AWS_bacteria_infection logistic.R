library('arm')

result <- bayesglm(bacteria_foodpathogen_infection~aLT+aWV+MR
                   , data = AWS_bacteria_infection_training
                   , family = binomial())
summary(result)
