library('arm')

result2 <- bayesglm(bacteria_foodpathogen_intoxication~aLT+aWV+MR
                   , data = AWS_bacteria_intoxication_training
                   , family = binomial())
summary(result2)
