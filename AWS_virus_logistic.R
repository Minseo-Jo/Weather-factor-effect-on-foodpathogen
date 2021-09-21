library('arm')

result_AWS_virus <-bayesglm(virus_foodpathogen~LT+MR+aWV,
                            data = virus_AWS_training, family=binomial())
summary(result_AWS_virus)
