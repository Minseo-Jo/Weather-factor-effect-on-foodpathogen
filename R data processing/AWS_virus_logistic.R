#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result_AWS_virus <-bayesglm(virus_foodpathogen~LT+MR+aWV,
                            data = virus_AWS_training, family=binomial())
summary(result_AWS_virus)
