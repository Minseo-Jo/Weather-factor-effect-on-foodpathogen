#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result <- bayesglm(bacteria_foodpathogen_infection~aLT+aWV+MR
                   , data = AWS_bacteria_infection_training
                   , family = binomial())
summary(result)
