#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result3 <- bayesglm(bacteria_foodpathogen_toxicoinfection~LT+aWV+MR
                   , data = AWS_bacteria_toxicoinfection_training
                   , family = binomial())
summary(result3)
