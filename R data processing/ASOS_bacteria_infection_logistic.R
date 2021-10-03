#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result_ASOS_bacteria_infection<- bayesglm(bacteria_foodpathogen_infection~
                                            lowest_sealevel_pressure+
                                            month_rainfall+
                                            avarage_wind_speed+
                                            highest_wind_speed,
                                          data = ASOS_bacteria_infection_training,
                                          family = binomial())
summary(result_ASOS_bacteria_infection)
