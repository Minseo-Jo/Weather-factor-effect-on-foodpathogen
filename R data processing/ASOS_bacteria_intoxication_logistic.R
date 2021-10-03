#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

result_ASOS_bacteria_intoxication <- bayesglm(bacteria_foodpathogen_intoxication~lowest_sealevel_pressure+
                                                lowest_relative_humidity+day_rainfall+
                                                solar_radiation_quantity
                                              ,data = ASOS_bacteria_intoxication_training
                                              , family = binomial())
summary(result_ASOS_bacteria_intoxication)
