#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

result_ASOS_bacteria_toxicoinfection <- bayesglm(bacteria_foodpathogen_toxicoinfection~lowest_sealevel_pressure+
                                                lowest_relative_humidity+month_rainfall+
                                                solar_radiation_quantity+highest_wind_speed
                                              ,data = ASOS_bacteria_toxicoinfection_training
                                              , family = binomial())
summary(result_ASOS_bacteria_toxicoinfection)
