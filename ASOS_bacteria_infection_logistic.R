library('arm')

result_ASOS_bacteria_infection<- bayesglm(bacteria_foodpathogen_infection~
                                            lowest_sealevel_pressure+
                                            month_rainfall+
                                            avarage_wind_speed+
                                            highest_wind_speed,
                                          data = ASOS_bacteria_infection_training,
                                          family = binomial())
summary(result_ASOS_bacteria_infection)
