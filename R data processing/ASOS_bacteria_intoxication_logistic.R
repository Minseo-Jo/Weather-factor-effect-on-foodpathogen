result_ASOS_bacteria_intoxication <- bayesglm(bacteria_foodpathogen_intoxication~lowest_sealevel_pressure+
                                                lowest_relative_humidity+day_rainfall+
                                                solar_radiation_quantity
                                              ,data = ASOS_bacteria_intoxication_training
                                              , family = binomial())
summary(result_ASOS_bacteria_intoxication)
