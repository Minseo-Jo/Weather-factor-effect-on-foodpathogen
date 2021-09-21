result_ASOS_bacteria_toxicoinfection <- bayesglm(bacteria_foodpathogen_toxicoinfection~lowest_sealevel_pressure+
                                                lowest_relative_humidity+month_rainfall+
                                                solar_radiation_quantity+highest_wind_speed
                                              ,data = ASOS_bacteria_toxicoinfection_training
                                              , family = binomial())
summary(result_ASOS_bacteria_toxicoinfection)
