library('arm')

result_ASOS_virus <- bayesglm(virus_foodpathogen~
                                LSP+aRT+LRT+DR+aWS+SRQ
                              ,data = virus_ASOS_training, family = binomial())

summary(result_ASOS_virus)
