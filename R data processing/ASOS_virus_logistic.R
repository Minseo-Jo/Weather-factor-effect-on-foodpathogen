#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result_ASOS_virus <- bayesglm(virus_foodpathogen~
                                LSP+aRT+LRT+DR+aWS+SRQ
                              ,data = virus_ASOS_training, family = binomial())

summary(result_ASOS_virus)
