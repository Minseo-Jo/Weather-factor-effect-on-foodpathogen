#Original code
#Copyright (c) 2016, TOBIGS
#https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=leedk1110&logNo=220774824473

library('arm')

result_AWS_bacteria <- bayesglm(bacteria_foodpathogen~MR.mm.
                                +aLT..C.+aWV.m.s.
                                , data = bacteria_AWS_training
                                , family = binomial())
summary(result_AWS_bacteria)

# Call:
#   bayesglm(formula = bacteria_foodpathogen ~ MR.mm. + aLT..C. + 
#              aWV.m.s., family = binomial(), data = bacteria_AWS_training)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.8844  -0.4618  -0.2830  -0.1730   2.6046  
# 
# Coefficients:
#               Estimate  Std.Error z value Pr(>|z|)    
# (Intercept)    -2.8963     0.3451  -8.392  < 2e-16 ***
#   MR.mm.       -0.1415     0.3090  -0.458  0.64700    
#   aLT..C.       1.2047     0.3807   3.164  0.00155 ** 
#   aWV.m.s.      0.3041     0.2537   1.199  0.23060    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 131.83  on 233  degrees of freedom
# Residual deviance: 113.62  on 230  degrees of freedom
# AIC: 121.62
# 
# Number of Fisher Scoring iterations: 10
