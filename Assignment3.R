# Assignment 3
# Regression & Model Evaluation

# Q1 ----

setwd("D:\\temp")

redwine <- read.csv("winequality-red.csv", sep=";")
whitewine <- read.csv("winequality-white.csv", sep=";")

summary(redwine)
# fixed.acidity   volatile.acidity  citric.acid    residual.sugar     chlorides      
# Min.   : 4.60   Min.   :0.1200   Min.   :0.000   Min.   : 0.900   Min.   :0.01200  
# 1st Qu.: 7.10   1st Qu.:0.3900   1st Qu.:0.090   1st Qu.: 1.900   1st Qu.:0.07000  
# Median : 7.90   Median :0.5200   Median :0.260   Median : 2.200   Median :0.07900  
# Mean   : 8.32   Mean   :0.5278   Mean   :0.271   Mean   : 2.539   Mean   :0.08747  
# 3rd Qu.: 9.20   3rd Qu.:0.6400   3rd Qu.:0.420   3rd Qu.: 2.600   3rd Qu.:0.09000  
# Max.   :15.90   Max.   :1.5800   Max.   :1.000   Max.   :15.500   Max.   :0.61100  
# free.sulfur.dioxide total.sulfur.dioxide    density             pH       
# Min.   : 1.00       Min.   :  6.00       Min.   :0.9901   Min.   :2.740  
# 1st Qu.: 7.00       1st Qu.: 22.00       1st Qu.:0.9956   1st Qu.:3.210  
# Median :14.00       Median : 38.00       Median :0.9968   Median :3.310  
# Mean   :15.87       Mean   : 46.47       Mean   :0.9967   Mean   :3.311  
# 3rd Qu.:21.00       3rd Qu.: 62.00       3rd Qu.:0.9978   3rd Qu.:3.400  
# Max.   :72.00       Max.   :289.00       Max.   :1.0037   Max.   :4.010  
#   sulphates         alcohol         quality     
# Min.   :0.3300   Min.   : 8.40   Min.   :3.000  
# 1st Qu.:0.5500   1st Qu.: 9.50   1st Qu.:5.000  
# Median :0.6200   Median :10.20   Median :6.000  
# Mean   :0.6581   Mean   :10.42   Mean   :5.636  
# 3rd Qu.:0.7300   3rd Qu.:11.10   3rd Qu.:6.000  
# Max.   :2.0000   Max.   :14.90   Max.   :8.000 

summary(whitewine)
#  fixed.acidity    volatile.acidity  citric.acid     residual.sugar     chlorides      
# Min.   : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600   Min.   :0.00900  
# 1st Qu.: 6.300   1st Qu.:0.2100   1st Qu.:0.2700   1st Qu.: 1.700   1st Qu.:0.03600  
# Median : 6.800   Median :0.2600   Median :0.3200   Median : 5.200   Median :0.04300  
# Mean   : 6.855   Mean   :0.2782   Mean   :0.3342   Mean   : 6.391   Mean   :0.04577  
# 3rd Qu.: 7.300   3rd Qu.:0.3200   3rd Qu.:0.3900   3rd Qu.: 9.900   3rd Qu.:0.05000  
# Max.   :14.200   Max.   :1.1000   Max.   :1.6600   Max.   :65.800   Max.   :0.34600  
# free.sulfur.dioxide total.sulfur.dioxide    density             pH       
# Min.   :  2.00      Min.   :  9.0        Min.   :0.9871   Min.   :2.720  
# 1st Qu.: 23.00      1st Qu.:108.0        1st Qu.:0.9917   1st Qu.:3.090  
# Median : 34.00      Median :134.0        Median :0.9937   Median :3.180  
# Mean   : 35.31      Mean   :138.4        Mean   :0.9940   Mean   :3.188  
# 3rd Qu.: 46.00      3rd Qu.:167.0        3rd Qu.:0.9961   3rd Qu.:3.280  
# Max.   :289.00      Max.   :440.0        Max.   :1.0390   Max.   :3.820  
#   sulphates         alcohol         quality     
# Min.   :0.2200   Min.   : 8.00   Min.   :3.000  
# 1st Qu.:0.4100   1st Qu.: 9.50   1st Qu.:5.000  
# Median :0.4700   Median :10.40   Median :6.000  
# Mean   :0.4898   Mean   :10.51   Mean   :5.878  
# 3rd Qu.:0.5500   3rd Qu.:11.40   3rd Qu.:6.000  
# Max.   :1.0800   Max.   :14.20   Max.   :9.000  
# Check for each variable how many have missing records
sapply(redwine, function(x) sum(is.na(x)))
# fixed.acidity     volatile.acidity          citric.acid       residual.sugar 
#             0                    0                    0                    0 
#     chlorides  free.sulfur.dioxide total.sulfur.dioxide              density 
#             0                    0                    0                    0 
#            pH            sulphates              alcohol              quality 
#             0                    0                    0                    0 

sapply(whitewine, function(x) sum(is.na(x)))
# fixed.acidity     volatile.acidity          citric.acid       residual.sugar 
#             0                    0                    0                    0 
#     chlorides  free.sulfur.dioxide total.sulfur.dioxide              density 
#             0                    0                    0                    0 
#            pH            sulphates              alcohol              quality 
#             0                    0                    0                    0 


# Check for each variable how many unique values there are
sapply(redwine, function(x) length(unique(x)))
# fixed.acidity     volatile.acidity          citric.acid       residual.sugar 
#            96                  143                   80                   91 
#     chlorides  free.sulfur.dioxide total.sulfur.dioxide              density 
#           153                   60                  144                  436 
#            pH            sulphates              alcohol              quality 
#            89                   96                   65                    6 

sapply(whitewine, function(x) length(unique(x)))
# fixed.acidity     volatile.acidity          citric.acid       residual.sugar 
#            68                  125                   87                  310 
#     chlorides  free.sulfur.dioxide total.sulfur.dioxide              density 
#           160                  132                  251                  890 
#            pH            sulphates              alcohol              quality 
#           103                   79                  103                    7 



# Q2 ----

mean(redwine$quality)
# [1] 5.636023

mean(whitewine$quality)
# [1] 5.877909

# Ans:
# Average quality for red wine: 5.64
# Average quality for white wine: 5.88



# Q3 ----

# The sample are quite random, so we can take the top 80% as training data,
# and the rest as testing data.

redwine_train <- redwine[1:floor(0.8*nrow(redwine)),]
redwine_test <- redwine[(floor(0.8*nrow(redwine))+1):nrow(redwine),]

whitewine_train <- whitewine[1:floor(0.8*nrow(whitewine)),]
whitewine_test <- whitewine[(floor(0.8*nrow(whitewine))+1):nrow(whitewine),]


library(ggplot2)
library(tidyr)

# plot the histogram to explore the distribution of each variables (plot is captured in the word document)
ggplot(gather(redwine_train, cols, value), aes(x = value)) + 
  geom_histogram() + facet_grid(.~cols, scales="free")

ggplot(gather(whitewine_train, cols, value), aes(x = value)) + 
  geom_histogram() + facet_grid(.~cols, scales="free")



# outlier removal

redwine_train_wo_out <- redwine_train

for (i in 1:11) {
  redwine_train_wo_out[,i] <- ifelse(redwine_train_wo_out[,i] %in% boxplot.stats(redwine_train_wo_out[,i])$out, NA, redwine_train_wo_out[,i])
}

redwine_train_wo_out <- redwine_train_wo_out[complete.cases(redwine_train_wo_out),]


whitewine_train_wo_out <- whitewine_train

for (i in 1:11) {
  whitewine_train_wo_out[,i] <- ifelse(whitewine_train_wo_out[,i] %in% boxplot.stats(whitewine_train_wo_out[,i])$out, NA, whitewine_train_wo_out[,i])
}

whitewine_train_wo_out <- whitewine_train_wo_out[complete.cases(whitewine_train_wo_out),]



# Q4 ----

# multiple regression model - multi-factor / predictors
fit_red <- lm(quality ~ ., data = redwine_train)
summary(fit_red)

# Call:
# lm(formula = quality ~ ., data = redwine_train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.67559 -0.38331 -0.07094  0.45606  1.97593 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           2.332e+01  2.359e+01   0.989 0.323052    
# fixed.acidity         2.281e-02  2.965e-02   0.769 0.441931    
# volatile.acidity     -1.052e+00  1.361e-01  -7.729 2.20e-14 ***
# citric.acid          -1.801e-01  1.658e-01  -1.087 0.277460    
# residual.sugar        1.059e-02  1.779e-02   0.596 0.551596    
# chlorides            -1.758e+00  4.641e-01  -3.789 0.000158 ***
# free.sulfur.dioxide   3.320e-03  2.484e-03   1.337 0.181599    
# total.sulfur.dioxide -3.687e-03  8.233e-04  -4.478 8.22e-06 ***
# density              -1.942e+01  2.410e+01  -0.806 0.420436    
# pH                   -3.415e-01  2.130e-01  -1.603 0.109171    
# sulphates             8.072e-01  1.236e-01   6.530 9.49e-11 ***
# alcohol               2.834e-01  2.893e-02   9.795  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6478 on 1267 degrees of freedom
# Multiple R-squared:  0.3696,	Adjusted R-squared:  0.3642 
# F-statistic: 67.54 on 11 and 1267 DF,  p-value: < 2.2e-16



fit_white <- lm(quality ~ ., data = whitewine_train)
summary(fit_white)

# Call:
# lm(formula = quality ~ ., data = whitewine_train)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.4877 -0.5083 -0.0267  0.4738  3.1563 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.468e+02  2.022e+01   7.259 4.69e-13 ***
# fixed.acidity         4.430e-02  2.317e-02   1.912   0.0559 .  
# volatile.acidity     -1.745e+00  1.298e-01 -13.438  < 2e-16 ***
# citric.acid          -3.829e-02  1.069e-01  -0.358   0.7203    
# residual.sugar        7.921e-02  8.200e-03   9.660  < 2e-16 ***
# chlorides            -5.343e-02  6.187e-01  -0.086   0.9312    
# free.sulfur.dioxide   4.750e-03  9.676e-04   4.909 9.54e-07 ***
# total.sulfur.dioxide -2.161e-04  4.220e-04  -0.512   0.6087    
# density              -1.473e+02  2.052e+01  -7.179 8.37e-13 ***
# pH                    7.430e-01  1.173e-01   6.336 2.62e-10 ***
# sulphates             7.509e-01  1.148e-01   6.538 7.02e-11 ***
# alcohol               2.243e-01  2.630e-02   8.528  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7638 on 3906 degrees of freedom
# Multiple R-squared:  0.2993,	Adjusted R-squared:  0.2973 
# F-statistic: 151.7 on 11 and 3906 DF,  p-value: < 2.2e-16



# using step function to step through the predictors
fit_red_formula <- formula(fit_red)
step.model_red = step(fit_red, direction='both', scope=fit_red_formula)
summary(step.model_red)

# Call:
# lm(formula = quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + 
#     total.sulfur.dioxide + pH + sulphates + alcohol, data = redwine_train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.67665 -0.38678 -0.06228  0.46157  1.98870 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           4.1171419  0.4457275   9.237  < 2e-16 ***
# volatile.acidity     -0.9878732  0.1126320  -8.771  < 2e-16 ***
# chlorides            -1.9156022  0.4333427  -4.421 1.07e-05 ***
# free.sulfur.dioxide   0.0039052  0.0024355   1.603  0.10909    
# total.sulfur.dioxide -0.0039048  0.0007578  -5.153 2.97e-07 ***
# pH                   -0.3782620  0.1287288  -2.938  0.00336 ** 
# sulphates             0.7789214  0.1199043   6.496 1.18e-10 ***
# alcohol               0.2964891  0.0180531  16.423  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6473 on 1271 degrees of freedom
# Multiple R-squared:  0.3686,	Adjusted R-squared:  0.3651 
# F-statistic:   106 on 7 and 1271 DF,  p-value: < 2.2e-16



fit_white_formula <- formula(fit_white)
step.model_white = step(fit_white, direction='both', scope=fit_white_formula)
summary(step.model_white)

# Call:
# lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
#     free.sulfur.dioxide + density + pH + sulphates + alcohol, 
#     data = whitewine_train)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.4720 -0.5110 -0.0233  0.4717  3.1587 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          1.497e+02  1.947e+01   7.689 1.86e-14 ***
# fixed.acidity        4.417e-02  2.266e-02   1.949   0.0513 .  
# volatile.acidity    -1.750e+00  1.255e-01 -13.944  < 2e-16 ***
# residual.sugar       8.004e-02  7.944e-03  10.076  < 2e-16 ***
# free.sulfur.dioxide  4.434e-03  7.778e-04   5.700 1.28e-08 ***
# density             -1.503e+02  1.973e+01  -7.616 3.27e-14 ***
# pH                   7.513e-01  1.150e-01   6.531 7.38e-11 ***
# sulphates            7.439e-01  1.141e-01   6.517 8.07e-11 ***
# alcohol              2.231e-01  2.619e-02   8.518  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7635 on 3909 degrees of freedom
# Multiple R-squared:  0.2992,	Adjusted R-squared:  0.2978 
# F-statistic: 208.6 on 8 and 3909 DF,  p-value: < 2.2e-16



# multiple regression model - multi-factor / predictors without outlier
fit_red_wo_out <- lm(quality ~ ., data = redwine_train_wo_out)
summary(fit_red_wo_out)

# Call:
# lm(formula = quality ~ ., data = redwine_train_wo_out)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.05556 -0.36090 -0.05173  0.42230  1.89158 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           30.539780  29.817761   1.024   0.3060    
# fixed.acidity          0.004591   0.034479   0.133   0.8941    
# volatile.acidity      -0.902867   0.168003  -5.374 9.70e-08 ***
# citric.acid           -0.264782   0.190256  -1.392   0.1643    
# residual.sugar         0.021696   0.055124   0.394   0.6940    
# chlorides             -0.359595   1.429690  -0.252   0.8015    
# free.sulfur.dioxide    0.006613   0.002979   2.220   0.0266 *  
# total.sulfur.dioxide  -0.004510   0.001064  -4.237 2.48e-05 ***
# density              -26.583680  30.442119  -0.873   0.3827    
# pH                    -0.532588   0.248395  -2.144   0.0323 *  
# sulphates              1.639331   0.187371   8.749  < 2e-16 ***
# alcohol                0.282276   0.036537   7.726 2.83e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5863 on 945 degrees of freedom
# Multiple R-squared:  0.4136,	Adjusted R-squared:  0.4068 
# F-statistic:  60.6 on 11 and 945 DF,  p-value: < 2.2e-16


fit_white_wo_out <- lm(quality ~ ., data = whitewine_train_wo_out)
summary(fit_white_wo_out)

# Call:
# lm(formula = quality ~ ., data = whitewine_train_wo_out)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.4324 -0.5198 -0.0108  0.4648  2.8104 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.995e+02  3.084e+01   6.470 1.13e-10 ***
# fixed.acidity         1.389e-01  3.116e-02   4.458 8.56e-06 ***
# volatile.acidity     -1.603e+00  1.864e-01  -8.600  < 2e-16 ***
# citric.acid          -1.472e-01  1.582e-01  -0.930    0.352    
# residual.sugar        9.501e-02  1.151e-02   8.254  < 2e-16 ***
# chlorides            -2.394e+00  1.690e+00  -1.417    0.157    
# free.sulfur.dioxide   5.589e-03  1.200e-03   4.656 3.35e-06 ***
# total.sulfur.dioxide  3.100e-04  4.976e-04   0.623    0.533    
# density              -2.014e+02  3.125e+01  -6.446 1.32e-10 ***
# pH                    1.051e+00  1.447e-01   7.264 4.68e-13 ***
# sulphates             9.735e-01  1.449e-01   6.716 2.19e-11 ***
# alcohol               1.510e-01  3.855e-02   3.917 9.14e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7543 on 3217 degrees of freedom
# Multiple R-squared:  0.2703,	Adjusted R-squared:  0.2678 
# F-statistic: 108.4 on 11 and 3217 DF,  p-value: < 2.2e-16


# using step function to step through the predictors without outlier

fit_red_formula_wo_out <- formula(fit_red_wo_out)
step.model_red_wo_out = step(fit_red_wo_out, direction='both', scope=fit_red_formula_wo_out)
summary(step.model_red_wo_out)

# lm(formula = quality ~ volatile.acidity + citric.acid + free.sulfur.dioxide + 
#     total.sulfur.dioxide + pH + sulphates + alcohol, data = redwine_train_wo_out)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.05324 -0.36034 -0.04741  0.41602  1.89771 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           4.0330619  0.5946418   6.782 2.08e-11 ***
# volatile.acidity     -0.9809753  0.1562543  -6.278 5.21e-10 ***
# citric.acid          -0.3705907  0.1529924  -2.422 0.015610 *  
# free.sulfur.dioxide   0.0065308  0.0029382   2.223 0.026468 *  
# total.sulfur.dioxide -0.0044330  0.0009955  -4.453 9.47e-06 ***
# pH                   -0.5666460  0.1664492  -3.404 0.000691 ***
# sulphates             1.5751331  0.1799430   8.754  < 2e-16 ***
# alcohol               0.3098503  0.0219678  14.105  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5856 on 949 degrees of freedom
# Multiple R-squared:  0.4126,	Adjusted R-squared:  0.4082 
# F-statistic: 95.21 on 7 and 949 DF,  p-value: < 2.2e-16


fit_white_formula_wo_out <- formula(fit_white_wo_out)
step.model_white_wo_out = step(fit_white_wo_out, direction='both', scope=fit_white_formula_wo_out)
summary(step.model_white_wo_out)

# Call:
# lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
#     free.sulfur.dioxide + density + pH + sulphates + alcohol, 
#     data = whitewine_train_wo_out)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.4320 -0.5264 -0.0100  0.4634  2.8008 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          2.073e+02  2.851e+01   7.271 4.46e-13 ***
# fixed.acidity        1.398e-01  3.044e-02   4.594 4.51e-06 ***
# volatile.acidity    -1.554e+00  1.797e-01  -8.645  < 2e-16 ***
# residual.sugar       9.785e-02  1.081e-02   9.049  < 2e-16 ***
# free.sulfur.dioxide  5.936e-03  9.470e-04   6.269 4.13e-10 ***
# density             -2.094e+02  2.886e+01  -7.257 4.95e-13 ***
# pH                   1.076e+00  1.421e-01   7.570 4.85e-14 ***
# sulphates            9.804e-01  1.445e-01   6.782 1.40e-11 ***
# alcohol              1.477e-01  3.784e-02   3.902 9.73e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7544 on 3220 degrees of freedom
# Multiple R-squared:  0.2696,	Adjusted R-squared:  0.2678 
# F-statistic: 148.6 on 8 and 3220 DF,  p-value: < 2.2e-16





# Q5 ----

M <- cor(redwine) # get correlations

library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix


#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           4.1171419  0.4457275   9.237  < 2e-16 ***
# volatile.acidity     -0.9878732  0.1126320  -8.771  < 2e-16 ***
# chlorides            -1.9156022  0.4333427  -4.421 1.07e-05 ***
# free.sulfur.dioxide   0.0039052  0.0024355   1.603  0.10909    
# total.sulfur.dioxide -0.0039048  0.0007578  -5.153 2.97e-07 ***
# pH                   -0.3782620  0.1287288  -2.938  0.00336 ** 
# sulphates             0.7789214  0.1199043   6.496 1.18e-10 ***
# alcohol               0.2964891  0.0180531  16.423  < 2e-16 ***

# Multicollinearity between free.sulfur.dioxide and total.sulfur.dioxide
# Exclude free.sulfur.dioxide from the model:

improved.step.model_red = lm(formula = quality ~ volatile.acidity + chlorides +
                               total.sulfur.dioxide + pH + sulphates + alcohol, data = redwine_train)
summary(improved.step.model_red)

# Call:
# lm(formula = quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + 
#     pH + sulphates + alcohol, data = redwine_train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.60944 -0.37929 -0.07128  0.46400  1.95003 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           4.0181035  0.4416996   9.097  < 2e-16 ***
# volatile.acidity     -1.0081450  0.1119893  -9.002  < 2e-16 ***
# chlorides            -1.9140239  0.4336091  -4.414 1.10e-05 ***
# total.sulfur.dioxide -0.0030719  0.0005521  -5.564 3.21e-08 ***
# pH                   -0.3450333  0.1271282  -2.714  0.00674 ** 
# sulphates             0.7827645  0.1199544   6.526 9.76e-11 ***
# alcohol               0.2982770  0.0180298  16.544  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6477 on 1272 degrees of freedom
# Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3644 
# F-statistic: 123.1 on 6 and 1272 DF,  p-value: < 2.2e-16


M <- cor(whitewine) # get correlations

corrplot(M, method = "circle") #plot matrix


#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          1.497e+02  1.947e+01   7.689 1.86e-14 ***
# fixed.acidity        4.417e-02  2.266e-02   1.949   0.0513 .  
# volatile.acidity    -1.750e+00  1.255e-01 -13.944  < 2e-16 ***
# residual.sugar       8.004e-02  7.944e-03  10.076  < 2e-16 ***
# free.sulfur.dioxide  4.434e-03  7.778e-04   5.700 1.28e-08 ***
# density             -1.503e+02  1.973e+01  -7.616 3.27e-14 ***
# pH                   7.513e-01  1.150e-01   6.531 7.38e-11 ***
# sulphates            7.439e-01  1.141e-01   6.517 8.07e-11 ***
# alcohol              2.231e-01  2.619e-02   8.518  < 2e-16 ***

# Multicollinearity between residual.sugar and density
# Exclude density from the model:

improved.step.model_white = lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                                 free.sulfur.dioxide + pH + sulphates + alcohol, 
                               data = whitewine_train)
summary(improved.step.model_white)


# Call:
# lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
#     free.sulfur.dioxide + pH + sulphates + alcohol, data = whitewine_train)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.3729 -0.5100 -0.0178  0.4708  3.2436 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          1.4659409  0.3857029   3.801 0.000146 ***
# fixed.acidity       -0.0763247  0.0163392  -4.671 3.09e-06 ***
# volatile.acidity    -1.9206521  0.1244258 -15.436  < 2e-16 ***
# residual.sugar       0.0234435  0.0028257   8.297  < 2e-16 ***
# free.sulfur.dioxide  0.0047199  0.0007825   6.032 1.77e-09 ***
# pH                   0.2211367  0.0922498   2.397 0.016570 *  
# sulphates            0.4907318  0.1099860   4.462 8.36e-06 ***
# alcohol              0.4028446  0.0114480  35.189  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7691 on 3910 degrees of freedom
# Multiple R-squared:  0.2888,	Adjusted R-squared:  0.2875 
# F-statistic: 226.8 on 7 and 3910 DF,  p-value: < 2.2e-16


# similar for without outliers

improved.step.model_red_wo_out = lm(formula = quality ~ volatile.acidity + chlorides +
                               total.sulfur.dioxide + pH + sulphates + alcohol, data = redwine_train_wo_out)
summary(improved.step.model_red_wo_out)

# Call:
# lm(formula = quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + 
#     pH + sulphates + alcohol, data = redwine_train_wo_out)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -2.11009 -0.35150 -0.05962  0.44395  1.83630 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.1927652  0.5397958   5.915 4.63e-09 ***
# volatile.acidity     -0.7849302  0.1311661  -5.984 3.08e-09 ***
# chlorides            -0.9978337  1.3673721  -0.730   0.4657    
# total.sulfur.dioxide -0.0031280  0.0007561  -4.137 3.83e-05 ***
# pH                   -0.3264983  0.1457563  -2.240   0.0253 *  
# sulphates             1.5567471  0.1791418   8.690  < 2e-16 ***
# alcohol               0.3074677  0.0218318  14.083  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5889 on 950 degrees of freedom
# Multiple R-squared:  0.4052,	Adjusted R-squared:  0.4014 
# F-statistic: 107.9 on 6 and 950 DF,  p-value: < 2.2e-16


improved.step.model_white_wo_out = lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                                 free.sulfur.dioxide + pH + sulphates + alcohol, 
                               data = whitewine_train_wo_out)
summary(improved.step.model_white_wo_out)

# Call:
# lm(formula = quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
#     free.sulfur.dioxide + pH + sulphates + alcohol, data = whitewine_train_wo_out)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.3480 -0.5230 -0.0125  0.4614  2.7388 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          0.4266255  0.4527533   0.942 0.346114    
# fixed.acidity       -0.0277975  0.0199793  -1.391 0.164226    
# volatile.acidity    -1.7324881  0.1794678  -9.653  < 2e-16 ***
# residual.sugar       0.0231989  0.0033569   6.911 5.79e-12 ***
# free.sulfur.dioxide  0.0060352  0.0009544   6.323 2.91e-10 ***
# pH                   0.3867343  0.1065668   3.629 0.000289 ***
# sulphates            0.6394673  0.1377939   4.641 3.61e-06 ***
# alcohol              0.4057149  0.0130294  31.138  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7604 on 3221 degrees of freedom
# Multiple R-squared:  0.2577,	Adjusted R-squared:  0.2561 
# F-statistic: 159.7 on 7 and 3221 DF,  p-value: < 2.2e-16




# Q6 ----

# (explanation is provided in the word documents)



# Q7 ----

predicted_red <- predict(step.model_red, redwine_test)
predicted_white <- predict(step.model_white, whitewine_test)

predicted_red_wo_out <- predict(step.model_red_wo_out, redwine_test)
predicted_white_wo_out <- predict(step.model_white_wo_out, whitewine_test)


improved.predicted_red <- predict(improved.step.model_red, redwine_test)
improved.predicted_white <- predict(improved.step.model_white, whitewine_test)

improved.predicted_red_wo_out <- predict(improved.step.model_red_wo_out, redwine_test)
improved.predicted_white_wo_out <- predict(improved.step.model_white_wo_out, whitewine_test)


# Q8 ----

residual_red <- redwine_test$quality - predicted_red
rmse_red <- sqrt(sum(residual_red * residual_red) / length(residual_red))
rmse_red
# [1] 0.6569716

residual_white <- whitewine_test$quality - predicted_white
rmse_white <- sqrt(sum(residual_white * residual_white) / length(residual_white))
rmse_white
# [1] 0.7121194

residual_red_wo_out <- redwine_test$quality - predicted_red_wo_out
rmse_red_wo_out <- sqrt(sum(residual_red_wo_out * residual_red_wo_out) / length(residual_red_wo_out))
rmse_red_wo_out
# [1] 0.660956

residual_white_wo_out <- whitewine_test$quality - predicted_white_wo_out
rmse_white_wo_out <- sqrt(sum(residual_white_wo_out * residual_white_wo_out) / length(residual_white_wo_out))
rmse_white_wo_out
# [1] 0.7191801

improved.residual_red <- redwine_test$quality - improved.predicted_red
improved.rmse_red <- sqrt(sum(improved.residual_red * improved.residual_red) / length(improved.residual_red))
improved.rmse_red
# [1] 0.6596683

improved.residual_white <- whitewine_test$quality - improved.predicted_white
improved.rmse_white <- sqrt(sum(improved.residual_white * improved.residual_white) / length(improved.residual_white))
improved.rmse_white
# [1] 0.7157928

improved.residual_red_wo_out <- redwine_test$quality - improved.predicted_red_wo_out
improved.rmse_red_wo_out <- sqrt(sum(improved.residual_red_wo_out * improved.residual_red_wo_out) / length(improved.residual_red_wo_out))
improved.rmse_red_wo_out
# [1] 0.6597003

improved.residual_white_wo_out <- whitewine_test$quality - improved.predicted_white_wo_out
improved.rmse_white_wo_out <- sqrt(sum(improved.residual_white_wo_out * improved.residual_white_wo_out) / length(improved.residual_white_wo_out))
improved.rmse_white_wo_out
# [1] 0.7234663


# To interpret RMSE, the lower the value, the better the model prediction


df_red <- data.frame(predicted=predicted_red, actual=redwine_test$quality)
ggplot(df_red, aes(x=predicted, y=actual)) + geom_point()


df_white <- data.frame(predicted=predicted_white, actual=whitewine_test$quality)
ggplot(df_white, aes(x=predicted, y=actual)) + geom_point()


df_red_wo_out <- data.frame(predicted=predicted_red_wo_out, actual=redwine_test$quality)
ggplot(df_red_wo_out, aes(x=predicted, y=actual)) + geom_point()


df_white_wo_out <- data.frame(predicted=predicted_white_wo_out, actual=whitewine_test$quality)
ggplot(df_white_wo_out, aes(x=predicted, y=actual)) + geom_point()



# Q9 ----


red_mean <- mean(redwine_test$quality)
total_variance <- sum((redwine_test$quality - red_mean)^2)
residual_variance <- sum(residual_red^2)
R_red = 1 - residual_variance / total_variance
R_red
# [1] 0.2873284


white_mean <- mean(whitewine_test$quality)
total_variance <- sum((whitewine_test$quality - white_mean)^2)
residual_variance <- sum(residual_white^2)
R_white = 1 - residual_variance / total_variance
R_white
# [1] 0.1566482


red_mean_wo_out <- mean(redwine_test$quality)
total_variance <- sum((redwine_test$quality - red_mean_wo_out)^2)
residual_variance <- sum(residual_red_wo_out^2)
R_red_wo_out = 1 - residual_variance / total_variance
R_red_wo_out
# [1] 0.2786579


white_mean_wo_out <- mean(whitewine_test$quality)
total_variance <- sum((whitewine_test$quality - white_mean_wo_out)^2)
residual_variance <- sum(residual_white_wo_out^2)
R_white_wo_out = 1 - residual_variance / total_variance
R_white_wo_out
# [1] 0.1398414


# R-squared for red wine model is higher than white wine model.
# Red wine model is more accurate than white wine model.
