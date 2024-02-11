install.packages('fastDummies')
library(fastDummies)
library(dplyr)
library(tidyverse)

data <- X1
attach(data)

#確認quality等級有幾個：等級3~8
category_qual <- data %>% 
  group_by(quality) %>% 
  summarise(count=n())
category_qual

#設為五個dummy variable，等級3為base group
#整理好資料為data1
data1 <- dummy_columns(data,select_columns = "quality",remove_first_dummy = TRUE)
data1 <- select(data1,-quality)


#確認變數間無完全共線性
library(corrplot)
cor=cor(data1[,c(1:16)])
cor
corrplot(cor,method = "number")

#確認變數間無完全共線性
#開始對資料跑迴歸
attach(data1)
fit <- lm(`total sulfur dioxide`~.,data=data1)
summary(fit)

#  設定alpha=0.05
#  "residual sugar"、"alcohol"的p-value大於0.025，所以無法拒絕虛無假設
#  "quality"只有quality_5有顯著，但因為quality為dummy variable，單一項無解釋作用，所以應該全部拿掉

#拿掉"residual sugar"、"alcohol"以及所有化為dummy的"quality 4~8"
fit1 <- lm(`total sulfur dioxide`~`fixed acidity`+`volatile acidity`+`citric acid`+chlorides+`free sulfur dioxide`+density+pH+sulphates,data=data1)
summary(fit1)

#假設檢定: F-test for joint significant
#看拿掉的"residual sugar"、"alcohol"是否存在joint significant
f_statistic <- ((summary_0$r.squared-summary_1$r.squared)/2)/((1-summary_0$r.squared)/(nrow(data)-10))
f_statistic

#sulphates在新的迴歸式中並不顯著，因此考慮把此變數拿掉
fit2 <- lm(`total sulfur dioxide`~`fixed acidity`+`volatile acidity`+`citric acid`+chlorides+`free sulfur dioxide`+density+pH,data=data1)
summary(fit2)

# 跑Breusch-pegan test，看fit2 mode的error是否為homoskedasticity
# u_hat^2 = b0+b1X1+b2X2+...+error
# H0:b0=b1=...=0（與給定的x無關）
library(lmtest)
lmtest::bptest(fit2)

#發現拒絕虛無假設，代表模型目前為Heteroskedasticity

#做Feasible GLS
u_hat <- resid(fit2)

log_u_hat_sqr <- log(u_hat^2)

reg_u_hat <- lm(log_u_hat_sqr~`fixed acidity`+`volatile acidity`+`citric acid`+chlorides+`free sulfur dioxide`+density+pH)

g_i <- fitted.values(reg_u_hat)

h_hat <- exp(g_i)

w <- 1/h_hat

#weights=w
weighted_fit2 <- lm(`total sulfur dioxide`~`fixed acidity`+`volatile acidity`+`citric acid`+chlorides+`free sulfur dioxide`+density+pH,data=data1,weights = w)
summary(weighted_fit2)

#確認為Homoskedasticity
lmtest::bptest(weighted_fit2)

#weights=w
weighted_fit3 <- lm(`total sulfur dioxide`~`fixed acidity`+`volatile acidity`+`citric acid`+`free sulfur dioxide`+density+pH,data=data1,weights = w)
summary(weighted_fit3)

lmtest::bptest(weighted_fit3)
