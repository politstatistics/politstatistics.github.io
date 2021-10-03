##### Regression analysis: panel data and causality, 2021 
##### Lab 2. FE- and RE-models (part 2) 

install.packages("haven")
install.packages("plm")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")

library(haven)
library(plm)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)

# use data RAPDC_lab1.dta
panel<-read_dta(file.choose())

# Reminder: Only country effects
LSDV_country <- lm(fh_polity~state_capacity + country, data = panel)
summary(LSDV_country)
fe_country <- plm(fh_polity~state_capacity, data = panel, index=c("country", "period"), effect = "individual", model="within")
summary(fe_country)
# extract country effects
summary(fixef(fe_country))

### Do we need a fixed-effects model? 
# run a pooled model by using plm package
ols <- plm(fh_polity~state_capacity, data=panel, model="pooling")
summary(ols)

pFtest(fe_country, ols)

# run a random-effects model (only intercepts are different)
re <- plm(fh_polity~state_capacity, data=panel, index=c("country", "period"), model="random")
summary(re)

# Breush-Pagan test. Do we need a random-effects model?  
plmtest(ols, type=c("bp"))

# Hausman test: FE VS RE original
phtest(fe_country, re)

# Only time effects
LSDV_time <- lm(fh_polity ~ state_capacity + as.factor(period), data = panel)
summary(LSDV_time)
fe_time <- plm(fh_polity ~ state_capacity, data = panel, index=c("country", "period"), effect = "time", model = "within")
summary(fe_time)
# extract time effects
summary(fixef(fe_time))

# Compare more and less parsimonious models (without and with time effects)
pFtest(fe_time, ols)

# For illustration: Both country and time effects
fe_twoways <- plm(fh_polity ~ state_capacity, data = panel, index=c("country", "period"), effect = "twoways", model = "within")
summary(fe_twoways)

# Heteroskedasticity adjustment 
bptest(fe_country)
coeftest(fe_country, vcov = vcovHC, type = "HC3")

### Test whether the model fits well
y_pred <- LSDV_country$fitted 
panel1 <- data.frame(panel, y_pred) 
# subset the data by countries and find how strongly y observed and predicted are correlated
merged <- panel1 %>% group_by(country)%>% summarize(., cor(fh_polity, y_pred))%>% merge(panel1, ., by="country")
# test whether results are robust to excluding observations with small correlations
merged$new <- ifelse(abs(merged$`cor(fh_polity, y_pred)`)<0.3,1,0)
fe_country_2 <- plm(fh_polity ~ state_capacity, merged[merged$new == 0,], index=c("country", "period"), effect = "individual")
coeftest(fe_country_2, vcov = vcovHC, type = "HC3")

### test a FE-model with varying slopes
feslope <- lm(fh_polity~state_capacity*country, data = panel)
summary(feslope)
feslope_pred <- feslope$fitted
# plot varying slopes 
colors <- colors(distinct = TRUE)
mypalette <- sample(colors, length(unique(panel$country)))
ggplot(panel, aes(x = state_capacity, y = feslope_pred, color = country))+ geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = mypalette)
