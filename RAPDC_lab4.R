##### Regression analysis: panel data and causality, 2021 
##### Lab 4. Mixed-effects models (part 2) 

install.packages("haven")
install.packages("arm")
install.packages("ggplot2")
install.packages("sjPlot")
install.packages("dplyr")

library(haven)
library(arm)
library(ggplot2)
library(sjPlot)
library(dplyr)

# open RAPDC_lab1.dta 
data <- read_dta(file.choose())

# visualize what happens if we regress political regime on period by groups (different countries)
data$period <- data$period - 1
head(data)
anovaicc <- aov(fh_polity ~ country, data) 
ICC1(anovaicc)

ggplot(data, aes(x=period, y=fh_polity, group = country))  + theme_classic()+
  geom_smooth(method=lm, se = TRUE, color = "black") + geom_point() + facet_wrap(~country)

model1 <- lmer(fh_polity ~ period + (1 + period|country), REML = FALSE, data)
summary(model1)

# plot predicted values (only fixed part)
plot_model(model1, type = "pred")

# save predicted values (fixed + random part)
pr_re <- predict(model1)
pr_re

# save predicted values (only fixed part)
pr_fe <- predict(model1, re.form = NA)
pr_fe

# plot predicted values with a random part (+BLUP)
data %>% 
  # add predicted values to the dataset
  mutate(pr_re = predict(model1), pr_fe = predict(model1, re.form = NA)) %>% 
  # visualize
  ggplot(aes(x=period, y=pr_re, group = country)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(period, pr_fe)) + facet_wrap(~country)

# Do we need a quadratic term? Visualize the data
ggplot(data, aes(x = period, y = fh_polity)) + theme_light()+
  geom_line() + geom_point()+facet_wrap( ~ country)

# you can you loess as a smooth function, but we do not have enough observations. The example is as follows:
# ggplot(data, aes(x=period, y=fh_polity))  + theme_light()+
# geom_smooth(method=loess, se = TRUE, color = "black") + geom_point() + facet_wrap(~country)

# add fixed effects for the period and quadratic period variables
model2 <- lmer(fh_polity ~ period + I(period^2) + (1 + period|country), REML = FALSE, data)
summary(model2)

# plot predicted values (only fixed part). You can extend the period for a smoother graph 
plot_model(model2, type = "pred", terms = "period [0:15]")

# plot predicted values with a random part (+BLUP)
data %>% 
  # add predicted values to the dataset
  mutate(pr_re2 = predict(model2), pr_fe2 = predict(model2, re.form = NA)) %>% 
  # visualize
  ggplot(aes(x=period, y=pr_re2, group = country)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(period, pr_fe2)) + facet_wrap(~country)

# you can run this code without facet_wrap to plot all the lines at once. But this may result in an overloaded graph
