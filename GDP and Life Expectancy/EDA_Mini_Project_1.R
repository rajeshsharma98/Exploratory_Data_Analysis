library(ggplot2)
library(dplyr)
library(gapminder)
library(tidyverse)
library(broom)
library(viridis)
library(viridisLite)

#PART-1
#log transformation of GDP per capita
gapminder$gdp_log = log(gapminder$gdpPercap)

#log_GDP and Life expectancy in 2007
#data for year 2007
gapminder_2007 = subset(gapminder, gapminder$year  == 2007)

#GDP per capita vs life expectancy in year 2007 scatter-plot
ggplot(gapminder_2007, aes(x = gdp_log, y = lifeExp)) + geom_point() + 
  ggtitle("Life expectancy vs log of GDP per capita for year 2007") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Log of GDP per capita") + 
  ylab("Life Expectancy") + geom_smooth(model = lm, se = FALSE)
gapminder_lm = lm(lifeExp ~ gdp_log, data = gapminder_2007)
gm_2007_res = data.frame(continent = gapminder_2007$continent, residual = residuals(gapminder_lm))

#QQ plot to check normality of residuals
ggplot(gm_2007_res, aes(sample = residual)) + stat_qq() + geom_abline(intercept = mean(gm_2007_res$residual), slope=summary(gapminder_lm)$sigma) + 
  ggtitle("QQ plot of residuals") + theme(plot.title = element_text(hjust = 0.5))

# Point plot between residuals of linear model fitted on gapminder 2007 data vs log of GDP per capita
ggplot(augment(gapminder_lm,data = gapminder_2007)) +
  geom_point(aes(x = gdp_log, y = .resid)) + ggtitle("Point plot of residuals vs log of GDP") + theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals of linear model") + 
  xlab("Log of GDP per Capita")

#removing oceania to fit linear model to each continent
gm_2007 <- gapminder_2007[gapminder_2007$continent != "Oceania",]
#GDP per capita vs life expectancy for each continent in 2007
ggplot(gm_2007,aes(x = gdp_log, y = lifeExp)) + geom_point() + facet_wrap( ~continent, scale = "free_x", ncol = 2) + 
  ggtitle("Life Expectancy vs GDP per capita for each continent") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Log of GDP per capita") + ylab("Life Expectancy") + geom_smooth(model = lm, se = FALSE)

#PART-2
#Life Expectancy over time by continent
#weighted mean based on population of countries
mean_continent_year<- gapminder %>% group_by(continent,year) %>% summarize(mean = weighted.mean(lifeExp, pop))

#Line plot of each continent for mean life expectancy vs year
ggplot() + geom_line(data=mean_continent_year,aes(x=year,y=mean)) + facet_wrap( ~continent) + 
  ggtitle("Average Life Expectancy vs Year graph for each continent") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Average life expectancy") + 
  xlab("Year")
#combined graph of average life expectancy vs year
ggplot(mean_continent_year,aes(x=year,y=mean,color=continent)) + geom_line() + 
  ggtitle("Average Life Expectancy vs Year graph for each continent(combined)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Average life expectancy") + 
  xlab("Year") + geom_point() + geom_smooth()

#life expectancy of each country of Asia to see which country helped in catching up
gapminder_asia = subset(gapminder, gapminder$continent == "Asia")
ggplot() + geom_line(data=gapminder_asia,aes(x=year,y=lifeExp)) + facet_wrap( ~country) + 
  ggtitle("Life Expectancy vs Year graph of each Country of Asia") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Life Expectancy") + 
  xlab("Year")

#life expectancy of each country of Africa to see which countries are responsible for decrease in life expectancy 
#between years 1987-2007
gapminder_africa = subset(gapminder, gapminder$continent == "Africa")
ggplot() + geom_line(data=gapminder_africa,aes(x=year,y=lifeExp)) + facet_wrap( ~country) + 
  ggtitle("Life Expectancy vs Year graph of each Country of Africa") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Life Expectancy") + 
  xlab("Year")

#PART-3
#change in relationship between gdp and life expectancy for each continent for different years
ggplot(data = gapminder, aes(x=gdp_log,y=lifeExp)) + geom_point(aes(color = year)) + 
  geom_smooth(color = "red") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~continent, scale = "free_x") + ggtitle("Change in Life expectancy vs GDP per capita for each continent over years") + 
  xlab("Log of GDP per capita") + 
  ylab("Life Expectancy")
  
#Overall comparision of GDP per capita vs Life expectancy
ggplot(gapminder, aes(x = gdp_log, y = lifeExp)) + geom_point() + geom_smooth(se = F) + 
  ggtitle("Life Expectancy vs Log of GDP per capita") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Log of GDP per capita") + 
  ylab("Life Expectancy")

#relation of year + gdp vs lifeExp
gm_lm1 = lm(lifeExp ~ gdp_log + year, data = gapminder)
gm_res = data.frame(continent = gapminder$continent, residual = residuals(gm_lm1))
ggplot(gm_res, aes(sample = residual)) + stat_qq() + geom_abline(intercept = mean(gm_res$residual), slope=summary(gm_lm1)$sigma) + 
  ggtitle("QQ plot of residuals for Linear model with gdp + year") + theme(plot.title = element_text(hjust = 0.5))
ggplot(augment(gm_lm1,data = gapminder)) +
  geom_point(aes(x = gdp_log + year, y = .resid)) + ggtitle("Point plot of residuals vs log of GDP for linear model with gdp + year") + theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals of linear model") + 
  xlab("Log of GDP per Capita")

# Linear model with lifeExp~gdp_log
gapminder_lm1 = lm(lifeExp ~ gdp_log, data = gapminder)
gm1_res = data.frame(continent = gapminder$continent, residual = residuals(gapminder_lm1))
ggplot(gm1_res, aes(sample = residual)) + stat_qq() + geom_abline(intercept = mean(gm1_res$residual), slope=summary(gapminder_lm1)$sigma) + 
  ggtitle("QQ plot of residuals") + theme(plot.title = element_text(hjust = 0.5))
ggplot(augment(gapminder_lm1,data = gapminder)) +
  geom_point(aes(x = gdp_log, y = .resid)) + ggtitle("Point plot of residuals vs log of GDP") + theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals of linear model") + 
  xlab("Log of GDP per Capita")


#Linear Model with lifeExp ~ gdp_log + continent
gapminder_lm3 = lm(lifeExp ~ gdp_log + continent, data = gapminder)
gm3_res = data.frame(continent = gapminder$continent, residual = residuals(gapminder_lm3))
ggplot(gm3_res, aes(sample = residual)) + stat_qq() + geom_abline(intercept = mean(gm3_res$residual), slope=summary(gapminder_lm3)$sigma) + 
  ggtitle("QQ plot of residuals for Linear model with gdp + gdp^2") + theme(plot.title = element_text(hjust = 0.5))
ggplot(augment(gapminder_lm3,data = gapminder)) +
  geom_point(aes(x = gdp_log + I(gdp_log^2), y = .resid)) + ggtitle("Point plot of residuals vs log of GDP for Linear model with gdp + gdp^2") + theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals of linear model") + 
  xlab("Log of GDP per Capita")

gapminder_lm1 = lm(lifeExp ~ gdp_log, data = gapminder)
gm1_res = data.frame(continent = gapminder$continent, residual = residuals(gapminder_lm1))
ggplot(gm1_res, aes(sample = residual)) + stat_qq() + geom_abline(intercept = mean(gm1_res$residual), slope=summary(gapminder_lm1)$sigma) + 
  ggtitle("QQ plot of residuals") + theme(plot.title = element_text(hjust = 0.5))
ggplot(augment(gapminder_lm1,data = gapminder)) +
  geom_point(aes(x = gdp_log, y = .resid)) + ggtitle("Point plot of residuals vs log of GDP") + theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Residuals of linear model") + 
  xlab("Log of GDP per Capita")


