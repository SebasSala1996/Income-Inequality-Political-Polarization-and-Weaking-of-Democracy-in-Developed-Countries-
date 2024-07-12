##Libraries and setting of the working directory

library(devtools)
library(ggplot2)
library(dplyr)
library(foreign)
library(car)
library(AER)
library(stargazer)
library(plm)
load_all
setwd("C:\\Users\\SEBASTIAN\\Documents\\MA Social Science\\Regimes, State and Institutions\\Research proposal and investigation")

##Open V-dem, voting share & income share data 

devtools::install_github("vdeminstitute/vdemdata")
v_dem<-read.csv("V-Dem-CY-Full+Others-v10.csv", header = TRUE, sep = ",", dec = ".")

##Data analisys 
#Countries in V dem dataset and the indexes description

#V dem data
count(v_dem, country_name) 
summary(v_dem$v2x_polyarchy)
summary(v_dem$v2xeg_eqdr)
summary(v_dem$v2smpolsoc)
summary(v_dem$e_peedgini)

#Voting share
count(voting_share, country_name) 
count(voting_share, family_name)


                 ##Median GDP pc = 9324 and mean GDP pc = 15937

    #Subset V dem dataset only to develped countries

developed_countries_1<-(filter(v_dem, e_migdppc&year > 2000))
count(developed_countries_1, country_name)
developed_countries<-filter(v_dem, country_name == "Argentina" |country_name == "Australia"|country_name == "Austria"|country_name == "Bahrain"|country_name == "Belgium"|country_name == "Canada"|country_name == "Croatia"|country_name == "Cyprus"|country_name == "Czech Republic"|country_name == "Denmark"|country_name == "Estonia"|country_name == "Finland"|country_name == "France"|country_name == "Germany" |country_name == "Greece"|country_name == "Hong Kong"|country_name == "Hungary"|country_name == "Iceland"|country_name == "Ireland"|country_name == "Israel"|country_name == "Italy"|country_name == "Japan" |country_name == "Latvia"|country_name == "Lithuania"|country_name == "Malta"|country_name == "Netherlands"|country_name == "New Zealand"|country_name == "Norway"|country_name == "Poland"|country_name == "Portugal"|country_name == "Russia"|country_name == "Singapore"|country_name == "Slovakia"|country_name == "Slovenia"|country_name == "South Korea"|country_name == "Spain"|country_name == "Sweden"|country_name == "Switzerland"|country_name == "Taiwan"|country_name == "United Kingdom"|country_name == "United States of America")
    #Subset developed_countries dataset to 1970-2019

t_dev_coun<-filter(developed_countries, year > 1969)
count(developed_countries, country_name)

    #Evolution of Index by year

  #Liberal Democracy Index evolution through 1789-2019
y<-(tapply(developed_countries$v2x_libdem,developed_countries$year, mean))
plot(c(1789:2019),y,type = "l",ylim=c(0.6, 0.8),xlim=c(2003,2019), xlab = "Year", ylab = "Liberal Democracy Index")
t_dev_coun<-filter(developed_countries, year > 1999)

boxplot(t_dev_coun$v2x_libdem~t_dev_coun$year,xlab = "Year", ylab = "Liberal Democracy Index")

  #Inequality Index evolution through 1900-2019
x<-(tapply(developed_countries$v2xeg_eqdr,developed_countries$year, mean))
plot(c(1789:2019),x,type = "l",ylim=c(0.8, 0.95),xlim=c(2000,2019) , xlab = "Year", ylab = " Equal distribution of resources Index")

  #Polarization evolution through 2000-2019
boxplot(developed_countries$v2smpolsoc~developed_countries$year,xlab = "Year", ylab = "Polarization")

  #Evolution by 5 years interval since 1970
breaks <- seq(from = 1970, to = 2019, by = 5)

     #Polyarchy Boxplot 
boxplot(t_dev_coun$v2x_polyarchy~cut(t_dev_coun$year,breaks=breaks, right=FALSE), xlab = "Year", ylab = "Polyarchy Index")

     #Inequality Boxplot
boxplot(t_dev_coun$v2xeg_eqdr~cut(t_dev_coun$year,breaks=breaks, right=FALSE), xlab = "Year", ylab = "Inequality")

     #Education Ine. Boxplot 
boxplot(t_dev_coun$e_peedgini~cut(t_dev_coun$year,breaks=breaks, right=FALSE), xlab = "Year", ylab = "Education Inequality")

     #Power distribution Boxplot 
boxplot(t_dev_coun$v2pepwrses~cut(t_dev_coun$year,breaks=breaks, right=FALSE), xlab = "Year", ylab = "Power distribution")

#reg1 <- lm(v2smpolsoc~ v2xeg_eqdr, data=t_dev_coun)
#summary(reg1, diagnostics=TRUE)
#reg2 <- lm(v2x_libdem~ v2smpolsoc, data=t_dev_coun)
#summary(reg2, diagnostics=TRUE)
#reg3 <- lm(v2x_libdem~ v2smpolsoc+v2xeg_eqdr+(v2smpolsoc*v2xeg_eqdr), data=t_dev_coun)
#summary(reg3, diagnostics=TRUE)

reg1 <-plm(v2smpolsoc~ v2xeg_eqdr, data=t_dev_coun, model = "within", index = c("country_name", "year"))
reg2 <-plm(v2x_libdem~ v2smpolsoc, data=t_dev_coun, model = "within", index = c("country_name", "year"))
reg3 <-plm(v2x_libdem~ v2smpolsoc+v2xeg_eqdr+(v2smpolsoc*v2xeg_eqdr),data=t_dev_coun, model = "within", index = c("country_name", "year"))  
stargazer(reg1,reg2,reg3,type = "latex",dep.var.labels=c("Polarization Index","Liberal Democracy Index"),covariate.labels=c("Equal distribution of resources Index","Equal distribution of resources Index*Polarization Index",
                                                                                                                            "Polarization Index"), style = "aer", out = "T1.tex")