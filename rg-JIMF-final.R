rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library(readr)
library(countrycode)
library(devtools)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)

#read data
dat <- fread(here("rg-dynamics-JIMF-final.csv"))
dat <- subset(dat, year %in% c('1970','1971','1972','1973','1974','1975','1976','1977','1978','1979','1980','1981','1982','1983','1984','1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018'))

dat_rg_episodes <- subset(dat, YNRGepisode %in% c('1'))

#change type of output gap variable
typeof(dat$outputgap)
dat$outputgap <- as.numeric(dat$outputgap)

#calculate real interest rates
dat$realLTrate <- dat$LTrate - dat$inflation

#invert finreform
dat$fin_repression <- 1/dat$finreform

#calculate public debt in dollar terms
dat$PDebt_absolute <- dat$PDebt * dat$GDP

#calculate fiscal space variable (ratio of government debt to total tax revenue)
dat$FiscalSpace <- dat$PDebt_absolute/dat$TaxRevenue/1000000

#calculate net foreign assets to GDP
dat$NetForeignAssets <- dat$Net_foreign_assets_World_Bank / dat$GDP_World_Bank*100

#calculate GDP size
dat$GDP_size <- dat$GDP_World_Bank / dat$World_GDP*100

#calculate spread to Germany
dat$spread_Germany <- dat$LTrate - dat$LTrate_Germany

#calculate spread to US
dat$spread_USA <- dat$LTrate - dat$LTrate_USA

#calculate sum of public debt
dat <- 
  dat %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(sum_PDebt = sum(PDebt_absolute, na.rm=TRUE)) %>%
  ungroup()

#size of debt in relation to OECD total
dat$debt_size <- dat$PDebt_absolute/dat$sum_PDebt*100

#rename variables according to introduction in the paper
dat$r<-dat$LTrate
dat$g<-dat$GDP_growth
dat$real_GDP_gr<-dat$real_GDP_growth
dat$real_LTrate<- dat$realLTrate
dat$safety<-dat$safe_Germany
dat$fin_open<-dat$ka_open
dat$NFA<-dat$NetForeignAssets
dat$EuroMember<-dat$EuroMembership
dat$EuroCrisis<-dat$EuroCrisis20082012
dat$StandaCrisis<-dat$Standalone20082012

#country sub-samples: Euro and standalone
dat_Euro <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_standalone <- subset(dat, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))

#Euro periphery vs. core
dat_Euro_periphery <- subset(dat, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_Euro_core <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'FIN', 'FRA', 'NLD'))

#descriptive statistics
#mean
mean(dat$rminusg, na.rm=TRUE)
mean(dat$rgNegative, na.rm=TRUE)

#median
median(dat$rminusg, na.rm=TRUE)
median(dat$rgNegative, na.rm=TRUE)

#minmax
min(dat$rminusg, na.rm=TRUE)
max(dat$rminusg, na.rm=TRUE)

#standard deviation
sd(dat$rminusg, na.rm=TRUE)
sd(dat$rgNegative, na.rm=TRUE)

#density plot
partialvector <- dat$rminusg
m<-mean(dat$rminusg, na.rm=TRUE)
std<-sqrt(var(dat$rminusg, na.rm=TRUE))

hist(partialvector,breaks = 30, freq=F,main="Distribution of interest-growth differentials (r-g)",xlab="interest-growth differential (r-g) in percentage points",ylab="Density", ylim=c(0,0.12), xlim=c(-30,30))
lines(density(partialvector, na.rm=TRUE), col="black", lwd=2) 
abline(v = 0.7917991, col="red", lwd=3, lty=2)
abline(v = 0.2400496, col="green", lwd=3, lty=2)
#+1 standard deviation
#abline(v = -4.296933, col="black", lwd=2)
#-1 standard deviation
#abline(v = 4.296933, col="black", lwd=2)
text(8, 0.1, "mean = 0.79", col="red")
text(-9, 0.1, "median = 0.24", col="green")
#curve(dnorm(x, mean=m, sd=std), 
#      col="darkblue", lwd=2, lty=2, add=TRUE, yaxt="n")
density(partialvector, na.rm=TRUE)

#There is very little deviation of the sample distribution from the theoretical bell curve distribution (dotted blue line).

#test for normal distribution
qqnorm(dat$rminusg, pch = 1, frame = FALSE, na.rm=TRUE)
qqline(dat$rminusg, col = "steelblue", lwd = 2, na.rm=TRUE)
qqPlot(dat$rminusg)

#Shapiro-Wilk's method 
#if p<0.1, we can reject the assumption of normality
shapiro.test(dat$rminusg)
#For larger samples (i.e. more than one hundred), the normality tests are overly conservative and the assumption of normality might be rejected too easily (see robust exceptions below).
#An issue with the Shapiro-Wilk's test is that when you feed it more data, the chances of the null hypothesis being rejected becomes larger.
#So what happens is that for large amounts of data even very small deviations from normality can be detected, leading to rejection of the null hypothesis event hough for practical purposes the data is more than normal enough.

#skewness and kurtosis
skew(dat$rminusg)
#skewness is a measure of symmetry. As a rule, positive skewness indicates that the mean of the data values is more than the median, and the data distribution is right-skewed.
#library(moments)
#kurtosis(dat$rminusg, na.rm=TRUE)
#the excess kurtosis describes the tail shape of the data distribution. The normal distribution has zero excess kurtosis and thus the standard tail shape. It is said to be mesokurtic. Negative excess kurtosis would indicate a thin-tailed data distribution, and is said to be platykurtic. Positive excess kurtosis would indicate a fat-tailed distribution, and is said to be leptokurtic.

#count r-g observations
count_rg_observations <- dat %>%
  group_by(ccode) %>% 
  summarise_at(vars(starts_with("rminusg")), ~sum(!is.na(.)))

#negative r-g episodes
number_negative_rg_observations <- dat %>%
  dplyr::group_by(ccode) %>% 
  dplyr::summarise(numberofnegativerg=sum(rgNegative==1, na.rm=TRUE))

#join data
join_rg_obs <- left_join(count_rg_observations, number_negative_rg_observations, by=c("ccode" = "ccode"))

#share of negative r-g episodes in total country observations
join_rg_obs$share_negative_rg <- join_rg_obs$numberofnegativerg/join_rg_obs$rminusg*100

#mean of share of negative r-g episodes
mean(join_rg_obs$share_negative_rg)

#plot
plot_rg_negative <- ggplot(join_rg_obs, aes(x = reorder(ccode, -share_negative_rg), y = share_negative_rg)) +
  geom_bar(stat = "identity") +
  ggtitle("")+
  theme(panel.background = element_rect(fill = "white")) +
  xlab("Country") +
  ylab("negative r-g episodes in % of all country observations") +
  geom_hline(yintercept=47.4, linetype="dashed", colour="black") +
  theme(axis.text.x=element_text(size=7))+
  theme(axis.title.x=element_text(size=11)) +
  theme(axis.text.y=element_text(size=11))+
  annotate("text", x=18, y=52, label = "mean = 47.4") +
  theme(axis.title.y=element_text(size=9))
plot_rg_negative

#Negative rg-spells and public-debt-to-GDP
#subset data
dat_negative_rg <- subset(dat, YNRGepisode %in% c('1'))

#plot negative r-g episodes against public-debt-to-GDP
library(mgcv)
plot_pdebt_rgspell <- ggplot(dat_negative_rg,aes(PDebt, lengthRGepisode))+
  geom_point(col="grey49")+
  xlab("Public debt in % of GDP") +
  ylab("Length of negative r-g episode (in years)") +
  ggtitle("")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white")) +
  geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
plot_pdebt_rgspell
#The regression line is based on a generalised additive model with local smoothing, and the smoothing parameter was selected by using the default method of cross- validation (Wood 2017)

#Question 2: Is the volatility of r-g driven more by changes in r or by changes in g?
#measuring volatility

#see descriptive plots for r and g

#standard deviation as the traditional measure of volatility (distribution is indeed close to a normal distribution, i.e. standard deviation should be a good measure of volatility)
sd(dat$rminusg, na.rm=TRUE)
sd(dat$rgNegative, na.rm=TRUE)

#Question 3
#What about the role of inflation?

dat_UR <- select(dat, ccode, year, rminusg)
dat_UR <- na.omit (dat_UR)
dat_UR <- pdata.frame(dat_UR)
dat_test <- select(dat, rminusg, PDebt, real_LTrate, inflation, pop_growth, primaryFB, fin_open, safety)
dat_test <- na.omit (dat_test)

#Unit root for r-g
dat_UR <- pdata.frame(dat, index = c("ccode", "year"))
cips <- plm::cipstest(dat_UR$rminusg, type = "drift")
cips

#Maddala-Wu test for cointegration
purtest(dat_test, pmax = 4, exo = "intercept", test = "madwu")

#Explaining variation in r-g
#with country-fixed effects
#rg differential as dependent variable
rg_determinants_v1 <- plm(rminusg~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v1)
coeftest(rg_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v2)
coeftest(rg_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
rg_determinants_v3 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v3)
coeftest(rg_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
rg_determinants_v4 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v4)
coeftest(rg_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
rg_determinants_v5 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v5)
coeftest(rg_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
rg_determinants_v6 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(rg_determinants_v6)
coeftest(rg_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery)
coeftest(rg_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(rg_determinants_v6_core)
coeftest(rg_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(rg_determinants_v7)
coeftest(rg_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v8)
coeftest(rg_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
dat_exclude_US_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(rg_determinants_v9)
coeftest(rg_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
dat_exclude_US <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(rg_determinants_v10)
coeftest(rg_determinants_v10, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
dat_exclude_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11 <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(rg_determinants_v11)
coeftest(rg_determinants_v11, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
dat_3_year <- dat %>%
  dplyr::arrange(ccode, year) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  dplyr::group_by(ccode, Period) %>%
  dplyr::summarise_all(mean)

fdata_3_year <- data.frame(dat_3_year)
pdata_3_year <- pdata.frame(x = fdata_3_year, index = c("ccode", "Period"))

rg_determinants_v5_3_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_3_year)
summary(rg_determinants_v5_3_year)
coeftest(rg_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.rg_determinants_v1 <- list(coeftest(rg_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v1 <- list(coeftest(rg_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v1 <- list(coeftest(rg_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v2 <- list(coeftest(rg_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2 <- list(coeftest(rg_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2 <- list(coeftest(rg_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3 <- list(coeftest(rg_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3 <- list(coeftest(rg_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3 <- list(coeftest(rg_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4 <- list(coeftest(rg_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4 <- list(coeftest(rg_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4 <- list(coeftest(rg_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5 <- list(coeftest(rg_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5 <- list(coeftest(rg_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5 <- list(coeftest(rg_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6 <- list(coeftest(rg_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6 <- list(coeftest(rg_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6 <- list(coeftest(rg_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery <- list(coeftest(rg_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery <- list(coeftest(rg_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery <- list(coeftest(rg_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core <- list(coeftest(rg_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core <- list(coeftest(rg_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core <- list(coeftest(rg_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7 <- list(coeftest(rg_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7 <- list(coeftest(rg_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7 <- list(coeftest(rg_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v8 <- list(coeftest(rg_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v8 <- list(coeftest(rg_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v8 <- list(coeftest(rg_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9 <- list(coeftest(rg_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9 <- list(coeftest(rg_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9 <- list(coeftest(rg_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_3_year <- list(coeftest(rg_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_3_year <- list(coeftest(rg_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_3_year <- list(coeftest(rg_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#Table 2
#stargazer table for LaTEX
stargazer(rg_determinants_v3, rg_determinants_v5, rg_determinants_v9, rg_determinants_v5_3_year, rg_determinants_v6, rg_determinants_v7, rg_determinants_v6_periphery, rg_determinants_v6_core, t=list(unlist(tvals.rg_determinants_v3), unlist(tvals.rg_determinants_v5), unlist(tvals.rg_determinants_v9), unlist(tvals.rg_determinants_v5_3_year), unlist(tvals.rg_determinants_v6), unlist(tvals.rg_determinants_v7), unlist(tvals.rg_determinants_v6_periphery), unlist(tvals.rg_determinants_v6_core)), se=list(unlist(ses.rg_determinants_v3), unlist(ses.rg_determinants_v5), unlist(ses.rg_determinants_v9), unlist(ses.rg_determinants_v5_3_year), unlist(ses.rg_determinants_v6), unlist(ses.rg_determinants_v7), unlist(ses.rg_determinants_v6_periphery), unlist(ses.rg_determinants_v6_core)), p=list(unlist(pvals.rg_determinants_v3), unlist(pvals.rg_determinants_v5), unlist(pvals.rg_determinants_v9), unlist(pvals.rg_determinants_v5_3_year), unlist(pvals.rg_determinants_v6), unlist(pvals.rg_determinants_v7), unlist(pvals.rg_determinants_v6_periphery), unlist(pvals.rg_determinants_v6_core)), type='html', out="output/Table2.htm")

#descriptives
descriptives <- select(dat, rminusg, PDebt, realLTrate, inflation, pop_growth, primaryFB, fin_open, safety, EuroMember, EuroCrisis, StandaCrisis)
stargazer(dat_3_year)

#5-year average
dat_5_year <- dat %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  group_by(ccode, Period) %>%
  summarise_all(mean)

dat_Euro_5_year <- dat_Euro %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  group_by(ccode, Period) %>%
  summarise_all(mean)

dat_Euro_periphery_5_year <- dat_Euro_periphery %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  group_by(ccode, Period) %>%
  summarise_all(mean)

dat_Euro_core_5_year <- dat_Euro_core %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  group_by(ccode, Period) %>%
  summarise_all(mean)

dat_standalone_5_year <- dat_standalone %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  group_by(ccode, Period) %>%
  summarise_all(mean)

fdata <- data.frame(dat_5_year)
pdata <- pdata.frame(x = fdata, index = c("ccode", "Period"))

fdata_Euro <- data.frame(dat_Euro_5_year)
pdata_Euro <- pdata.frame(x = fdata_Euro, index = c("ccode", "Period"))

fdata_standalone <- data.frame(dat_standalone_5_year)
pdata_standalone <- pdata.frame(x = fdata_standalone, index = c("ccode", "Period"))

fdata_Euro_periphery <- data.frame(dat_Euro_periphery_5_year)
pdata_Euro_periphery <- pdata.frame(x = fdata_Euro_periphery, index = c("ccode", "Period"))

fdata_Euro_core <- data.frame(dat_Euro_core_5_year)
pdata_Euro_core <- pdata.frame(x = fdata_Euro_core, index = c("ccode", "Period"))

#
rg_determinants_v1_5_year <- plm(rminusg~ inflation + real_LTrate + pop_growth, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v1_5_year)
coeftest(rg_determinants_v1_5_year, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v2_5_year)
coeftest(rg_determinants_v2_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
rg_determinants_v3_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety + fin_open, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v3_5_year)
coeftest(rg_determinants_v3_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
rg_determinants_v4_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + fin_open, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v4_5_year)
coeftest(rg_determinants_v4_5_year, vcov.=function(x) vcovHC(x, type="sss"))


#Euro
rg_determinants_v6_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_Euro)
summary(rg_determinants_v6_5_year)
coeftest(rg_determinants_v6_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_Euro_periphery)
summary(rg_determinants_v6_periphery_5_year)
coeftest(rg_determinants_v6_periphery_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_Euro_core)
summary(rg_determinants_v6_core_5_year)
coeftest(rg_determinants_v6_core_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_standalone)
summary(rg_determinants_v7_5_year)
coeftest(rg_determinants_v7_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v8_5_year)
coeftest(rg_determinants_v8_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
dat_exclude_US_DEU_5_year <- subset(pdata, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata)
summary(rg_determinants_v9_5_year)
coeftest(rg_determinants_v9_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#

#exclude US
dat_exclude_US_5_year <- subset(pdata, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "Period"), model="within", effect="individual", data=dat_exclude_US_5_year)
summary(rg_determinants_v10_5_year)
coeftest(rg_determinants_v10_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
dat_exclude_DEU_5_year <- subset(pdata, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11_5_year <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "Period"), model="within", effect="individual", data=dat_exclude_DEU_5_year)
summary(rg_determinants_v11_5_year)
coeftest(rg_determinants_v11_5_year, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.rg_determinants_v1_5_year <- list(coeftest(rg_determinants_v1_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v1_5_year <- list(coeftest(rg_determinants_v1_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v1_5_year <- list(coeftest(rg_determinants_v1_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v2_5_year <- list(coeftest(rg_determinants_v2_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2_5_year <- list(coeftest(rg_determinants_v2_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2_5_year <- list(coeftest(rg_determinants_v2_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3_5_year <- list(coeftest(rg_determinants_v3_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_5_year <- list(coeftest(rg_determinants_v3_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_5_year <- list(coeftest(rg_determinants_v3_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4_5_year <- list(coeftest(rg_determinants_v4_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4_5_year <- list(coeftest(rg_determinants_v4_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4_5_year <- list(coeftest(rg_determinants_v4_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_5_year <- list(coeftest(rg_determinants_v6_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_5_year <- list(coeftest(rg_determinants_v6_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_5_year <- list(coeftest(rg_determinants_v6_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_5_year <- list(coeftest(rg_determinants_v6_periphery_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_5_year <- list(coeftest(rg_determinants_v6_periphery_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_5_year <- list(coeftest(rg_determinants_v6_periphery_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_5_year <- list(coeftest(rg_determinants_v6_core_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_5_year <- list(coeftest(rg_determinants_v6_core_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_5_year <- list(coeftest(rg_determinants_v6_core_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_5_year <- list(coeftest(rg_determinants_v7_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_5_year <- list(coeftest(rg_determinants_v7_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_5_year <- list(coeftest(rg_determinants_v7_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v8_5_year <- list(coeftest(rg_determinants_v8_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v8_5_year <- list(coeftest(rg_determinants_v8_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v8_5_year <- list(coeftest(rg_determinants_v8_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_5_year <- list(coeftest(rg_determinants_v9_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_5_year <- list(coeftest(rg_determinants_v9_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_5_year <- list(coeftest(rg_determinants_v9_5_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX: main regression results
stargazer(rg_determinants_v3_5_year, rg_determinants_v6_5_year, rg_determinants_v7_5_year, rg_determinants_v6_periphery_5_year, rg_determinants_v6_core_5_year, rg_determinants_v9_5_year, t=list(unlist(tvals.rg_determinants_v3_5_year), unlist(tvals.rg_determinants_v6_5_year), unlist(tvals.rg_determinants_v7_5_year), unlist(tvals.rg_determinants_v6_periphery_5_year), unlist(tvals.rg_determinants_v6_core_5_year), unlist(tvals.rg_determinants_v9_5_year)), se=list(unlist(ses.rg_determinants_v3_5_year), unlist(ses.rg_determinants_v6_5_year), unlist(ses.rg_determinants_v7_5_year), unlist(ses.rg_determinants_v6_periphery_5_year), unlist(ses.rg_determinants_v6_core_5_year), unlist(ses.rg_determinants_v9_5_year)), p=list(unlist(pvals.rg_determinants_v3_5_year), unlist(pvals.rg_determinants_v6_5_year), unlist(pvals.rg_determinants_v7_5_year), unlist(pvals.rg_determinants_v6_periphery_5_year), unlist(pvals.rg_determinants_v6_core_5_year), unlist(pvals.rg_determinants_v9_5_year)))

#Question 4: What is the Probability of future 5-year r-g  > 0 in the future?
#positive average five-year ahead r-g
dat$rg_positive_3year <- ifelse(dat$rminusg_future3>0, 1, 0)
dat$rg_positive_5year <- ifelse(dat$rminusg_future5>0, 1, 0)
dat$rg_positive_10year <- ifelse(dat$rminusg_future10>0, 1, 0)

#euro sample
dat_Euro$rg_positive_3year <- ifelse(dat_Euro$rminusg_future3>0, 1, 0)
dat_Euro$rg_positive_5year <- ifelse(dat_Euro$rminusg_future5>0, 1, 0)
dat_Euro$rg_positive_10year <- ifelse(dat_Euro$rminusg_future10>0, 1, 0)

#euro periphery sample
dat_Euro_periphery$rg_positive_3year <- ifelse(dat_Euro_periphery$rminusg_future3>0, 1, 0)
dat_Euro_periphery$rg_positive_5year <- ifelse(dat_Euro_periphery$rminusg_future5>0, 1, 0)
dat_Euro_periphery$rg_positive_10year <- ifelse(dat_Euro_periphery$rminusg_future10>0, 1, 0)

#euro core sample
dat_Euro_core$rg_positive_3year <- ifelse(dat_Euro_core$rminusg_future3>0, 1, 0)
dat_Euro_core$rg_positive_5year <- ifelse(dat_Euro_core$rminusg_future5>0, 1, 0)
dat_Euro_core$rg_positive_10year <- ifelse(dat_Euro_core$rminusg_future10>0, 1, 0)

#standalone sample
dat_standalone$rg_positive_3year <- ifelse(dat_standalone$rminusg_future3>0, 1, 0)
dat_standalone$rg_positive_5year <- ifelse(dat_standalone$rminusg_future5>0, 1, 0)
dat_standalone$rg_positive_10year <- ifelse(dat_standalone$rminusg_future10>0, 1, 0)

#exclude Euro crisis
dat_exclude_EuroCrisis <- subset(dat_Euro, year %in% c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006', '2007'))

dat_exclude_EuroCrisis$rg_positive_3year <- ifelse(dat_exclude_EuroCrisis$rminusg_future3>0, 1, 0)
dat_exclude_EuroCrisis$rg_positive_5year <- ifelse(dat_exclude_EuroCrisis$rminusg_future5>0, 1, 0)
dat_exclude_EuroCrisis$rg_positive_10year <- ifelse(dat_exclude_EuroCrisis$rminusg_future10>0, 1, 0)

#quartiles based on the distribution of public-debt-to-GDP
dat$PDebt_quartile <- with(dat, cut(PDebt, 
                            breaks=quantile(PDebt, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                            include.lowest=TRUE, labels=c("Q1","Q2","Q3","Q4")))

#quartiles Euro sample
dat_Euro$PDebt_quartile <- with(dat_Euro, cut(PDebt, 
                              breaks=quantile(PDebt, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                              include.lowest=TRUE, labels=c("Q1","Q2","Q3","Q4")))

#quartiles standalone sample
dat_standalone$PDebt_quartile <- with(dat_standalone, cut(PDebt, 
                                        breaks=quantile(PDebt, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                        include.lowest=TRUE, labels=c("Q1","Q2","Q3","Q4")))

#subsample cut-off after 2007 (i.e. 1990-2007; 2008-2018 excluded)
dat_exclude_EuroCrisis <- subset(dat_Euro, year %in% c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006', '2007'))

#Graphs for predicted probabilities
#plot probability that r-g turns positive at mean public-debt-to-GDP ratio
#The predict() function can be used to predict the probability that r-g will be higher than 0, given values of the predictors
#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit (i.e. "response" estimates predicted probabilities based on fitted values). If no data set is supplied to the predict() function, then the probabilities are computed for the training data that was used to fit the logistic regression model.

#dat_mean for 2016-2018

dat_2009_2018 <- subset(dat, year %in% c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'))
dat_2016_2018 <- subset(dat, year %in% c('2016', '2017', '2018'))
dat_1990s <- subset(dat, year %in% c('1990','1991', '1992', '1993', '1994', '1995', '1996','1997','1998','1999'))


#calculate mean of public debt for each country
dat_2009_2018_mean_PDebt <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_pop_growth <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_inflation <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2009_2018_mean_safety <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2009_2018_mean_debt_size <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2009_2018_mean_GDP_size <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2009_2018_mean_primaryFB <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2009_2018_mean_real_GDP_gr <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2009_2018_mean_fin_open <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2009_2018_mean_NFA <- dat_2009_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_1990s_mean_PDebt <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_pop_growth <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_inflation <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_1990s_mean_safety <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_1990s_mean_debt_size <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_1990s_mean_GDP_size <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_1990s_mean_primaryFB <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_1990s_mean_real_GDP_gr <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_1990s_mean_fin_open <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_1990s_mean_NFA <- dat_1990s %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_2016_2018_mean_PDebt <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_pop_growth <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_inflation <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2016_2018_mean_safety <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2016_2018_mean_debt_size <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2016_2018_mean_GDP_size <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2016_2018_mean_primaryFB <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2016_2018_mean_real_GDP_gr <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2016_2018_mean_fin_open <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2016_2018_mean_NFA <- dat_2016_2018 %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#Euro
dat_2009_2018_Euro <- subset(dat_Euro, year %in% c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'))
dat_2016_2018_Euro <- subset(dat_Euro, year %in% c('2016', '2017', '2018'))
dat_1990s_Euro <- subset(dat_Euro, year %in% c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999'))

#calculate mean of public debt for each country
dat_2009_2018_mean_PDebt_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_pop_growth_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_inflation_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2009_2018_mean_safety_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2009_2018_mean_debt_size_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2009_2018_mean_GDP_size_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2009_2018_mean_primaryFB_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2009_2018_mean_real_GDP_gr_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2009_2018_mean_fin_open_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2009_2018_mean_NFA_Euro <- dat_2009_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_1990s_mean_PDebt_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_pop_growth_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_inflation_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_1990s_mean_safety_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_1990s_mean_debt_size_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_1990s_mean_GDP_size_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_1990s_mean_primaryFB_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_1990s_mean_real_GDP_gr_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_1990s_mean_fin_open_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_1990s_mean_NFA_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_2016_2018_mean_PDebt_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_pop_growth_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_inflation_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2016_2018_mean_safety_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2016_2018_mean_debt_size_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2016_2018_mean_GDP_size_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2016_2018_mean_primaryFB_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2016_2018_mean_real_GDP_gr_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2016_2018_mean_fin_open_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2016_2018_mean_NFA_Euro <- dat_2016_2018_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#standalone
dat_2009_2018_standalone <- subset(dat_standalone, year %in% c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'))
dat_2016_2018_standalone <- subset(dat_standalone, year %in% c('2016', '2017', '2018'))
dat_1990s_standalone <- subset(dat_standalone, year %in% c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999'))

#calculate mean of public debt for each country
dat_2009_2018_mean_PDebt_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_pop_growth_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2009_2018_mean_inflation_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2009_2018_mean_safety_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2009_2018_mean_debt_size_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2009_2018_mean_GDP_size_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2009_2018_mean_primaryFB_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2009_2018_mean_real_GDP_gr_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2009_2018_mean_real_LTrate_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2009_2018_mean_fin_open_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2009_2018_mean_NFA_standalone <- dat_2009_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_1990s_mean_PDebt_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_pop_growth_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_1990s_mean_inflation_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_1990s_mean_safety_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_1990s_mean_debt_size_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_1990s_mean_GDP_size_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_1990s_mean_primaryFB_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_1990s_mean_real_GDP_gr_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_1990s_mean_real_LTrate_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_1990s_mean_fin_open_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_1990s_mean_NFA_standalone <- dat_1990s_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_2016_2018_mean_PDebt_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_pop_growth_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_2016_2018_mean_inflation_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_2016_2018_mean_safety_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_2016_2018_mean_debt_size_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_2016_2018_mean_GDP_size_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_2016_2018_mean_primaryFB_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_2016_2018_mean_real_GDP_gr_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_2016_2018_mean_real_LTrate_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_2016_2018_mean_fin_open_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_2016_2018_mean_NFA_standalone <- dat_2016_2018_standalone %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of public debt for each country
dat_mean_PDebt <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_PDebt = mean(PDebt, na.rm=TRUE))

#calculate mean of population growth for each country
dat_mean_pop_growth <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_pop_growth = mean(pop_growth, na.rm=TRUE))

#calculate mean of population growth for each country
dat_mean_inflation <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_inflation = mean(inflation, na.rm=TRUE))

#calculate mean of safe for each country
dat_mean_safety <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_mean_real_LTrate <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of debt_size for each country
dat_mean_debt_size <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_debt_size = mean(debt_size, na.rm=TRUE))

#calculate mean of GDP_size for each country
dat_mean_GDP_size <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_GDP_size = mean(GDP_size, na.rm=TRUE))

#calculate mean of primaryFB for each country
dat_mean_primaryFB <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_primaryFB = mean(primaryFB, na.rm=TRUE))

#calculate mean of real_GDP_gr for each country
dat_mean_real_GDP_gr <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_GDP_gr = mean(real_GDP_gr, na.rm=TRUE))

#calculate mean of real_LTrate for each country
dat_mean_real_LTrate <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_real_LTrate = mean(real_LTrate, na.rm=TRUE))

#calculate mean of fin_open for each country
dat_mean_fin_open <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_fin_open = mean(fin_open, na.rm=TRUE))

#calculate mean of NFA for each country
dat_mean_NFA <- dat %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_NFA = mean(NFA, na.rm=TRUE))

#calculate mean of population growth for each public debt quartile
dat_mean_pop_growth_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_pop_growth_quartile = mean(pop_growth, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of safety for each public debt quartile
dat_mean_safety_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_safety_quartile = mean(safety, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_GDP_gr_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_GDP_gr_quartile = mean(real_GDP_gr, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_LTrate_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_LTrate_quartile = mean(real_LTrate, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of primaryFB for each public debt quartile
dat_mean_primaryFB_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_primaryFB_quartile = mean(primaryFB, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of fin_open for each public debt quartile
dat_mean_fin_open_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_fin_open_quartile = mean(fin_open, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of NFA for each public debt quartile
dat_mean_NFA_quartile <- dat %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_NFA_quartile = mean(NFA, na.rm=TRUE)) %>%
  na.omit()

#Euro Sample
#calculate mean of population growth for each public debt quartile
dat_mean_pop_growth_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_pop_growth_quartile = mean(pop_growth, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of safety for each public debt quartile
dat_mean_safety_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_safety_quartile = mean(safety, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_GDP_gr_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_GDP_gr_quartile = mean(real_GDP_gr, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_LTrate_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_LTrate_quartile = mean(real_LTrate, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of primaryFB for each public debt quartile
dat_mean_primaryFB_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_primaryFB_quartile = mean(primaryFB, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of fin_open for each public debt quartile
dat_mean_fin_open_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_fin_open_quartile = mean(fin_open, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of NFA for each public debt quartile
dat_mean_NFA_quartile_Euro <- dat_Euro %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_NFA_quartile = mean(NFA, na.rm=TRUE)) %>%
  na.omit()

#standalone Sample
#calculate mean of population growth for each public debt quartile
dat_mean_pop_growth_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_pop_growth_quartile = mean(pop_growth, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of safety for each public debt quartile
dat_mean_safety_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_safety_quartile = mean(safety, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_GDP_gr_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_GDP_gr_quartile = mean(real_GDP_gr, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_LTrate_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_LTrate_quartile = mean(real_LTrate, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of primaryFB for each public debt quartile
dat_mean_primaryFB_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_primaryFB_quartile = mean(primaryFB, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of fin_open for each public debt quartile
dat_mean_fin_open_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_fin_open_quartile = mean(fin_open, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of NFA for each public debt quartile
dat_mean_NFA_quartile_standalone <- dat_standalone %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_NFA_quartile = mean(NFA, na.rm=TRUE)) %>%
  na.omit()

#exclude_EuroCrisis Sample
#calculate mean of population growth for each public debt quartile
dat_mean_pop_growth_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_pop_growth_quartile = mean(pop_growth, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of safety for each public debt quartile
dat_mean_safety_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_safety_quartile = mean(safety, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_GDP_gr_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_GDP_gr_quartile = mean(real_GDP_gr, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of real_GDP_gr for each public debt quartile
dat_mean_real_LTrate_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_real_LTrate_quartile = mean(real_LTrate, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of primaryFB for each public debt quartile
dat_mean_primaryFB_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_primaryFB_quartile = mean(primaryFB, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of fin_open for each public debt quartile
dat_mean_fin_open_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_fin_open_quartile = mean(fin_open, na.rm=TRUE)) %>%
  na.omit()

#calculate mean of NFA for each public debt quartile
dat_mean_NFA_quartile_exclude_EuroCrisis <- dat_exclude_EuroCrisis %>%
  group_by(PDebt_quartile) %>%
  dplyr::summarize(Mean_NFA_quartile = mean(NFA, na.rm=TRUE)) %>%
  na.omit()

#Euro samples
dat_mean_PDebt_Euro <- subset(dat_mean_PDebt, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_pop_growth_Euro <- subset(dat_mean_pop_growth, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_safety_Euro <- subset(dat_mean_safety, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_primaryFB_Euro <- subset(dat_mean_primaryFB, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_real_GDP_gr_Euro <- subset(dat_mean_real_GDP_gr, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_real_LTrate_Euro <- subset(dat_mean_real_LTrate, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_fin_open_Euro <- subset(dat_mean_fin_open, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_NFA_Euro <- subset(dat_mean_NFA, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_inflation_Euro <- subset(dat_mean_inflation, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))


#standalone samples
dat_mean_PDebt_standalone <- subset(dat_mean_PDebt, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_pop_growth_standalone  <- subset(dat_mean_pop_growth, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_safety_standalone  <- subset(dat_mean_safety, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_real_LTrate_standalone <- subset(dat_mean_real_LTrate, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_primaryFB_standalone <- subset(dat_mean_primaryFB, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_real_GDP_gr_standalone<- subset(dat_mean_real_GDP_gr, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_real_fin_open_standalone <- subset(dat_mean_fin_open, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_real_NFA_standalone <- subset(dat_mean_NFA, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))
dat_mean_inflation_standalone <- subset(dat_mean_inflation, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_fin_open_standalone <- subset(dat_mean_fin_open, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))

#Euro samples
dat_mean_PDebt_exclude_EuroCrisis <- subset(dat_mean_PDebt, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_pop_growth_exclude_EuroCrisis <- subset(dat_mean_pop_growth, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_mean_safety_exclude_EuroCrisis <- subset(dat_mean_safety, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))

#with PDebt quartiles
#all countries
probit_allcountries <- glm(rg_positive_5year ~ real_GDP_gr + real_LTrate + pop_growth + PDebt_quartile + primaryFB + fin_open + safety*EuroMember+safety*EuroCrisis+safety*StandaCrisis, family=binomial(link="probit"), data=dat)
summary(probit_allcountries)

#Euro countries
probit_Euro<- glm(rg_positive_5year ~ real_GDP_gr + real_LTrate + pop_growth + PDebt_quartile + primaryFB + fin_open + safety*EuroCrisis, family=binomial(link="probit"), data=dat_Euro)
summary(probit_Euro)

#Standalone countries
probit_standalone <- glm(rg_positive_5year ~ real_GDP_gr + real_LTrate + pop_growth + PDebt_quartile + primaryFB + fin_open + safety*StandaCrisis, family=binomial(link="probit"), data=dat_standalone)
summary(probit_standalone)

#with PDebt as continuous variable
#all countries
probit_allcountries_PDebt <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat)
summary(probit_allcountries_PDebt)

probit_allcountries_PDebt_for_predictions <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat)
summary(probit_allcountries_PDebt_for_predictions)

#Euro countries
probit_Euro_PDebt<- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_Euro)
summary(probit_Euro_PDebt)

#Standalone countries
probit_standalone_PDebt <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_standalone)
summary(probit_standalone_PDebt)

#Euro periphery
probit_Euro_PDebt_periphery<- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_Euro_periphery)
summary(probit_Euro_PDebt_periphery)

#Euro core
probit_Euro_PDebt_core<- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_Euro_core)
summary(probit_Euro_PDebt_core)

#preparations for stargazer table
ses.probit_allcountries <- list(coeftest(probit_allcountries, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_allcountries <- list(coeftest(probit_allcountries, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_allcountries <- list(coeftest(probit_allcountries, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_Euro <- list(coeftest(probit_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_Euro <- list(coeftest(probit_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_Euro <- list(coeftest(probit_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_standalone <- list(coeftest(probit_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_standalone <- list(coeftest(probit_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_standalone <- list(coeftest(probit_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_allcountries_PDebt<- list(coeftest(probit_allcountries_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_allcountries_PDebt <- list(coeftest(probit_allcountries_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_allcountries_PDebt <- list(coeftest(probit_allcountries_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_Euro_PDebt <- list(coeftest(probit_Euro_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_Euro_PDebt <- list(coeftest(probit_Euro_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_Euro_PDebt <- list(coeftest(probit_Euro_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_Euro_PDebt_periphery <- list(coeftest(probit_Euro_PDebt_periphery, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_Euro_PDebt_periphery <- list(coeftest(probit_Euro_PDebt_periphery, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_Euro_PDebt_periphery <- list(coeftest(probit_Euro_PDebt_periphery, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_Euro_PDebt_core <- list(coeftest(probit_Euro_PDebt_core, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_Euro_PDebt_core <- list(coeftest(probit_Euro_PDebt_core, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_Euro_PDebt_core <- list(coeftest(probit_Euro_PDebt_core, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.probit_standalone_PDebt <- list(coeftest(probit_standalone_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.probit_standalone_PDebt <- list(coeftest(probit_standalone_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.probit_standalone_PDebt <- list(coeftest(probit_standalone_PDebt, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

#Table A8
#stargazer table
stargazer(probit_allcountries_PDebt, probit_Euro_PDebt, probit_standalone_PDebt, probit_Euro_PDebt_periphery, probit_Euro_PDebt_core, t=list(unlist(tvals.probit_allcountries_PDebt), unlist(tvals.probit_Euro_PDebt), unlist(tvals.probit_standalone_PDebt), unlist(tvals.probit_Euro_PDebt_periphery), unlist(tvals.probit_Euro_PDebt_core)), se=list(unlist(ses.probit_allcountries_PDebt), unlist(ses.probit_Euro_PDebt), unlist(ses.probit_standalone_PDebt), unlist(ses.probit_Euro_PDebt_periphery), unlist(ses.probit_Euro_PDebt_core)), p=list(unlist(pvals.probit_allcountries_PDebt), unlist(pvals.probit_Euro_PDebt), unlist(pvals.probit_standalone_PDebt), unlist(pvals.probit_Euro_PDebt_periphery), unlist(pvals.probit_Euro_PDebt_core)))

#stargazer(probit_allcountries_PDebt, probit_Euro_PDebt, probit_standalone_PDebt, probit_allcountries, probit_Euro, probit_standalone, t=list(unlist(tvals.probit_allcountries_PDebt), unlist(tvals.probit_Euro_PDebt), unlist(tvals.probit_standalone_PDebt), unlist(tvals.probit_allcountries), unlist(tvals.probit_Euro), unlist(tvals.probit_standalone)), se=list(unlist(ses.probit_allcountries_PDebt), unlist(ses.probit_Euro_PDebt), unlist(ses.probit_standalone_PDebt), unlist(ses.probit_allcountries), unlist(ses.probit_Euro), unlist(ses.probit_standalone)), p=list(unlist(pvals.probit_allcountries_PDebt), unlist(pvals.probit_Euro_PDebt), unlist(pvals.probit_standalone_PDebt), unlist(pvals.probit_allcountries), unlist(pvals.probit_Euro), unlist(pvals.probit_standalone)))

#setting initial conditions for estimating predicted probabilities
allmean_allcountries <- data.frame(EuroMember=rep(1,4), EuroCrisis=rep(1,4), StandaCrisis=rep(1,4), real_GDP_gr=dat_mean_real_GDP_gr_quartile$Mean_real_GDP_gr_quartile, pop_growth=dat_mean_pop_growth_quartile$Mean_pop_growth_quartile, primaryFB=dat_mean_primaryFB_quartile$Mean_primaryFB_quartile, fin_open=dat_mean_fin_open_quartile$Mean_fin_open_quartile, NFA=dat_mean_NFA_quartile$Mean_NFA_quartile, real_LTrate=dat_mean_real_LTrate_quartile$Mean_real_LTrate_quartile, safety=dat_mean_safety_quartile$Mean_safety_quartile, PDebt_quartile=as.factor(c("Q1","Q2","Q3","Q4")))

#bind data frame with predicted probabilities and standard errors
allmean_probit_allcountries <- cbind(allmean_allcountries, predict(probit_allcountries, newdata=allmean_allcountries, type="response", se.fit=TRUE))

#plot allcountries
#probit
boxplot_probit_allcountries <- ggplot(allmean_probit_allcountries,aes(x=PDebt_quartile, y=fit))+
  geom_point() +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                position=position_dodge(.9), col="red")+
  xlab("Public-debt-to-GDP quartile") +
  ylab("Probability of 5-year future r-g  > 0") +
  ggtitle("Probabilities of 5-year future r - g > 0\n in different public debt groups (22 OECD countries)")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white"))
boxplot_probit_allcountries

#Euro
#setting initial conditions for estimating predicted probabilities
allmean_Euro <- data.frame(EuroMember=rep(1,4), EuroCrisis=rep(1,4), StandaCrisis=rep(1,4), real_GDP_gr=dat_mean_real_GDP_gr_quartile$Mean_real_GDP_gr_quartile, pop_growth=dat_mean_pop_growth_quartile$Mean_pop_growth_quartile, primaryFB=dat_mean_primaryFB_quartile$Mean_primaryFB_quartile, fin_open=dat_mean_fin_open_quartile$Mean_fin_open_quartile, NFA=dat_mean_NFA_quartile$Mean_NFA_quartile, real_LTrate=dat_mean_real_LTrate_quartile$Mean_real_LTrate_quartile, safety=dat_mean_safety_quartile$Mean_safety_quartile, PDebt_quartile=as.factor(c("Q1","Q2","Q3","Q4")))

#bind data frame with predicted probabilities and standard errors
allmean_probit_Euro <- cbind(allmean_Euro, predict(probit_Euro, newdata=allmean_Euro, type="response", se.fit=TRUE))

#single-country predictions
dat_2018<-subset(dat, year %in% c('2018'))

#subset data with 2018 levels
allmean_all_2018_levels <- data.frame(PDebt=dat_2018$PDebt, inflation=dat_2018$inflation, pop_growth=dat_2018$pop_growth, safety=dat_2018$safety, real_LTrate=dat_2018$real_LTrate, real_GDP_gr=dat_2018$real_GDP_gr, fin_open=dat_2018$fin_open, primaryFB=dat_2018$primaryFB, NFA=c(-25.1748252, 24.1758242, 22.0994475, 0.7259370, 72.70115, 43.6868687, 180.1120448, -8.2394366, 27.9152951, 13.2258065, 13.8461538, 21.6960081, 42.2279793, -10.5263158, 1484.8484848, 24302.3255814, 23.8512035, -247.1395881, -37.7143014, -13.3702875, 130.2702703, -0.3908593), ccode=as.factor(unique(dat$ccode)), EuroMember=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), EuroCrisis=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), StandaCrisis=c(1,0,0,1,1,0,1,0,0,0,1,0,0,0,1,1,0,1,1,0,1,1))
#subset data with 2009-2018 mean
allmean_all_2009_2018_levels <- data.frame(PDebt=dat_2009_2018_mean_PDebt$Mean_PDebt, inflation=dat_2009_2018_mean_inflation$Mean_inflation, pop_growth=dat_2009_2018_mean_pop_growth$Mean_pop_growth, safety=dat_2009_2018_mean_safety$Mean_safety, real_LTrate=dat_2009_2018_mean_real_LTrate$Mean_real_LTrate, fin_open=dat_2009_2018_mean_fin_open$Mean_fin_open, primaryFB=dat_2009_2018_mean_primaryFB$Mean_primaryFB, NFA=c(-25.7584065, 35.4170205, 24.0135113, -0.99442479, 59.63333638, 39.06364031, 74.42953109, -5.15524912, 16.09940412, 20.05018642, -0.06045097, 4.53513968, 29.33113581, -2.05448548, 639.87511745, 8310.86935398, 16.65973384, -30.55840644, -18.73772063, -15.15863748, 24.36126539, 1.01516812), ccode=as.factor(unique(dat$ccode)), EuroMember=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), EuroCrisis=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), StandaCrisis=c(1,0,0,1,1,0,1,0,0,0,1,0,0,0,1,1,0,1,1,0,1,1))
#subset data with 2009-2018 mean
allmean_all_2016_2018_levels <- data.frame(PDebt=dat_2016_2018_mean_PDebt$Mean_PDebt, inflation=dat_2016_2018_mean_inflation$Mean_inflation, pop_growth=dat_2016_2018_mean_pop_growth$Mean_pop_growth, safety=dat_2016_2018_mean_safety$Mean_safety, real_LTrate=dat_2016_2018_mean_real_LTrate$Mean_real_LTrate, fin_open=dat_2016_2018_mean_fin_open$Mean_fin_open, primaryFB=dat_2016_2018_mean_primaryFB$Mean_primaryFB, NFA=c(-25.7584065, 35.4170205, 24.0135113, -0.99442479, 59.63333638, 39.06364031, 74.42953109, -5.15524912, 16.09940412, 20.05018642, -0.06045097, 4.53513968, 29.33113581, -2.05448548, 639.87511745, 8310.86935398, 16.65973384, -30.55840644, -18.73772063, -15.15863748, 24.36126539, 1.01516812), ccode=as.factor(unique(dat$ccode)), EuroMember=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), EuroCrisis=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), StandaCrisis=c(1,0,0,1,1,0,1,0,0,0,1,0,0,0,1,1,0,1,1,0,1,1))
#subset data with 1990s mean
allmean_all_1990s_levels <- data.frame(PDebt=dat_1990s_mean_PDebt$Mean_PDebt, inflation=dat_1990s_mean_inflation$Mean_inflation, pop_growth=dat_1990s_mean_pop_growth$Mean_pop_growth, safety=dat_1990s_mean_safety$Mean_safety, real_LTrate=dat_1990s_mean_real_LTrate$Mean_real_LTrate, fin_open=dat_1990s_mean_fin_open$Mean_fin_open, primaryFB=dat_1990s_mean_primaryFB$Mean_primaryFB, NFA=c(-25.7584065, 35.4170205, 24.0135113, -0.99442479, 59.63333638, 39.06364031, 74.42953109, -5.15524912, 16.09940412, 20.05018642, -0.06045097, 4.53513968, 29.33113581, -2.05448548, 639.87511745, 8310.86935398, 16.65973384, -30.55840644, -18.73772063, -15.15863748, 24.36126539, 1.01516812), ccode=as.factor(unique(dat$ccode)), EuroMember=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), EuroCrisis=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), StandaCrisis=c(1,0,0,1,1,0,1,0,0,0,1,0,0,0,1,1,0,1,1,0,1,1))

#mean values
allmean_all_mean_values <- data.frame(PDebt=dat_mean_PDebt$Mean_PDebt, inflation=dat_mean_inflation$Mean_inflation, pop_growth=dat_mean_pop_growth$Mean_pop_growth, safety=dat_mean_safety$Mean_safety, real_LTrate=dat_mean_real_LTrate$Mean_real_LTrate, real_GDP_gr=dat_mean_real_GDP_gr$Mean_real_GDP_gr, fin_open=dat_mean_fin_open$Mean_fin_open, primaryFB=dat_mean_primaryFB$Mean_primaryFB, NFA=dat_mean_NFA$Mean_NFA, ccode=as.factor(unique(dat$ccode)), EuroMember=c(0,1,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0), EuroCrisis=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), StandaCrisis=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
#bind data frame with predicted probabilities and standard errors - 2018 levels
allmean_probit_all_2018_levels <- cbind(allmean_all_2018_levels, predict(probit_allcountries_PDebt_for_predictions, newdata=allmean_all_2018_levels, type="response", se.fit=TRUE))

#probit regression model
probit_allcountries_PDebt_for_predictions <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat)
#bind data frame with predicted probabilities and standard errors - 
allmean_probit_all_mean_levels <- cbind(allmean_all_mean_values, predict(probit_allcountries_PDebt_for_predictions, newdata=allmean_all_mean_values, type="response", se.fit=TRUE))
allmean_probit_all_2009_2018_levels <- cbind(allmean_all_2009_2018_levels, predict(probit_allcountries_PDebt_for_predictions, newdata=allmean_all_2009_2018_levels, type="response", se.fit=TRUE))
allmean_probit_all_2016_2018_levels <- cbind(allmean_all_2016_2018_levels, predict(probit_allcountries_PDebt_for_predictions, newdata=allmean_all_2016_2018_levels, type="response", se.fit=TRUE))

#allmean_logit_all$fit2 <- factor(allmean_logit_all$fit, levels = rev(levels(factor(allmean_logit_all$fit))))
#reorder
allmean_probit_all_2018_levels$ccode = factor(allmean_probit_all_2018_levels$ccode, levels=allmean_probit_all_2018_levels[order(-allmean_probit_all_2018_levels$fit), "ccode"])
allmean_probit_all_mean_levels$ccode = factor(allmean_probit_all_mean_levels$ccode, levels=allmean_probit_all_mean_levels[order(-allmean_probit_all_mean_levels$fit), "ccode"])
allmean_probit_all_2009_2018_levels$ccode = factor(allmean_probit_all_2009_2018_levels$ccode, levels=allmean_probit_all_2009_2018_levels[order(-allmean_probit_all_2009_2018_levels$fit), "ccode"])
allmean_probit_all_2016_2018_levels$ccode = factor(allmean_probit_all_2016_2018_levels$ccode, levels=allmean_probit_all_2016_2018_levels[order(-allmean_probit_all_2016_2018_levels$fit), "ccode"])

#Euro
dat_2018_Euro<-subset(dat_Euro, year %in% c('2018'))

#subset data with 2018 levels
allmean_all_2018_levels_Euro <- data.frame(PDebt=dat_2018_Euro$PDebt, inflation=dat_2018_Euro$inflation, pop_growth=dat_2018_Euro$pop_growth, safety=dat_2018_Euro$safety, real_LTrate=dat_2018_Euro$real_LTrate, real_GDP_gr=dat_2018_Euro$real_GDP_gr, fin_open=dat_2018_Euro$fin_open, primaryFB=dat_2018_Euro$primaryFB, NFA=dat_2018_Euro$NFA, ccode=as.factor(unique(dat_Euro$ccode)))
#subset data with 2009-2018 mean
allmean_all_2009_2018_levels_Euro <- data.frame(PDebt=dat_2009_2018_mean_PDebt_Euro$Mean_PDebt, inflation=dat_2009_2018_mean_inflation_Euro$Mean_inflation, pop_growth=dat_2009_2018_mean_pop_growth_Euro$Mean_pop_growth, safety=dat_2009_2018_mean_safety_Euro$Mean_safety, real_LTrate=dat_2009_2018_mean_real_LTrate_Euro$Mean_real_LTrate, fin_open=dat_2009_2018_mean_fin_open_Euro$Mean_fin_open, primaryFB=dat_2009_2018_mean_primaryFB_Euro$Mean_primaryFB, NFA=dat_2009_2018_mean_NFA_Euro$Mean_NFA, ccode=as.factor(unique(dat_Euro$ccode)))
#subset data with 2009-2018 mean
allmean_all_2016_2018_levels_Euro <- data.frame(PDebt=dat_2016_2018_mean_PDebt_Euro$Mean_PDebt, inflation=dat_2016_2018_mean_inflation_Euro$Mean_inflation, pop_growth=dat_2016_2018_mean_pop_growth_Euro$Mean_pop_growth, safety=dat_2016_2018_mean_safety_Euro$Mean_safety, real_LTrate=dat_2016_2018_mean_real_LTrate_Euro$Mean_real_LTrate, fin_open=dat_2016_2018_mean_fin_open_Euro$Mean_fin_open, primaryFB=dat_2016_2018_mean_primaryFB_Euro$Mean_primaryFB, NFA=dat_2016_2018_mean_NFA_Euro$Mean_NFA, ccode=as.factor(unique(dat_Euro$ccode)))
#mean values
allmean_all_mean_values_Euro <- data.frame(PDebt=dat_mean_PDebt_Euro$Mean_PDebt, inflation=dat_mean_inflation_Euro$Mean_inflation, pop_growth=dat_mean_pop_growth_Euro$Mean_pop_growth, safety=dat_mean_safety_Euro$Mean_safety, real_LTrate=dat_mean_real_LTrate_Euro$Mean_real_LTrate, real_GDP_gr=dat_mean_real_GDP_gr_Euro$Mean_real_GDP_gr, fin_open=dat_mean_fin_open_Euro$Mean_fin_open, primaryFB=dat_mean_primaryFB_Euro$Mean_primaryFB, NFA=dat_mean_NFA_Euro$Mean_NFA, ccode=as.factor(unique(dat_Euro$ccode)))
#subset data with 2009-2018 mean
allmean_all_1990s_levels_Euro <- data.frame(PDebt=dat_1990s_mean_PDebt_Euro$Mean_PDebt, inflation=dat_1990s_mean_inflation_Euro$Mean_inflation, pop_growth=dat_1990s_mean_pop_growth_Euro$Mean_pop_growth, safety=dat_1990s_mean_safety_Euro<-c(0.9948077, 0.3961595, 1.0000000, 0.7363295, 0.8056146, 0.4541528, -0.06402603, 0.4963542, 0.8824311, 0.8170807, 0.9729007), real_LTrate=dat_1990s_mean_real_LTrate_Euro$Mean_real_LTrate, fin_open=dat_1990s_mean_fin_open_Euro$Mean_fin_open, primaryFB=dat_1990s_mean_primaryFB_Euro$Mean_primaryFB, NFA=dat_1990s_mean_NFA_Euro$Mean_NFA, ccode=as.factor(unique(dat_Euro$ccode)))

#probit model
probit_allcountries_PDebt_for_predictions_Euro <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_Euro)

#bind data frame with predicted probabilities and standard errors - 2018 levels
allmean_probit_all_2018_levels_Euro <- cbind(allmean_all_2018_levels_Euro, predict(probit_allcountries_PDebt_for_predictions_Euro, newdata=allmean_all_2018_levels_Euro, type="response", se.fit=TRUE))

#bind data frame with predicted probabilities and standard errors - 
allmean_probit_all_mean_levels_Euro <- cbind(allmean_all_mean_values_Euro, predict(probit_allcountries_PDebt_for_predictions_Euro, newdata=allmean_all_mean_values_Euro, type="response", se.fit=TRUE))
allmean_probit_all_2009_2018_levels_Euro <- cbind(allmean_all_2009_2018_levels_Euro, predict(probit_allcountries_PDebt_for_predictions_Euro, newdata=allmean_all_2009_2018_levels_Euro, type="response", se.fit=TRUE))
allmean_probit_all_2016_2018_levels_Euro <- cbind(allmean_all_2016_2018_levels_Euro, predict(probit_allcountries_PDebt_for_predictions_Euro, newdata=allmean_all_2016_2018_levels_Euro, type="response", se.fit=TRUE))
allmean_probit_all_1990s_levels_Euro <- cbind(allmean_all_1990s_levels_Euro, predict(probit_allcountries_PDebt_for_predictions_Euro, newdata=allmean_all_1990s_levels_Euro, type="response", se.fit=TRUE))

#Euro periphery
dat_2018_Euro_periphery<-subset(dat_Euro_periphery, year %in% c('2018'))
#unique(dat_2018_Euro_periphery$ccode)
dat_2009_2018_mean_PDebt_Euro_periphery <- subset(dat_2009_2018_mean_PDebt_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_real_LTrate_Euro_periphery <- subset(dat_2009_2018_mean_real_LTrate_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_inflation_Euro_periphery <- subset(dat_2009_2018_mean_inflation_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_pop_growth_Euro_periphery <- subset(dat_2009_2018_mean_pop_growth_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_primaryFB_Euro_periphery <- subset(dat_2009_2018_mean_primaryFB_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_fin_open_Euro_periphery <- subset(dat_2009_2018_mean_fin_open_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_NFA_Euro_periphery <- subset(dat_2009_2018_mean_NFA_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2009_2018_mean_safety_Euro_periphery <- subset(dat_2009_2018_mean_safety_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_PDebt_Euro_periphery <- subset(dat_2016_2018_mean_PDebt_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_real_LTrate_Euro_periphery <- subset(dat_2016_2018_mean_real_LTrate_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_inflation_Euro_periphery <- subset(dat_2016_2018_mean_inflation_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_pop_growth_Euro_periphery <- subset(dat_2016_2018_mean_pop_growth_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_primaryFB_Euro_periphery <- subset(dat_2016_2018_mean_primaryFB_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_fin_open_Euro_periphery <- subset(dat_2016_2018_mean_fin_open_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_NFA_Euro_periphery <- subset(dat_2016_2018_mean_NFA_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_2016_2018_mean_safety_Euro_periphery <- subset(dat_2016_2018_mean_safety_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_PDebt_Euro_periphery <- subset(dat_1990s_mean_PDebt_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_real_LTrate_Euro_periphery <- subset(dat_1990s_mean_real_LTrate_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_inflation_Euro_periphery <- subset(dat_1990s_mean_inflation_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_pop_growth_Euro_periphery <- subset(dat_1990s_mean_pop_growth_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_primaryFB_Euro_periphery <- subset(dat_1990s_mean_primaryFB_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_fin_open_Euro_periphery <- subset(dat_1990s_mean_fin_open_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_NFA_Euro_periphery <- subset(dat_1990s_mean_NFA_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_1990s_mean_safety_Euro <- dat_1990s_Euro %>%
  group_by(ccode) %>%
  dplyr::summarize(Mean_safety = mean(safety, na.rm=TRUE))
dat_1990s_mean_safety_Euro_periphery <- subset(dat_1990s_mean_safety_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_PDebt_Euro_periphery <- subset(dat_mean_PDebt_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_real_LTrate_Euro_periphery <- subset(dat_mean_real_LTrate_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_inflation_Euro_periphery <- subset(dat_mean_inflation_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_pop_growth_Euro_periphery <- subset(dat_mean_pop_growth_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_primaryFB_Euro_periphery <- subset(dat_mean_primaryFB_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_fin_open_Euro_periphery <- subset(dat_mean_fin_open_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_NFA_Euro_periphery <- subset(dat_mean_NFA_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_mean_safety_Euro_periphery <- subset(dat_mean_safety_Euro, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))

#subset data with 2018 levels
allmean_all_2018_levels_Euro_periphery <- data.frame(PDebt=dat_2018_Euro_periphery$PDebt, inflation=dat_2018_Euro_periphery$inflation, pop_growth=dat_2018_Euro_periphery$pop_growth, safety=dat_2018_Euro_periphery$safety, real_LTrate=dat_2018_Euro_periphery$real_LTrate, real_GDP_gr=dat_2018_Euro_periphery$real_GDP_gr, fin_open=dat_2018_Euro_periphery$fin_open, primaryFB=dat_2018_Euro_periphery$primaryFB, NFA=dat_2018_Euro_periphery$NFA, ccode=as.factor(unique(dat_Euro_periphery$ccode)))
#subset data with 2009-2018 mean
allmean_all_2009_2018_levels_Euro_periphery <- data.frame(PDebt=dat_2009_2018_mean_PDebt_Euro_periphery$Mean_PDebt, inflation=dat_2009_2018_mean_inflation_Euro_periphery$Mean_inflation, pop_growth=dat_2009_2018_mean_pop_growth_Euro_periphery$Mean_pop_growth, safety=dat_2009_2018_mean_safety_Euro_periphery$Mean_safety, real_LTrate=dat_2009_2018_mean_real_LTrate_Euro_periphery$Mean_real_LTrate, fin_open=dat_2009_2018_mean_fin_open_Euro_periphery$Mean_fin_open, primaryFB=dat_2009_2018_mean_primaryFB_Euro_periphery$Mean_primaryFB, NFA=dat_2009_2018_mean_NFA_Euro_periphery$Mean_NFA, ccode=as.factor(unique(dat_Euro_periphery$ccode)))
#subset data with 2009-2018 mean
allmean_all_2016_2018_levels_Euro_periphery <- data.frame(PDebt=dat_2016_2018_mean_PDebt_Euro_periphery$Mean_PDebt, inflation=dat_2016_2018_mean_inflation_Euro_periphery$Mean_inflation, pop_growth=dat_2016_2018_mean_pop_growth_Euro_periphery$Mean_pop_growth, safety=dat_2016_2018_mean_safety_Euro_periphery$Mean_safety, real_LTrate=dat_2016_2018_mean_real_LTrate_Euro_periphery$Mean_real_LTrate, fin_open=dat_2016_2018_mean_fin_open_Euro_periphery$Mean_fin_open, primaryFB=dat_2016_2018_mean_primaryFB_Euro_periphery$Mean_primaryFB, NFA=dat_2016_2018_mean_NFA_Euro_periphery$Mean_NFA, ccode=as.factor(unique(dat_Euro_periphery$ccode)))
#subset data with 2009-2018 mean
allmean_all_1990s_levels_Euro_periphery <- data.frame(PDebt=dat_1990s_mean_PDebt_Euro_periphery$Mean_PDebt, inflation=dat_1990s_mean_inflation_Euro_periphery$Mean_inflation, pop_growth=dat_1990s_mean_pop_growth_Euro_periphery$Mean_pop_growth, safety=dat_1990s_mean_safety_Euro_periphery$Mean_safety, real_LTrate=dat_1990s_mean_real_LTrate_Euro_periphery$Mean_real_LTrate, fin_open=dat_1990s_mean_fin_open_Euro_periphery$Mean_fin_open, primaryFB=dat_1990s_mean_primaryFB_Euro_periphery$Mean_primaryFB, NFA=dat_1990s_mean_NFA_Euro_periphery$Mean_NFA, ccode=as.factor(unique(dat_Euro_periphery$ccode)))
#mean values
allmean_all_mean_values_Euro_periphery <- data.frame(PDebt=dat_mean_PDebt_Euro_periphery$Mean_PDebt, inflation=dat_mean_inflation_Euro_periphery$Mean_inflation, pop_growth=dat_mean_pop_growth_Euro_periphery$Mean_pop_growth, safety=dat_mean_safety_Euro_periphery$Mean_safety, real_LTrate=dat_mean_real_LTrate_Euro_periphery$Mean_real_LTrate, fin_open=dat_mean_fin_open_Euro_periphery$Mean_fin_open, primaryFB=dat_mean_primaryFB_Euro_periphery$Mean_primaryFB, NFA=dat_mean_NFA_Euro_periphery$Mean_NFA, ccode=as.factor(unique(dat_Euro_periphery$ccode)))

#probit model
probit_allcountries_PDebt_for_predictions_Euro_periphery <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_Euro_periphery)

#bind data frame with predicted probabilities and standard errors - 2018 levels
allmean_probit_all_2018_levels_Euro_periphery <- cbind(allmean_all_2018_levels_Euro_periphery, predict(probit_allcountries_PDebt_for_predictions_Euro_periphery, newdata=allmean_all_2018_levels_Euro_periphery, type="response", se.fit=TRUE))

#bind data frame with predicted probabilities and standard errors - 
allmean_probit_all_mean_levels_Euro_periphery <- cbind(allmean_all_mean_values_Euro_periphery, predict(probit_allcountries_PDebt_for_predictions_Euro_periphery, newdata=allmean_all_mean_values_Euro_periphery, type="response", se.fit=TRUE))
allmean_probit_all_2009_2018_levels_Euro_periphery <- cbind(allmean_all_2009_2018_levels_Euro_periphery, predict(probit_allcountries_PDebt_for_predictions_Euro_periphery, newdata=allmean_all_2009_2018_levels_Euro_periphery, type="response", se.fit=TRUE))
allmean_probit_all_2016_2018_levels_Euro_periphery <- cbind(allmean_all_2016_2018_levels_Euro_periphery, predict(probit_allcountries_PDebt_for_predictions_Euro_periphery, newdata=allmean_all_2016_2018_levels_Euro_periphery, type="response", se.fit=TRUE))
allmean_probit_all_1990s_levels_Euro_periphery <- cbind(allmean_all_1990s_levels_Euro_periphery, predict(probit_allcountries_PDebt_for_predictions_Euro_periphery, newdata=allmean_all_1990s_levels_Euro_periphery, type="response", se.fit=TRUE))

#standalone
dat_2018_standalone<-subset(dat_standalone, year %in% c('2018'))

#subset data with 2018 levels
allmean_all_2018_levels_standalone <- data.frame(PDebt=dat_2018_standalone$PDebt, inflation=dat_2018_standalone$inflation, pop_growth=dat_2018_standalone$pop_growth, safety=dat_2018_standalone$safety, real_LTrate=dat_2018_standalone$real_LTrate, real_GDP_gr=dat_2018_standalone$real_GDP_gr, fin_open=dat_2018_standalone$fin_open, primaryFB=dat_2018_standalone$primaryFB, NFA=dat_2018_standalone$NFA, ccode=as.factor(unique(dat_standalone$ccode)))
#subset data with 2009-2018 mean
allmean_all_2009_2018_levels_standalone <- data.frame(PDebt=dat_2009_2018_mean_PDebt_standalone$Mean_PDebt, inflation=dat_2009_2018_mean_inflation_standalone$Mean_inflation, pop_growth=dat_2009_2018_mean_pop_growth_standalone$Mean_pop_growth, safety=dat_2009_2018_mean_safety_standalone$Mean_safety, real_LTrate=dat_2009_2018_mean_real_LTrate_standalone$Mean_real_LTrate, fin_open=dat_2009_2018_mean_fin_open_standalone$Mean_fin_open, primaryFB=dat_2009_2018_mean_primaryFB_standalone$Mean_primaryFB, NFA=c(-25.7584065, 0.7259370, 50.3836005, 134.5705110, 7.9551025, 1509.2464315, 21877.0286174, -209.4921429, -46.4867051, 181.0657983, -0.6641431), ccode=as.factor(unique(dat_standalone$ccode)))
#subset data with 2009-2018 mean
allmean_all_2016_2018_levels_standalone <- data.frame(PDebt=dat_2016_2018_mean_PDebt_standalone$Mean_PDebt, inflation=dat_2016_2018_mean_inflation_standalone$Mean_inflation, pop_growth=dat_2016_2018_mean_pop_growth_standalone$Mean_pop_growth, safety=dat_2016_2018_mean_safety_standalone$Mean_safety, real_LTrate=dat_2016_2018_mean_real_LTrate_standalone$Mean_real_LTrate, fin_open=dat_2016_2018_mean_fin_open_standalone$Mean_fin_open, primaryFB=dat_2016_2018_mean_primaryFB_standalone$Mean_primaryFB, NFA=c(-25.167018, 0.7259370, 72.701149, 208.848247, 14.312630, 1501.556229, 25177.318404, -241.674756, -38.909089, 204.599731, -1.254506), ccode=as.factor(unique(dat_standalone$ccode)))
#mean values
allmean_all_mean_values_standalone <- data.frame(PDebt=dat_mean_PDebt_standalone$Mean_PDebt, inflation=dat_mean_inflation_standalone$Mean_inflation, pop_growth=dat_mean_pop_growth_standalone$Mean_pop_growth, safety=dat_mean_safety_standalone$Mean_safety, real_LTrate=dat_mean_real_LTrate_standalone$Mean_real_LTrate, real_GDP_gr=dat_mean_real_GDP_gr_standalone$Mean_real_GDP_gr, fin_open=dat_mean_fin_open_standalone$Mean_fin_open, primaryFB=dat_mean_primaryFB_standalone$Mean_primaryFB, NFA=c(-25.1748252, 0.7259370, 72.70115, 180.1120448, 13.8461538, 1484.8484848, 24302.3255814, -247.1395881, -37.7143014, 130.2702703, -0.3908593), ccode=as.factor(unique(dat_standalone$ccode)))

allmean_all_2016_2018_levels_standalone
#probit model
probit_allcountries_PDebt_for_predictions_standalone <- glm(rg_positive_3year ~ inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, family=binomial(link="probit"), data=dat_standalone)

#bind data frame with predicted probabilities and standard errors - 2018 levels
allmean_probit_all_2018_levels_standalone <- cbind(allmean_all_2018_levels_standalone, predict(probit_allcountries_PDebt_for_predictions_standalone, newdata=allmean_all_2018_levels_standalone, type="response", se.fit=TRUE))

#bind data frame with predicted probabilities and standard errors - 
allmean_probit_all_mean_levels_standalone <- cbind(allmean_all_mean_values_standalone, predict(probit_allcountries_PDebt_for_predictions_standalone, newdata=allmean_all_mean_values_standalone, type="response", se.fit=TRUE))
allmean_probit_all_2009_2018_levels_standalone <- cbind(allmean_all_2009_2018_levels_standalone, predict(probit_allcountries_PDebt_for_predictions_standalone, newdata=allmean_all_2009_2018_levels_standalone, type="response", se.fit=TRUE))
allmean_probit_all_2016_2018_levels_standalone <- cbind(allmean_all_2016_2018_levels_standalone, predict(probit_allcountries_PDebt_for_predictions_standalone, newdata=allmean_all_2016_2018_levels_standalone, type="response", se.fit=TRUE))

#merge Euro and standalone
allmean_probit_all_2018_levels_merged <- rbind(allmean_probit_all_2018_levels_Euro, allmean_probit_all_2018_levels_standalone)
allmean_probit_all_mean_levels_merged <- rbind(allmean_probit_all_mean_levels_Euro, allmean_probit_all_mean_levels_standalone)
allmean_probit_all_2009_2018_levels_merged<- rbind(allmean_probit_all_2009_2018_levels_Euro, allmean_probit_all_2009_2018_levels_standalone)
allmean_probit_all_2016_2018_levels_merged <- rbind(allmean_probit_all_2016_2018_levels_Euro, allmean_probit_all_2016_2018_levels_standalone)

#reorder
allmean_probit_all_2018_levels_merged$ccode = factor(allmean_probit_all_2018_levels_merged$ccode, levels=allmean_probit_all_2018_levels_merged[order(-allmean_probit_all_2018_levels_merged$fit), "ccode"])
allmean_probit_all_mean_levels_merged$ccode = factor(allmean_probit_all_mean_levels_merged$ccode, levels=allmean_probit_all_mean_levels_merged[order(-allmean_probit_all_mean_levels_merged$fit), "ccode"])
allmean_probit_all_2009_2018_levels_merged$ccode = factor(allmean_probit_all_2009_2018_levels_merged$ccode, levels=allmean_probit_all_2009_2018_levels_merged[order(-allmean_probit_all_2009_2018_levels_merged$fit), "ccode"])
allmean_probit_all_2016_2018_levels_merged$ccode = factor(allmean_probit_all_2016_2018_levels_merged$ccode, levels=allmean_probit_all_2016_2018_levels_merged[order(-allmean_probit_all_2016_2018_levels_merged$fit), "ccode"])

#plot
#using 2018 public debt and population growth numbers
#probit

#2018 levels
boxplot_probit_all_2018_levels <- ggplot(allmean_probit_all_2018_levels_merged, aes(ccode, fit))+
  geom_point() +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                position=position_dodge(.9), col="red")+
  xlab("Country") +
  ylab("Probability of 5-year future r-g>0") +
  ggtitle("Predicted probabilities for individual countries\n holding control variables at 2018 values")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white"))
boxplot_probit_all_2018_levels

#2009-2018 levels
boxplot_probit_all_2009_2018_levels <- ggplot(allmean_probit_all_2009_2018_levels_merged, aes(ccode, fit))+
  geom_point() +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                position=position_dodge(.9), col="red")+
  xlab("Country") +
  ylab("Probability of 3-year future r-g>0") +
  ggtitle("Predicted probabilities for individual countries\n holding control variables at mean 2009-2018 values")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white"))
boxplot_probit_all_2009_2018_levels

#2016-2018 levels
boxplot_probit_all_2016_2018_levels <- ggplot(allmean_probit_all_2016_2018_levels_merged, aes(ccode, fit))+
  geom_point() +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                position=position_dodge(.9), col="red")+
  xlab("Country") +
  ylab("Probability of 3-year future r-g>0") +
  ggtitle("Predicted probabilities for individual countries\n holding control variables at mean 2016-2018 values")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white"))
boxplot_probit_all_2016_2018_levels

#mean values
boxplot_probit_all_mean_levels <- ggplot(allmean_probit_all_mean_levels_merged, aes(ccode, fit))+
  geom_point() +
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.2,
                position=position_dodge(.9), col="red")+
  xlab("Country") +
  ylab("Probability of 3-year future r-g>0") +
  ggtitle("Predicted probabilities for individual countries\n holding control variables at mean values")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(panel.background = element_rect(fill = "white"))
boxplot_probit_all_mean_levels

#Question 5: Are the r-g risks related to running expansionary fiscal policy increasing with the level of public debt?
#quantile regression with covariates
#all countries
#Q10
qr_q10_allcountries <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat, tau = 0.1)
summary(qr_q10_allcountries)

#Q50
qr_q50_allcountries <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat, tau = 0.5)
summary(qr_q50_allcountries)

#Q90
qr_q90_allcountries <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat, tau = 0.9)
summary(qr_q90_allcountries)

dat$model_Q10_allcountries <- stats::predict(qr_q10_allcountries, newdata=dat)
dat$model_Q50_allcountries <- stats::predict(qr_q50_allcountries, newdata=dat)
dat$model_Q90_allcountries <- stats::predict(qr_q90_allcountries, newdata=dat)

plot_5year_rg_pdebt_multivariate_allcountries <- ggplot(dat,aes(PDebt,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q10_allcountries, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q50_allcountries, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q90_allcountries, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("Future r-g and public-debt-to-GDP\n 22 OECD countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_allcountries

#quantile regressions: safety
plot_5year_rg_pdebt_multivariate_allcountries_safety <- ggplot(dat,aes(safety,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat, aes(x=safety, y=model_Q10_allcountries, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat, aes(x=safety, y=model_Q50_allcountries, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat, aes(x=safety, y=model_Q90_allcountries, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Safe government bonds") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("Future r-g and safe government bonds\n 22 OECD countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_allcountries_safety

#Euro
#Q10
qr_q10_Euro <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro, tau = 0.1)
summary(qr_q10_Euro)

#Q50
qr_q50_Euro <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro, tau = 0.5)
summary(qr_q50_Euro)

#Q90
qr_q90_Euro <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro, tau = 0.9)
summary(qr_q90_Euro)

dat_Euro$model_Q10_Euro <- stats::predict(qr_q10_Euro, newdata=dat_Euro)
dat_Euro$model_Q50_Euro <- stats::predict(qr_q50_Euro, newdata=dat_Euro)
dat_Euro$model_Q90_Euro <- stats::predict(qr_q90_Euro, newdata=dat_Euro)

plot_5year_rg_pdebt_multivariate_Euro <- ggplot(dat_Euro,aes(PDebt,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q10_Euro, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q50_Euro, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q90_Euro, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("11 Eurozone countries: Public-debt-to-GDP")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("Yes"), values = c("sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro

plot_5year_rg_pdebt_multivariate_Euro_safety <- ggplot(dat_Euro,aes(safety,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro, aes(x=safety, y=model_Q10_Euro, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=safety, y=model_Q50_Euro, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=safety, y=model_Q90_Euro, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Safe government bonds") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("11 Eurozone countries: safe bonds")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("Yes"), values = c("sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_safety

#Euro periphery
#Q10
qr_q10_Euro_periphery <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_periphery, tau = 0.1)
summary(qr_q10_Euro_periphery)

#Q50
qr_q50_Euro_periphery <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_periphery, tau = 0.5)
summary(qr_q50_Euro_periphery)

#Q90
qr_q90_Euro_periphery <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_periphery, tau = 0.9)
summary(qr_q90_Euro_periphery)

dat_Euro_periphery$model_Q10_Euro_periphery <- stats::predict(qr_q10_Euro_periphery, newdata=dat_Euro_periphery)
dat_Euro_periphery$model_Q50_Euro_periphery <- stats::predict(qr_q50_Euro_periphery, newdata=dat_Euro_periphery)
dat_Euro_periphery$model_Q90_Euro_periphery <- stats::predict(qr_q90_Euro_periphery, newdata=dat_Euro_periphery)

plot_5year_rg_pdebt_multivariate_Euro_periphery <- ggplot(dat_Euro_periphery,aes(PDebt,rminusg_future5,colour=factor(Post2007)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=PDebt, y=model_Q10_Euro_periphery, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=PDebt, y=model_Q50_Euro_periphery, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=PDebt, y=model_Q90_Euro_periphery, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 5-year future r - g (in ppts.)") +
  ggtitle("5 Eurozone periphery countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Post-2007') +
  scale_color_manual(labels = c("No", "Yes"), values = c("sandybrown", "grey49")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_periphery

plot_5year_rg_pdebt_multivariate_Euro_periphery_safety <- ggplot(dat_Euro_periphery,aes(safety,rminusg_future5,colour=factor(Post2007)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=safety, y=model_Q10_Euro_periphery, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=safety, y=model_Q50_Euro_periphery, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_periphery, aes(x=safety, y=model_Q90_Euro_periphery, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 5-year future r - g (in ppts.)") +
  ggtitle("5 Eurozone periphery countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Post-2007') +
  scale_color_manual(labels = c("No", "Yes"), values = c("sandybrown", "grey49")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_periphery_safety 

#Euro core
#Q10
qr_q10_Euro_core <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_core, tau = 0.1)
summary(qr_q10_Euro_core)

#Q50
qr_q50_Euro_core <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_core, tau = 0.5)
summary(qr_q50_Euro_core)

#Q90
qr_q90_Euro_core <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_Euro_core, tau = 0.9)
summary(qr_q90_Euro_core)

dat_Euro_core$model_Q10_Euro_core <- stats::predict(qr_q10_Euro_core, newdata=dat_Euro_core)
dat_Euro_core$model_Q50_Euro_core <- stats::predict(qr_q50_Euro_core, newdata=dat_Euro_core)
dat_Euro_core$model_Q90_Euro_core <- stats::predict(qr_q90_Euro_core, newdata=dat_Euro_core)

plot_5year_rg_pdebt_multivariate_Euro_core <- ggplot(dat_Euro_core,aes(PDebt,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=PDebt, y=model_Q10_Euro_core, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=PDebt, y=model_Q50_Euro_core, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=PDebt, y=model_Q90_Euro_core, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 5-year future r - g (in ppts.)") +
  ggtitle("5 Eurozone core countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_core

plot_5year_rg_pdebt_multivariate_Euro_core_safety <- ggplot(dat_Euro_core,aes(safety,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=safety, y=model_Q10_Euro_core, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=safety, y=model_Q50_Euro_core, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_Euro_core, aes(x=safety, y=model_Q90_Euro_core, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 5-year future r - g (in ppts.)") +
  ggtitle("5 Eurozone core countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_core_safety

#standalone
#Q10
qr_q10_standalone <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_standalone, tau = 0.1)
summary(qr_q10_standalone)

#Q50
qr_q50_standalone <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_standalone, tau = 0.5)
summary(qr_q50_standalone)

#Q90
qr_q90_standalone <- rq(rminusg_future3~inflation + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + safety, data=dat_standalone, tau = 0.9)
summary(qr_q90_standalone)

dat_standalone$model_Q10_standalone <- stats::predict(qr_q10_standalone, newdata=dat_standalone)
dat_standalone$model_Q50_standalone <- stats::predict(qr_q50_standalone, newdata=dat_standalone)
dat_standalone$model_Q90_standalone <- stats::predict(qr_q90_standalone, newdata=dat_standalone)

plot_5year_rg_pdebt_multivariate_standalone <- ggplot(dat_standalone,aes(PDebt,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q10_standalone, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q50_standalone, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q90_standalone, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("11 stand-alone countries: Public-debt-to-GDP")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +  scale_color_manual(labels = c("No", "Yes"), values = c("sandybrown", "grey49")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No"), values = c("grey49"))
plot_5year_rg_pdebt_multivariate_standalone

plot_5year_rg_pdebt_multivariate_standalone_safety <- ggplot(dat_standalone,aes(safety,rminusg_future5,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_standalone, aes(x=safety, y=model_Q10_standalone, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=safety, y=model_Q50_standalone, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=safety, y=model_Q90_standalone, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Safe government bonds") +
  ylab("Predicted 3-year future r - g (in ppts.)") +
  ggtitle("11 stand-alone countries: safe bonds")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +  scale_color_manual(labels = c("No", "Yes"), values = c("sandybrown", "grey49")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No"), values = c("grey49"))
plot_5year_rg_pdebt_multivariate_standalone_safety

library(ggpubr)
fig_quantiles <- ggpubr::ggarrange(
  plot_5year_rg_pdebt_multivariate_allcountries, plot_5year_rg_pdebt_multivariate_allcountries_safety, plot_5year_rg_pdebt_multivariate_Euro, plot_5year_rg_pdebt_multivariate_Euro_safety, plot_5year_rg_pdebt_multivariate_standalone, plot_5year_rg_pdebt_multivariate_standalone_safety, ncol = 2, nrow=3, labels = c("A)", "B)", "C)", "D)", "E)", "F)"), 
  common.legend = FALSE, legend = "bottom")
fig_quantiles

###
#exclude Euro Crisis years 2008-2015
#dat_exclude_EuroCrisis <- subset(dat_Euro, EuroCrisis20082015 %in% c('0'))
dat_exclude_EuroCrisis <- subset(dat_Euro, year %in% c('1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006', '2007'))

qr_q10_Euro_excludeEuroCrisis <- rq(rminusg_future5~real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open, data=dat_exclude_EuroCrisis, tau = 0.1)
summary(qr_q10_Euro_excludeEuroCrisis)

#Q50
qr_q50_Euro_excludeEuroCrisis <- rq(rminusg_future5~real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open, data=dat_exclude_EuroCrisis, tau = 0.5)
summary(qr_q50_Euro_excludeEuroCrisis)

#Q90
qr_q90_Euro_excludeEuroCrisis <- rq(rminusg_future5~real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open, data=dat_exclude_EuroCrisis, tau = 0.9)
summary(qr_q90_Euro_excludeEuroCrisis)

#

dat_exclude_EuroCrisis$model_Q10_Euro_excludeEuroCrisis <- stats::predict(qr_q10_Euro_excludeEuroCrisis, newdata=dat_exclude_EuroCrisis)
dat_exclude_EuroCrisis$model_Q50_Euro_excludeEuroCrisis <- stats::predict(qr_q50_Euro_excludeEuroCrisis, newdata=dat_exclude_EuroCrisis)
dat_exclude_EuroCrisis$model_Q90_Euro_excludeEuroCrisis <- stats::predict(qr_q90_Euro_excludeEuroCrisis, newdata=dat_exclude_EuroCrisis)

plot_5year_rg_pdebt_multivariate_Euro_excludeEuroCrisis <- ggplot(dat_exclude_EuroCrisis,aes(PDebt,rminusg_future5,colour=factor(Post2007)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_exclude_EuroCrisis, aes(x=PDebt, y=model_Q10_Euro_excludeEuroCrisis, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_exclude_EuroCrisis, aes(x=PDebt, y=model_Q50_Euro_excludeEuroCrisis, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  geom_smooth(method="lm", data=dat_exclude_EuroCrisis, aes(x=PDebt, y=model_Q90_Euro_excludeEuroCrisis, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5) +
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 5-year future r - g (in ppts.)") +
  ggtitle("11 Eurozone countries (post-2007 excluded)")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  labs(color='Post-2007') +
  scale_color_manual(labels = c("No", "Yes"), values = c("sandybrown", "grey49")) +
  theme(panel.background = element_rect(fill = "white"))
plot_5year_rg_pdebt_multivariate_Euro_excludeEuroCrisis

#grid-plots

library(ggpubr)
fig_Euro_standalone <- ggpubr::ggarrange(
  plot_5year_rg_pdebt_multivariate_Euro, plot_5year_rg_pdebt_multivariate_standalone, ncol = 2, labels = c("A)", "B)"), 
  common.legend = TRUE, legend = "bottom")
fig_Euro_standalone

fig_allcountries_Euro_standalone_Euroexclude <- ggpubr::ggarrange(
  plot_5year_rg_pdebt_multivariate_allcountries, plot_5year_rg_pdebt_multivariate_Euro, plot_5year_rg_pdebt_multivariate_standalone, plot_5year_rg_pdebt_multivariate_Euro_excludeEuroCrisis, ncol = 2, nrow=2, labels = c("A)", "B)", "C)"), 
  common.legend = TRUE, legend = "bottom")
fig_allcountries_Euro_standalone_Euroexclude

fig_quantiles_groups <- ggpubr::ggarrange(
  plot_5year_rg_pdebt_multivariate_allcountries, plot_5year_rg_pdebt_multivariate_Euro, plot_5year_rg_pdebt_multivariate_standalone, plot_5year_rg_pdebt_multivariate_Euro_periphery, plot_5year_rg_pdebt_multivariate_Euro_core,  ncol = 2, nrow=3, labels = c("A)", "B)", "C)", "D)", "E)"), 
  common.legend = TRUE, legend = "bottom")
fig_allcountries_Euro_standalone_Euroexclude

library(ggpubr)
fig_Euro_Euroexclude <- ggpubr::ggarrange(
  plot_5year_rg_pdebt_multivariate_Euro, plot_5year_rg_pdebt_multivariate_Euro_excludeEuroCrisis, ncol = 2, labels = c("A)", "B)"), 
  common.legend = TRUE, legend = "bottom")
fig_Euro_Euroexclude

#A higher public-debt-to-GDP ratio is not associated with higher future r-g. For the 90th quantile, the regression line is even downward sloping.

#Question 6: Are episodes of negative r-g on average shorter when the initial public-debt-to-GDP ratio is higher?
rglength_v1 <- lm(lengthRGepisode~InitialPDebtRGepisode, data=dat_rg_episodes)
summary(rglength_v1)

clubSandwich::coef_test(rglength_v1, vcov = "CR0", 
                        cluster = dat_rg_episodes$ccode, test = "naive-t")

#with country-fixed effects
rglength_v2 <- plm(lengthRGepisode~InitialPDebtRGepisode, index=c("ccode", "year"), model="within", effect="individual", data=dat_rg_episodes)
summary(rglength_v2)
coeftest(rglength_v2, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(rglength_v2, vcov = "CR0", 
                        cluster = dat_rg_episodes$ccode, test = "naive-t")

#with time-fixed effects
rglength_v3 <- plm(lengthRGepisode~InitialPDebtRGepisode, index=c("ccode", "year"), model="within", effect="time", data=dat_rg_episodes)
summary(rglength_v3)
coeftest(rglength_v3, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(rglength_v3, vcov = "CR0", 
                        cluster = dat_rg_episodes$ccode, test = "naive-t")

#with country- and time-fixed effects
rglength_v4 <- plm(lengthRGepisode~InitialPDebtRGepisode, index=c("ccode", "year"), model="within", effect="twoways", data=dat_rg_episodes)
summary(rglength_v4)
coeftest(rglength_v4, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(rglength_v4, vcov = "CR0", 
                        cluster = dat_rg_episodes$ccode, test = "naive-t")

#Question 7: How are interest-growth differentials related to the fiscal stance?
#with country-fixed effects
fiscalstance_v1 <- plm(primaryFB~lag(PDebt,1) + rminusg + outputgap, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v1)
coeftest(fiscalstance_v1, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(fiscalstance_v1, vcov = "CR0", 
                        cluster = dat$ccode, test = "naive-t")

#with country-fixed effects and interaction PDebt * rminusg
fiscalstance_v2 <- plm(primaryFB~lag(PDebt,1) + rminusg + lag(PDebt,1) * rminusg + outputgap + safety, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2)
coeftest(fiscalstance_v2, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(fiscalstance_v2, vcov = "CR0", 
                        cluster = dat$ccode, test = "naive-t")

#The primary fiscal balance is tighter when interest-growth differentials are higher, with the magnitude of tightening increasing with the initial debt level

#euro sample
fiscalstance_v2_Euro <- plm(primaryFB~lag(PDebt,1) + rminusg + lag(PDebt,1) * rminusg + outputgap + safety, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro)
coeftest(fiscalstance_v2_Euro, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(fiscalstance_v2_Euro, vcov = "CR0", 
                        cluster = dat_Euro$ccode, test = "naive-t")

#standalone sample
fiscalstance_v2_standalone <- plm(primaryFB~lag(PDebt,1) + rminusg + lag(PDebt,1) * rminusg + outputgap + safety, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone)
coeftest(fiscalstance_v2_standalone, vcov.=function(x) vcovHC(x, type="sss"))

clubSandwich::coef_test(fiscalstance_v2_standalone, vcov = "CR0", 
                        cluster = dat_standalone$ccode, test = "naive-t")

#for standalone countries, there is no significant relationship between higher r-g differentials and tighter primary balances. The interaction term with public debt also lacks significance in contrast to the estimate from the Euro sample, where we find evidence that the magnitude of tightening is increasing with higher public debt levels.

#appendix

#Explaining variation in LTrate
#with country-fixed effects
LTrate_determinants_v1 <- plm(LTrate~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v1)
coeftest(LTrate_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
LTrate_determinants_v2 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v2)
coeftest(LTrate_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
LTrate_determinants_v3 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v3)
coeftest(LTrate_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
LTrate_determinants_v4 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v4)
coeftest(LTrate_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
LTrate_determinants_v5 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v5)
coeftest(LTrate_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
LTrate_determinants_v6 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(LTrate_determinants_v6)
coeftest(LTrate_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
LTrate_determinants_v6_periphery <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(LTrate_determinants_v6_periphery)
coeftest(LTrate_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
LTrate_determinants_v6_core <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(LTrate_determinants_v6_core)
coeftest(LTrate_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
LTrate_determinants_v7 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(LTrate_determinants_v7)
coeftest(LTrate_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
LTrate_determinants_v8 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(LTrate_determinants_v8)
coeftest(LTrate_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
LTrate_determinants_v9 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(LTrate_determinants_v9)
coeftest(LTrate_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
LTrate_determinants_v10 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(LTrate_determinants_v10)
coeftest(LTrate_determinants_v10, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
LTrate_determinants_v11 <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(LTrate_determinants_v11)
coeftest(LTrate_determinants_v11, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
LTrate_determinants_v5_3_year <- plm(LTrate~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_3_year)
summary(LTrate_determinants_v5_3_year)
coeftest(LTrate_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for staLTrateazer table
ses.LTrate_determinants_v1 <- list(coeftest(LTrate_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v1 <- list(coeftest(LTrate_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v1 <- list(coeftest(LTrate_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v2 <- list(coeftest(LTrate_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v2 <- list(coeftest(LTrate_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v2 <- list(coeftest(LTrate_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v3 <- list(coeftest(LTrate_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v3 <- list(coeftest(LTrate_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v3 <- list(coeftest(LTrate_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v4 <- list(coeftest(LTrate_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v4 <- list(coeftest(LTrate_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v4 <- list(coeftest(LTrate_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v5 <- list(coeftest(LTrate_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v5 <- list(coeftest(LTrate_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v5 <- list(coeftest(LTrate_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v6 <- list(coeftest(LTrate_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v6 <- list(coeftest(LTrate_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v6 <- list(coeftest(LTrate_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v6_periphery <- list(coeftest(LTrate_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v6_periphery <- list(coeftest(LTrate_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v6_periphery <- list(coeftest(LTrate_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v6_core <- list(coeftest(LTrate_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v6_core <- list(coeftest(LTrate_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v6_core <- list(coeftest(LTrate_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v7 <- list(coeftest(LTrate_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v7 <- list(coeftest(LTrate_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v7 <- list(coeftest(LTrate_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v8 <- list(coeftest(LTrate_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v8 <- list(coeftest(LTrate_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v8 <- list(coeftest(LTrate_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v9 <- list(coeftest(LTrate_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v9 <- list(coeftest(LTrate_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v9 <- list(coeftest(LTrate_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.LTrate_determinants_v5_3_year <- list(coeftest(LTrate_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.LTrate_determinants_v5_3_year <- list(coeftest(LTrate_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.LTrate_determinants_v5_3_year <- list(coeftest(LTrate_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#Table A4
#stargazer table for LaTEX
stargazer(LTrate_determinants_v3, LTrate_determinants_v5, LTrate_determinants_v9, LTrate_determinants_v5_3_year, LTrate_determinants_v6, LTrate_determinants_v7, LTrate_determinants_v6_periphery, LTrate_determinants_v6_core, t=list(unlist(tvals.LTrate_determinants_v3), unlist(tvals.LTrate_determinants_v5), unlist(tvals.LTrate_determinants_v9), unlist(tvals.LTrate_determinants_v5_3_year), unlist(tvals.LTrate_determinants_v6), unlist(tvals.LTrate_determinants_v7), unlist(tvals.LTrate_determinants_v6_periphery), unlist(tvals.LTrate_determinants_v6_core)), se=list(unlist(ses.LTrate_determinants_v3), unlist(ses.LTrate_determinants_v5), unlist(ses.LTrate_determinants_v9), unlist(ses.LTrate_determinants_v5_3_year), unlist(ses.LTrate_determinants_v6), unlist(ses.LTrate_determinants_v7), unlist(ses.LTrate_determinants_v6_periphery), unlist(ses.LTrate_determinants_v6_core)), p=list(unlist(pvals.LTrate_determinants_v3), unlist(pvals.LTrate_determinants_v5), unlist(pvals.LTrate_determinants_v9), unlist(pvals.LTrate_determinants_v5_3_year), unlist(pvals.LTrate_determinants_v6), unlist(pvals.LTrate_determinants_v7), unlist(pvals.LTrate_determinants_v6_periphery), unlist(pvals.LTrate_determinants_v6_core)))

#Explaining variation in GDP_growth
#with country-fixed effects
GDP_growth_determinants_v1 <- plm(GDP_growth~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v1)
coeftest(GDP_growth_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
GDP_growth_determinants_v2 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v2)
coeftest(GDP_growth_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
GDP_growth_determinants_v3 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v3)
coeftest(GDP_growth_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
GDP_growth_determinants_v4 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v4)
coeftest(GDP_growth_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
GDP_growth_determinants_v5 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v5)
coeftest(GDP_growth_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
GDP_growth_determinants_v6 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(GDP_growth_determinants_v6)
coeftest(GDP_growth_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
GDP_growth_determinants_v6_periphery <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(GDP_growth_determinants_v6_periphery)
coeftest(GDP_growth_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
GDP_growth_determinants_v6_core <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(GDP_growth_determinants_v6_core)
coeftest(GDP_growth_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
GDP_growth_determinants_v7 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(GDP_growth_determinants_v7)
coeftest(GDP_growth_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
GDP_growth_determinants_v8 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(GDP_growth_determinants_v8)
coeftest(GDP_growth_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
GDP_growth_determinants_v9 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(GDP_growth_determinants_v9)
coeftest(GDP_growth_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
GDP_growth_determinants_v10 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(GDP_growth_determinants_v10)
coeftest(GDP_growth_determinants_v10, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
GDP_growth_determinants_v11 <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(GDP_growth_determinants_v11)
coeftest(GDP_growth_determinants_v11, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
GDP_growth_determinants_v5_3_year <- plm(GDP_growth~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_3_year)
summary(GDP_growth_determinants_v5_3_year)
coeftest(GDP_growth_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for staGDP_growthazer table
ses.GDP_growth_determinants_v1 <- list(coeftest(GDP_growth_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v1 <- list(coeftest(GDP_growth_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v1 <- list(coeftest(GDP_growth_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v2 <- list(coeftest(GDP_growth_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v2 <- list(coeftest(GDP_growth_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v2 <- list(coeftest(GDP_growth_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v3 <- list(coeftest(GDP_growth_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v3 <- list(coeftest(GDP_growth_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v3 <- list(coeftest(GDP_growth_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v4 <- list(coeftest(GDP_growth_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v4 <- list(coeftest(GDP_growth_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v4 <- list(coeftest(GDP_growth_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v5 <- list(coeftest(GDP_growth_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v5 <- list(coeftest(GDP_growth_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v5 <- list(coeftest(GDP_growth_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v6 <- list(coeftest(GDP_growth_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v6 <- list(coeftest(GDP_growth_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v6 <- list(coeftest(GDP_growth_determinants_v6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v6_periphery <- list(coeftest(GDP_growth_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v6_periphery <- list(coeftest(GDP_growth_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v6_periphery <- list(coeftest(GDP_growth_determinants_v6_periphery, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v6_core <- list(coeftest(GDP_growth_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v6_core <- list(coeftest(GDP_growth_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v6_core <- list(coeftest(GDP_growth_determinants_v6_core, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v7 <- list(coeftest(GDP_growth_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v7 <- list(coeftest(GDP_growth_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v7 <- list(coeftest(GDP_growth_determinants_v7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v8 <- list(coeftest(GDP_growth_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v8 <- list(coeftest(GDP_growth_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v8 <- list(coeftest(GDP_growth_determinants_v8, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v9 <- list(coeftest(GDP_growth_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v9 <- list(coeftest(GDP_growth_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v9 <- list(coeftest(GDP_growth_determinants_v9, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.GDP_growth_determinants_v5_3_year <- list(coeftest(GDP_growth_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.GDP_growth_determinants_v5_3_year <- list(coeftest(GDP_growth_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.GDP_growth_determinants_v5_3_year <- list(coeftest(GDP_growth_determinants_v5_3_year, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#Table A5
#stargazer table for LaTEX
stargazer(GDP_growth_determinants_v3, GDP_growth_determinants_v5, GDP_growth_determinants_v9, GDP_growth_determinants_v5_3_year, GDP_growth_determinants_v6, GDP_growth_determinants_v7, GDP_growth_determinants_v6_periphery, GDP_growth_determinants_v6_core, t=list(unlist(tvals.GDP_growth_determinants_v3), unlist(tvals.GDP_growth_determinants_v5), unlist(tvals.GDP_growth_determinants_v9), unlist(tvals.GDP_growth_determinants_v5_3_year), unlist(tvals.GDP_growth_determinants_v6), unlist(tvals.GDP_growth_determinants_v7), unlist(tvals.GDP_growth_determinants_v6_periphery), unlist(tvals.GDP_growth_determinants_v6_core)), se=list(unlist(ses.GDP_growth_determinants_v3), unlist(ses.GDP_growth_determinants_v5), unlist(ses.GDP_growth_determinants_v9), unlist(ses.GDP_growth_determinants_v5_3_year), unlist(ses.GDP_growth_determinants_v6), unlist(ses.GDP_growth_determinants_v7), unlist(ses.GDP_growth_determinants_v6_periphery), unlist(ses.GDP_growth_determinants_v6_core)), p=list(unlist(pvals.GDP_growth_determinants_v3), unlist(pvals.GDP_growth_determinants_v5), unlist(pvals.GDP_growth_determinants_v9), unlist(pvals.GDP_growth_determinants_v5_3_year), unlist(pvals.GDP_growth_determinants_v6), unlist(pvals.GDP_growth_determinants_v7), unlist(pvals.GDP_growth_determinants_v6_periphery), unlist(pvals.GDP_growth_determinants_v6_core)))

#fixed-effects panel regressions with risk spread to Germany instead of safety
#with country-fixed effects
#rg differential as dependent variable

rg_determinants_v1_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v1_spread_Germany)
coeftest(rg_determinants_v1_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v2_spread_Germany)
coeftest(rg_determinants_v2_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#+safety haven, GDP size and financial openness
rg_determinants_v3_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + spread_Germany + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v3_spread_Germany)
coeftest(rg_determinants_v3_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safety_haven*EuroMember
rg_determinants_v4_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + spread_Germany*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v4_spread_Germany)
coeftest(rg_determinants_v4_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safety_haven*EuroMember + interaction safety_haven*EuroCrisis
rg_determinants_v5_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroMember + spread_Germany*EuroCrisis + spread_Germany*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v5_spread_Germany)
coeftest(rg_determinants_v5_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
rg_determinants_v6_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(rg_determinants_v6_spread_Germany)
coeftest(rg_determinants_v6_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery_spread_Germany)
coeftest(rg_determinants_v6_periphery_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(rg_determinants_v6_core_spread_Germany)
coeftest(rg_determinants_v6_core_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(rg_determinants_v7_spread_Germany)
coeftest(rg_determinants_v7_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8_spread_Germany <- plm(rminusg_future10~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroMember + spread_Germany*EuroCrisis + spread_Germany*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v8_spread_Germany)
coeftest(rg_determinants_v8_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
#dat_exclude_US_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + fin_open + spread_Germany*EuroMember + spread_Germany*EuroCrisis + spread_Germany*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(rg_determinants_v9_spread_Germany)
coeftest(rg_determinants_v9_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#

#exclude US
#dat_exclude_US <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + spread_Germany*EuroMember + spread_Germany*EuroCrisis + spread_Germany*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(rg_determinants_v10_spread_Germany)
coeftest(rg_determinants_v10_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
#dat_exclude_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11_spread_Germany <- plm(rminusg_future5~ real_GDP_gr + real_LTrate + pop_growth + PDebt + primaryFB + spread_Germany*EuroMember + spread_Germany*EuroCrisis + spread_Germany*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(rg_determinants_v11_spread_Germany)
coeftest(rg_determinants_v11_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))


#preparations for stargazer table
ses.rg_determinants_v1_spread_Germany <- list(coeftest(rg_determinants_v1_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v1_spread_Germany <- list(coeftest(rg_determinants_v1_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v1_spread_Germany <- list(coeftest(rg_determinants_v1_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v2_spread_Germany <- list(coeftest(rg_determinants_v2_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2_spread_Germany <- list(coeftest(rg_determinants_v2_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2_spread_Germany <- list(coeftest(rg_determinants_v2_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3_spread_Germany <- list(coeftest(rg_determinants_v3_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_spread_Germany <- list(coeftest(rg_determinants_v3_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_spread_Germany <- list(coeftest(rg_determinants_v3_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4_spread_Germany <- list(coeftest(rg_determinants_v4_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4_spread_Germany <- list(coeftest(rg_determinants_v4_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4_spread_Germany <- list(coeftest(rg_determinants_v4_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_spread_Germany <- list(coeftest(rg_determinants_v5_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_spread_Germany <- list(coeftest(rg_determinants_v5_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_spread_Germany <- list(coeftest(rg_determinants_v5_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_spread_Germany <- list(coeftest(rg_determinants_v6_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_spread_Germany <- list(coeftest(rg_determinants_v6_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_spread_Germany <- list(coeftest(rg_determinants_v6_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_spread_Germany <- list(coeftest(rg_determinants_v6_periphery_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_spread_Germany <- list(coeftest(rg_determinants_v6_periphery_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_spread_Germany <- list(coeftest(rg_determinants_v6_periphery_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_spread_Germany <- list(coeftest(rg_determinants_v6_core_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_spread_Germany <- list(coeftest(rg_determinants_v6_core_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_spread_Germany <- list(coeftest(rg_determinants_v6_core_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_spread_Germany <- list(coeftest(rg_determinants_v7_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_spread_Germany <- list(coeftest(rg_determinants_v7_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_spread_Germany <- list(coeftest(rg_determinants_v7_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v8_spread_Germany <- list(coeftest(rg_determinants_v8_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v8_spread_Germany <- list(coeftest(rg_determinants_v8_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v8_spread_Germany <- list(coeftest(rg_determinants_v8_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_spread_Germany <- list(coeftest(rg_determinants_v9_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_spread_Germany <- list(coeftest(rg_determinants_v9_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_spread_Germany <- list(coeftest(rg_determinants_v9_spread_Germany, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX
stargazer(rg_determinants_v3_spread_Germany, rg_determinants_v4_spread_Germany, rg_determinants_v5_spread_Germany, rg_determinants_v6_spread_Germany, rg_determinants_v7_spread_Germany, rg_determinants_v6_periphery_spread_Germany, rg_determinants_v6_core_spread_Germany, t=list(unlist(tvals.rg_determinants_v3_spread_Germany), unlist(tvals.rg_determinants_v4_spread_Germany), unlist(tvals.rg_determinants_v5_spread_Germany), unlist(tvals.rg_determinants_v6_spread_Germany), unlist(tvals.rg_determinants_v7_spread_Germany), unlist(tvals.rg_determinants_v6_periphery_spread_Germany), unlist(tvals.rg_determinants_v6_core_spread_Germany)), se=list(unlist(ses.rg_determinants_v3_spread_Germany), unlist(ses.rg_determinants_v4_spread_Germany), unlist(ses.rg_determinants_v5_spread_Germany), unlist(ses.rg_determinants_v6_spread_Germany), unlist(ses.rg_determinants_v7_spread_Germany), unlist(ses.rg_determinants_v6_periphery_spread_Germany), unlist(ses.rg_determinants_v6_core_spread_Germany)), p=list(unlist(pvals.rg_determinants_v3_spread_Germany), unlist(pvals.rg_determinants_v4_spread_Germany), unlist(pvals.rg_determinants_v5_spread_Germany), unlist(pvals.rg_determinants_v6_spread_Germany), unlist(pvals.rg_determinants_v7_spread_Germany), unlist(pvals.rg_determinants_v6_periphery_spread_Germany), unlist(pvals.rg_determinants_v6_core_spread_Germany)))

#Predictors of public-debt-to-GDP ratios
#all countries
PDebt_determinants_v1 <- plm(PDebt~ lag(rminusg) + inflation + lag(real_LTrate) + pop_growth + primaryFB + safety + fin_open + EuroCrisis + StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(PDebt_determinants_v1)
coeftest(PDebt_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))

#Euro countries
PDebt_determinants_v2 <- plm(PDebt~ lag(rminusg) + inflation + lag(real_LTrate) + pop_growth + primaryFB + safety + fin_open + EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(PDebt_determinants_v2)
coeftest(PDebt_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))

#stand-alone countries
PDebt_determinants_v3 <- plm(PDebt~ lag(rminusg) + inflation + lag(real_LTrate) + pop_growth + primaryFB + safety + fin_open + StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(PDebt_determinants_v3)
coeftest(PDebt_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery countries
PDebt_determinants_v4 <- plm(PDebt~ lag(rminusg) + inflation + lag(real_LTrate) + pop_growth + primaryFB + safety + fin_open + EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(PDebt_determinants_v4)
coeftest(PDebt_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core countries
PDebt_determinants_v5 <- plm(PDebt~ lag(rminusg) + inflation + lag(real_LTrate) + pop_growth + primaryFB + safety + fin_open + EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(PDebt_determinants_v5)
coeftest(PDebt_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.PDebt_determinants_v1 <- list(coeftest(PDebt_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.PDebt_determinants_v1 <- list(coeftest(PDebt_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.PDebt_determinants_v1 <- list(coeftest(PDebt_determinants_v1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.PDebt_determinants_v2 <- list(coeftest(PDebt_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.PDebt_determinants_v2 <- list(coeftest(PDebt_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.PDebt_determinants_v2 <- list(coeftest(PDebt_determinants_v2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.PDebt_determinants_v3 <- list(coeftest(PDebt_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.PDebt_determinants_v3 <- list(coeftest(PDebt_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.PDebt_determinants_v3 <- list(coeftest(PDebt_determinants_v3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.PDebt_determinants_v4 <- list(coeftest(PDebt_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.PDebt_determinants_v4 <- list(coeftest(PDebt_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.PDebt_determinants_v4 <- list(coeftest(PDebt_determinants_v4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.PDebt_determinants_v5 <- list(coeftest(PDebt_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.PDebt_determinants_v5 <- list(coeftest(PDebt_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.PDebt_determinants_v5 <- list(coeftest(PDebt_determinants_v5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

stargazer(PDebt_determinants_v1, PDebt_determinants_v2, PDebt_determinants_v3, PDebt_determinants_v4, PDebt_determinants_v5, t=list(unlist(tvals.PDebt_determinants_v1), unlist(tvals.PDebt_determinants_v2), unlist(tvals.PDebt_determinants_v3), unlist(tvals.PDebt_determinants_v4), unlist(tvals.PDebt_determinants_v5)), se=list(unlist(ses.PDebt_determinants_v1), unlist(ses.PDebt_determinants_v2), unlist(ses.PDebt_determinants_v3), unlist(ses.PDebt_determinants_v4), unlist(ses.PDebt_determinants_v5)), p=list(unlist(pvals.PDebt_determinants_v1), unlist(pvals.PDebt_determinants_v2), unlist(pvals.PDebt_determinants_v3), unlist(pvals.PDebt_determinants_v4), unlist(pvals.PDebt_determinants_v5)))

#robustness check: US as safety benchmark for stand-alone countries
#Explaining variation in r-g
#with country-fixed effects
#rg differential as dependent variable
rg_determinants_v1_benchmark <- plm(rminusg~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v1_benchmark)
coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v2_benchmark)
coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
rg_determinants_v3_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v3_benchmark)
coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
rg_determinants_v4_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v4_benchmark)
coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
rg_determinants_v5_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v5_benchmark)
coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
rg_determinants_v6_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(rg_determinants_v6_benchmark)
coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery_benchmark)
coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(rg_determinants_v6_core_benchmark)
coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(rg_determinants_v7_benchmark)
coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v8_benchmark)
coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
#dat_exclude_US_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(rg_determinants_v9_benchmark)
coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
#dat_exclude_US <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(rg_determinants_v10_benchmark)
coeftest(rg_determinants_v10_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
#dat_exclude_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(rg_determinants_v11_benchmark)
coeftest(rg_determinants_v11_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

rg_determinants_v5_3_year_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_3_year)
summary(rg_determinants_v5_3_year_benchmark)
coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX
stargazer(rg_determinants_v3_benchmark, rg_determinants_v5_benchmark, rg_determinants_v9_benchmark, rg_determinants_v5_3_year_benchmark, rg_determinants_v6_benchmark, rg_determinants_v7_benchmark, rg_determinants_v6_periphery_benchmark, rg_determinants_v6_core_benchmark, t=list(unlist(tvals.rg_determinants_v3_benchmark), unlist(tvals.rg_determinants_v5_benchmark), unlist(tvals.rg_determinants_v9_benchmark), unlist(tvals.rg_determinants_v5_3_year_benchmark), unlist(tvals.rg_determinants_v6_benchmark), unlist(tvals.rg_determinants_v7_benchmark), unlist(tvals.rg_determinants_v6_periphery_benchmark), unlist(tvals.rg_determinants_v6_core_benchmark)), se=list(unlist(ses.rg_determinants_v3_benchmark), unlist(ses.rg_determinants_v5_benchmark), unlist(ses.rg_determinants_v9_benchmark), unlist(ses.rg_determinants_v5_3_year_benchmark), unlist(ses.rg_determinants_v6_benchmark), unlist(ses.rg_determinants_v7_benchmark), unlist(ses.rg_determinants_v6_periphery_benchmark), unlist(ses.rg_determinants_v6_core_benchmark)), p=list(unlist(pvals.rg_determinants_v3_benchmark), unlist(pvals.rg_determinants_v5_benchmark), unlist(pvals.rg_determinants_v9_benchmark), unlist(pvals.rg_determinants_v5_3_year_benchmark), unlist(pvals.rg_determinants_v6_benchmark), unlist(pvals.rg_determinants_v7_benchmark), unlist(pvals.rg_determinants_v6_periphery_benchmark), unlist(pvals.rg_determinants_v6_core_benchmark)))

#robustness check: US as safety benchmark for stand-alone countries
#Explaining variation in r-g
#with country-fixed effects
#rg differential as dependent variable
rg_determinants_v1_benchmark <- plm(rminusg~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v1_benchmark)
coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v2_benchmark)
coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+safe haven, GDP size and financial openness
rg_determinants_v3_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v3_benchmark)
coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember
rg_determinants_v4_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v4_benchmark)
coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
rg_determinants_v5_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v5_benchmark)
coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
rg_determinants_v6_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(rg_determinants_v6_benchmark)
coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery_benchmark)
coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_core)
summary(rg_determinants_v6_core_benchmark)
coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(rg_determinants_v7_benchmark)
coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v8_benchmark)
coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
#dat_exclude_US_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(rg_determinants_v9_benchmark)
coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
#dat_exclude_US <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US)
summary(rg_determinants_v10_benchmark)
coeftest(rg_determinants_v10_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
#dat_exclude_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis+ fin_open, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(rg_determinants_v11_benchmark)
coeftest(rg_determinants_v11_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

rg_determinants_v5_3_year_benchmark <- plm(rminusg~ inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safe_benchmark*EuroMember + safe_benchmark*EuroCrisis + safe_benchmark*StandaCrisis, index=c("ccode", "Period"), model="within", effect="individual", data=pdata_3_year)
summary(rg_determinants_v5_3_year_benchmark)
coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v1_benchmark <- list(coeftest(rg_determinants_v1_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2_benchmark <- list(coeftest(rg_determinants_v2_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_benchmark <- list(coeftest(rg_determinants_v3_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4_benchmark <- list(coeftest(rg_determinants_v4_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_benchmark <- list(coeftest(rg_determinants_v5_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_benchmark <- list(coeftest(rg_determinants_v6_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_benchmark <- list(coeftest(rg_determinants_v6_periphery_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_benchmark <- list(coeftest(rg_determinants_v6_core_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_benchmark <- list(coeftest(rg_determinants_v7_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v8_benchmark <- list(coeftest(rg_determinants_v8_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_benchmark <- list(coeftest(rg_determinants_v9_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_3_year_benchmark <- list(coeftest(rg_determinants_v5_3_year_benchmark, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX
stargazer(rg_determinants_v3_benchmark, rg_determinants_v5_benchmark, rg_determinants_v9_benchmark, rg_determinants_v5_3_year_benchmark, rg_determinants_v6_benchmark, rg_determinants_v7_benchmark, rg_determinants_v6_periphery_benchmark, rg_determinants_v6_core_benchmark, t=list(unlist(tvals.rg_determinants_v3_benchmark), unlist(tvals.rg_determinants_v5_benchmark), unlist(tvals.rg_determinants_v9_benchmark), unlist(tvals.rg_determinants_v5_3_year_benchmark), unlist(tvals.rg_determinants_v6_benchmark), unlist(tvals.rg_determinants_v7_benchmark), unlist(tvals.rg_determinants_v6_periphery_benchmark), unlist(tvals.rg_determinants_v6_core_benchmark)), se=list(unlist(ses.rg_determinants_v3_benchmark), unlist(ses.rg_determinants_v5_benchmark), unlist(ses.rg_determinants_v9_benchmark), unlist(ses.rg_determinants_v5_3_year_benchmark), unlist(ses.rg_determinants_v6_benchmark), unlist(ses.rg_determinants_v7_benchmark), unlist(ses.rg_determinants_v6_periphery_benchmark), unlist(ses.rg_determinants_v6_core_benchmark)), p=list(unlist(pvals.rg_determinants_v3_benchmark), unlist(pvals.rg_determinants_v5_benchmark), unlist(pvals.rg_determinants_v9_benchmark), unlist(pvals.rg_determinants_v5_3_year_benchmark), unlist(pvals.rg_determinants_v6_benchmark), unlist(pvals.rg_determinants_v7_benchmark), unlist(pvals.rg_determinants_v6_periphery_benchmark), unlist(pvals.rg_determinants_v6_core_benchmark)))

#robustness check: instrument PDebt with the average public debt ratios in other OECD countries

library(AER)
#rg differential as dependent variable
rg_determinants_v1_instrument <- plm(rminusg~ inflation + real_LTrate + pop_growth, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v1_instrument)
coeftest(rg_determinants_v1_instrument, vcov.=function(x) vcovHC(x, type="sss"))
#+fiscal fundamentals
rg_determinants_v2_instrument <- plm(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(rg_determinants_v2_instrument)
coeftest(rg_determinants_v2_instrument, vcov.=function(x) vcovHC(x, type="sss"))

library(ivpack)
library(clubSandwich)

#+safe haven, GDP size and financial openness
rg_determinants_v3_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + safety + fin_open + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + safety + fin_open + factor(ccode), data=dat)
summary(rg_determinants_v3_instrument)
#weak instrument test
summary(rg_determinants_v3_instrument, vcov = sandwich)
summary(rg_determinants_v3_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v3_instrument, vcov.=vcovHAC(rg_determinants_v3_instrument))
coef_test(rg_determinants_v3_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")

#+interaction safe_haven*EuroMember
rg_determinants_v4_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + safety*EuroMember + fin_open + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + safety*EuroMember + fin_open + factor(ccode), data=dat)
summary(rg_determinants_v4_instrument)
#weak instrument test
summary(rg_determinants_v4_instrument, vcov = sandwich)
summary(rg_determinants_v4_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v4_instrument, vcov.=vcovHAC(rg_determinants_v4_instrument))

coef_test(rg_determinants_v4_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
rg_determinants_v5_instrument <- ivreg(rminusg~ lag(PDebt,1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode), data=dat)
summary(rg_determinants_v5_instrument)
#weak instrument test
summary(rg_determinants_v5_instrument, vcov = sandwich)
summary(rg_determinants_v5_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v5_instrument, vcov.=vcovHAC(rg_determinants_v5_instrument))

coef_test(rg_determinants_v5_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")


#Euro
rg_determinants_v6_instrument <- plm(rminusg ~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode), index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(rg_determinants_v6_instrument)
coeftest(rg_determinants_v6_instrument, vcov.=function(x) vcovHC(x, type="sss"))

rg_determinants_v6_instrument <- ivreg(rminusg ~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode), data=dat_Euro)
summary(rg_determinants_v6_instrument)
#weak instrument test
summary(rg_determinants_v6_instrument, vcov = sandwich)
summary(rg_determinants_v6_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v6_instrument, vcov.=vcovHAC(rg_determinants_v6_instrument))

coef_test(rg_determinants_v6_instrument, vcov = "CR0", cluster = dat_Euro$ccode, test = "naive-t")

#Euro periphery
rg_determinants_v6_periphery_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode), data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery_instrument)
#weak instrument test
summary(rg_determinants_v6_periphery_instrument, vcov = sandwich)
summary(rg_determinants_v6_periphery_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v6_periphery_instrument, vcov.=vcovHAC(rg_determinants_v6_periphery_instrument))

coef_test(rg_determinants_v6_periphery_instrument, vcov = "CR0", cluster = dat_Euro_periphery$ccode, test = "naive-t")

#Euro core
rg_determinants_v6_core_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + factor(ccode), data=dat_Euro_core)
summary(rg_determinants_v6_core_instrument)
#weak instrument test
summary(rg_determinants_v6_core_instrument, vcov = sandwich)
summary(rg_determinants_v6_core_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v6_core_instrument, vcov.=vcovHAC(rg_determinants_v6_core_instrument))

coef_test(rg_determinants_v6_core_instrument, vcov = "CR0", cluster = dat_Euro_core$ccode, test = "naive-t")

#standalone
rg_determinants_v7_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*StandaCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*StandaCrisis + factor(ccode), data=dat_standalone)
summary(rg_determinants_v7_instrument)
#weak instrument test
summary(rg_determinants_v7_instrument, vcov = sandwich)
summary(rg_determinants_v7_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v7_instrument, vcov.=vcovHAC(rg_determinants_v7_instrument))

coef_test(rg_determinants_v7_instrument, vcov = "CR0", cluster = dat_standalone$ccode, test = "naive-t")

#exclude US and Germany
rg_determinants_v9_instrument <- ivreg(rminusg~ lag(PDebt, 1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode), data=dat_exclude_US_DEU)
summary(rg_determinants_v9_instrument)
#weak instrument test
summary(rg_determinants_v9_instrument, vcov = sandwich)
summary(rg_determinants_v9_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v9_instrument, vcov.=vcovHAC(rg_determinants_v9_instrument))

#coef_test(rg_determinants_v9_instrument, vcov = "CR0", cluster = dat_exclude_US_DEU_5_year$ccode, test = "naive-t")


#
rg_determinants_v5_3_year_instrument <- ivreg(rminusg~ lag(PDebt,1) + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode) | PDebt_instrument + inflation + lag(real_LTrate, 1) + pop_growth + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis + factor(ccode), data=pdata_3_year)
summary(rg_determinants_v5_3_year_instrument)
#weak instrument test
summary(rg_determinants_v5_3_year_instrument, vcov = sandwich)
summary(rg_determinants_v5_3_year_instrument, vcov = sandwich, df = Inf, diagnostics = TRUE)
coeftest(rg_determinants_v5_3_year_instrument, vcov.=vcovHAC(rg_determinants_v5_3_year_instrument))

coef_test(rg_determinants_v5_3_year_instrument, vcov = "CR0", cluster = pdata_3_year$ccode, test = "naive-t")

#preparations for stargazer table
#preparation for stargazer tables
ses.rg_determinants_v2_instrument <- list(coef_test(rg_determinants_v2_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v2_instrument <- list(coef_test(rg_determinants_v2_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v2_instrument <- list(coef_test(rg_determinants_v2_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v3_instrument <- list(coef_test(rg_determinants_v3_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_instrument <- list(coef_test(rg_determinants_v3_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_instrument <- list(coef_test(rg_determinants_v3_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v4_instrument <- list(coef_test(rg_determinants_v4_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v4_instrument <- list(coef_test(rg_determinants_v4_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v4_instrument <- list(coef_test(rg_determinants_v4_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_instrument <- list(coef_test(rg_determinants_v5_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_instrument <- list(coef_test(rg_determinants_v5_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_instrument <- list(coef_test(rg_determinants_v5_instrument, vcov = "CR0", cluster = dat$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_instrument <- list(coef_test(rg_determinants_v6_instrument, vcov = "CR0", cluster = dat_Euro$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_instrument <- list(coef_test(rg_determinants_v6_instrument, vcov = "CR0", cluster = dat_Euro$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_instrument <- list(coef_test(rg_determinants_v6_instrument, vcov = "CR0", cluster = dat_Euro$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_instrument <- list(coef_test(rg_determinants_v6_periphery_instrument, vcov = "CR0", cluster = dat_Euro_periphery$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_instrument <- list(coef_test(rg_determinants_v6_periphery_instrument, vcov = "CR0", cluster = dat_Euro_periphery$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_instrument <- list(coef_test(rg_determinants_v6_periphery_instrument, vcov = "CR0", cluster = dat_Euro_periphery$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_instrument <- list(coef_test(rg_determinants_v6_core_instrument, vcov = "CR0", cluster = dat_Euro_core$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_instrument <- list(coef_test(rg_determinants_v6_core_instrument, vcov = "CR0", cluster = dat_Euro_core$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_instrument <- list(coef_test(rg_determinants_v6_core_instrument, vcov = "CR0", cluster = dat_Euro_core$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_instrument <- list(coef_test(rg_determinants_v7_instrument, vcov = "CR0", cluster = dat_standalone$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_instrument <- list(coef_test(rg_determinants_v7_instrument, vcov = "CR0", cluster = dat_standalone$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_instrument <- list(coef_test(rg_determinants_v7_instrument, vcov = "CR0", cluster = dat_standalone$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_instrument <- list(coef_test(rg_determinants_v9_instrument, vcov = "CR0", cluster = dat_exclude_US_DEU$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_instrument <- list(coef_test(rg_determinants_v9_instrument, vcov = "CR0", cluster = dat_exclude_US_DEU$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_instrument <- list(coef_test(rg_determinants_v9_instrument, vcov = "CR0", cluster = dat_exclude_US_DEU$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_3_year_instrument <- list(coef_test(rg_determinants_v5_3_year_instrument, vcov = "CR0", cluster = pdata_3_year$ccode, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_3_year_instrument <- list(coef_test(rg_determinants_v5_3_year_instrument, vcov = "CR0", cluster = pdata_3_year$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_3_year_instrument <- list(coef_test(rg_determinants_v5_3_year_instrument, vcov = "CR0", cluster = pdata_3_year$ccode, test = "naive-t")[,4]) # heteroskedasticity-robust p-val

#Table A7
#stargazer table for LaTEX
stargazer(rg_determinants_v3_instrument, rg_determinants_v5_instrument, rg_determinants_v9_instrument, rg_determinants_v5_3_year_instrument, rg_determinants_v6_instrument, rg_determinants_v7_instrument, rg_determinants_v6_periphery_instrument, rg_determinants_v6_core_instrument, t=list(unlist(tvals.rg_determinants_v3_instrument), unlist(tvals.rg_determinants_v5_instrument), unlist(tvals.rg_determinants_v9_instrument), unlist(tvals.rg_determinants_v5_3_year_instrument), unlist(tvals.rg_determinants_v6_instrument), unlist(tvals.rg_determinants_v7_instrument), unlist(tvals.rg_determinants_v6_periphery_instrument), unlist(tvals.rg_determinants_v6_core_instrument)), se=list(unlist(ses.rg_determinants_v3_instrument), unlist(ses.rg_determinants_v5_instrument), unlist(ses.rg_determinants_v9_instrument), unlist(ses.rg_determinants_v5_3_year_instrument), unlist(ses.rg_determinants_v6_instrument), unlist(ses.rg_determinants_v7_instrument), unlist(ses.rg_determinants_v6_periphery_instrument), unlist(ses.rg_determinants_v6_core_instrument)), p=list(unlist(pvals.rg_determinants_v3_instrument), unlist(pvals.rg_determinants_v5_instrument), unlist(pvals.rg_determinants_v9_instrument), unlist(pvals.rg_determinants_v5_3_year_instrument), unlist(pvals.rg_determinants_v6_instrument), unlist(pvals.rg_determinants_v7_instrument), unlist(pvals.rg_determinants_v6_periphery_instrument), unlist(pvals.rg_determinants_v6_core_instrument)))

#+safe haven, GDP size and financial openness
rg_determinants_v3_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety + fin_open | lag(rminusg,2) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + safety + fin_open, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat)
summary(rg_determinants_v3_GMM)
coeftest(rg_determinants_v3_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#Test for autocorrelation
mtest(rg_determinants_v3_GMM, order = 2L)
#Test for instrument validity
plm::sargan(rg_determinants_v3_GMM)

#+interaction safe_haven*EuroMember
rg_determinants_v4_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + fin_open | lag(rminusg,2) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + safety*EuroMember + fin_open, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat)
summary(rg_determinants_v4_GMM)
coeftest(rg_determinants_v4_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#+interaction safe_haven*EuroMember + interaction safe_haven*EuroCrisis
rg_determinants_v5_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis | lag(rminusg,2) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat)
summary(rg_determinants_v5_GMM)
coeftest(rg_determinants_v5_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#Euro
rg_determinants_v6_GMM <- pgmm(rminusg~ lag(rminusg)  + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis | lag(rminusg,2)  + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_Euro)
summary(rg_determinants_v6_GMM, toll=NULL)
coeftest(rg_determinants_v6_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#Euro periphery
rg_determinants_v6_periphery_GMM <- pgmm(rminusg~ lag(rminusg) + safety*EuroMember + safety*EuroCrisis | lag(rminusg,2)  + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_Euro_periphery)
summary(rg_determinants_v6_periphery_GMM)
coeftest(rg_determinants_v6_periphery_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#Euro core
rg_determinants_v6_core_GMM <- pgmm(rminusg~ lag(rminusg) + safety*EuroMember + safety*EuroCrisis | lag(rminusg,2) + safety*EuroMember + safety*EuroCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_Euro_core)
summary(rg_determinants_v6_core_GMM)
coeftest(rg_determinants_v6_core_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#standalone
rg_determinants_v7_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*StandaCrisis | lag(rminusg,1) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_standalone)
summary(rg_determinants_v7_GMM)
coeftest(rg_determinants_v7_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#10-year-ahead r-g 
rg_determinants_v8_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis | lag(rminusg,1) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat)
summary(rg_determinants_v8_GMM)
coeftest(rg_determinants_v8_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US and Germany
#dat_exclude_US_DEU_GMM <- subset(dat, ccode %in% c('AUT', 'BEL', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))
rg_determinants_v9_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + fin_open + safety*EuroMember + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis | lag(rminusg,1) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_exclude_US_DEU)
summary(rg_determinants_v9_GMM)
coeftest(rg_determinants_v9_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#exclude US
#dat_exclude_US <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v10_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open | lag(rminusg,1) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_exclude_US)
summary(rg_determinants_v10_GMM)
coeftest(rg_determinants_v10_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#exclude Germany
#dat_exclude_DEU <- subset(dat, ccode %in% c('AUT', 'BEL', 'USA', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT', 'AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE'))

rg_determinants_v11_GMM <- pgmm(rminusg~ lag(rminusg) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 1) + primaryFB + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis+ fin_open | lag(rminusg,1) + inflation + lag(real_LTrate, 1) + pop_growth + lag(PDebt, 2) + primaryFB + fin_open + safety*EuroMember + safety*EuroCrisis + safety*StandaCrisis, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_exclude_DEU)
summary(rg_determinants_v11_GMM)
coeftest(rg_determinants_v11_GMM, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.rg_determinants_v3_GMM <- list(coeftest(rg_determinants_v3_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v3_GMM <- list(coeftest(rg_determinants_v3_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v3_GMM <- list(coeftest(rg_determinants_v3_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v5_GMM <- list(coeftest(rg_determinants_v5_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v5_GMM <- list(coeftest(rg_determinants_v5_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v5_GMM <- list(coeftest(rg_determinants_v5_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_GMM <- list(coeftest(rg_determinants_v6_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_GMM <- list(coeftest(rg_determinants_v6_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_GMM <- list(coeftest(rg_determinants_v6_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_periphery_GMM <- list(coeftest(rg_determinants_v6_periphery_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_periphery_GMM <- list(coeftest(rg_determinants_v6_periphery_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_periphery_GMM <- list(coeftest(rg_determinants_v6_periphery_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v6_core_GMM <- list(coeftest(rg_determinants_v6_core_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v6_core_GMM <- list(coeftest(rg_determinants_v6_core_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v6_core_GMM <- list(coeftest(rg_determinants_v6_core_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v7_GMM <- list(coeftest(rg_determinants_v7_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v7_GMM <- list(coeftest(rg_determinants_v7_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v7_GMM <- list(coeftest(rg_determinants_v7_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.rg_determinants_v9_GMM <- list(coeftest(rg_determinants_v9_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.rg_determinants_v9_GMM <- list(coeftest(rg_determinants_v9_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.rg_determinants_v9_GMM <- list(coeftest(rg_determinants_v9_GMM, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX
stargazer(rg_determinants_v3_GMM, rg_determinants_v5_GMM, rg_determinants_v9_GMM,rg_determinants_v6_GMM, rg_determinants_v7_GMM, t=list(unlist(tvals.rg_determinants_v3_GMM), unlist(tvals.rg_determinants_v5_GMM), unlist(tvals.rg_determinants_v9_GMM), unlist(tvals.rg_determinants_v6_GMM), unlist(tvals.rg_determinants_v7_GMM)), se=list(unlist(ses.rg_determinants_v3_GMM), unlist(ses.rg_determinants_v5_GMM), unlist(ses.rg_determinants_v9_GMM), unlist(ses.rg_determinants_v6_GMM), unlist(ses.rg_determinants_v7_GMM)), p=list(unlist(pvals.rg_determinants_v3_GMM), unlist(pvals.rg_determinants_v5_GMM), unlist(pvals.rg_determinants_v9_GMM), unlist(pvals.rg_determinants_v6_GMM), unlist(pvals.rg_determinants_v7_GMM)))

#stargazer(rg_determinants_v3_GMM, rg_determinants_v5_GMM, rg_determinants_v9_GMM, rg_determinants_v5_3_year_GMM, rg_determinants_v6_GMM, rg_determinants_v7_GMM, rg_determinants_v6_periphery_GMM, rg_determinants_v6_core_GMM, t=list(unlist(tvals.rg_determinants_v3_GMM), unlist(tvals.rg_determinants_v5_GMM), unlist(tvals.rg_determinants_v9_GMM), unlist(tvals.rg_determinants_v5_3_year_GMM), unlist(tvals.rg_determinants_v6_GMM), unlist(tvals.rg_determinants_v7_GMM), unlist(tvals.rg_determinants_v6_periphery_GMM), unlist(tvals.rg_determinants_v6_core_GMM)), se=list(unlist(ses.rg_determinants_v3_GMM), unlist(ses.rg_determinants_v5_GMM), unlist(ses.rg_determinants_v9_GMM), unlist(ses.rg_determinants_v5_3_year_GMM), unlist(ses.rg_determinants_v6_GMM), unlist(ses.rg_determinants_v7_GMM), unlist(ses.rg_determinants_v6_periphery_GMM), unlist(ses.rg_determinants_v6_core_GMM)), p=list(unlist(pvals.rg_determinants_v3_GMM), unlist(pvals.rg_determinants_v5_GMM), unlist(pvals.rg_determinants_v9_GMM), unlist(pvals.rg_determinants_v5_3_year_GMM), unlist(pvals.rg_determinants_v6_GMM), unlist(pvals.rg_determinants_v7_GMM), unlist(pvals.rg_determinants_v6_periphery_GMM), unlist(pvals.rg_determinants_v6_core_GMM)))


#stargazer(rg_determinants_v3, rg_determinants_v5, rg_determinants_v9, rg_determinants_v5_3_year, rg_determinants_v6, rg_determinants_v7, rg_determinants_v6_periphery, rg_determinants_v6_core, t=list(unlist(tvals.rg_determinants_v3), unlist(tvals.rg_determinants_v5), unlist(tvals.rg_determinants_v9), unlist(tvals.rg_determinants_v5_3_year), unlist(tvals.rg_determinants_v6), unlist(tvals.rg_determinants_v7), unlist(tvals.rg_determinants_v6_periphery), unlist(tvals.rg_determinants_v6_core)), se=list(unlist(ses.rg_determinants_v3), unlist(ses.rg_determinants_v5), unlist(ses.rg_determinants_v9), unlist(ses.rg_determinants_v5_3_year), unlist(ses.rg_determinants_v6), unlist(ses.rg_determinants_v7), unlist(ses.rg_determinants_v6_periphery), unlist(ses.rg_determinants_v6_core)), p=list(unlist(pvals.rg_determinants_v3), unlist(pvals.rg_determinants_v5), unlist(pvals.rg_determinants_v9), unlist(pvals.rg_determinants_v5_3_year), unlist(pvals.rg_determinants_v6), unlist(pvals.rg_determinants_v7), unlist(pvals.rg_determinants_v6_periphery), unlist(pvals.rg_determinants_v6_core)))


