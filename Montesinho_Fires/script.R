library(psych)
library(ggplot2)
library(GGally)
library(corrplot)
library(heplots)
library(ICSNP)
library(aplpack)
library(mixtools)
library(scatterplot3d)
library(MVN)


fires <- read.csv("forestfires.csv")

n <- nrow(fires)    # number of rows
head(fires)
str(fires)

# convert variables to factors
fires$month <- as.factor(fires$month)
fires$day <- as.factor(fires$day)
fires$RH <- as.numeric(fires$RH)
# create season variable based on month
fires$season <-numeric(nrow(fires)) 
fires$season[fires$month=="jul" | fires$month=="jun" | fires$month=="aug"] <- "summer"
fires$season[fires$month=="mar" | fires$month=="apr" | fires$month=="may"] <- "spring"
fires$season[fires$month=="dec" | fires$month=="jan" | fires$month=="feb"] <- "winter"
fires$season[fires$month=="sep" | fires$month=="oct" | fires$month=="nov"] <- "autumn"
fires$season <- as.factor(fires$season)    # convert to factor

# Scatter matrix

# pairwise correlations
pairs.panels(fires[,c("FFMC","DMC","ISI","DC","temp","RH","wind","rain","area")],cex.labels = 0.6,cex.cor = 2)


# colors by season
cols <- as.numeric(as.factor(fires$season))
# Scale point sizes based on burned area
cex_vals <- sqrt(fires$area)
cex_vals <- cex_vals / max(cex_vals) *2 + 0.7
# Pairwise scatterplots
# i
pairs(fires[,c("FFMC","DMC","ISI","DC","temp","RH","wind","rain")],col=cols,
      upper.panel = NULL,cex=cex_vals)  

par(xpd = NA)
legend("topright",
       legend = levels(fires$season),
       col = 1:length(levels(fires$season)),pch=19,cex=0.7)  

par(xpd=FALSE)  
  
# ii
ggpairs(fires,
        columns = c("FFMC","DMC","ISI","DC","temp","RH"),
        aes(color = season, size = area),
        upper = list(
          continuous = wrap("cor", size = 3)
        ))


# Correlogram

corrplot(cor(fires[, c("FFMC","DMC","ISI","DC","temp","RH","wind","rain","area")]),
         method = "color",
         type = "upper",
         addCoef.col = "black")


# Histogram

# RH vs temo
ggplot(fires, aes(x = RH, y = temp)) +
  geom_hex(bins = 30) +
  scale_fill_gradient2(low = "white", mid = "grey70", high = "black") +
  theme_minimal() +
  labs(x = "RH",
    y = "temp",
    fill = "Number\nof fires"
  )

# FFMC vs ISI
ggplot(fires, aes(x = FFMC, y = ISI)) +
  geom_hex(bins = 30) +
  scale_fill_gradient2(low = "white", mid = "grey70", high = "black") +
  theme_minimal() +
  labs(x = "FFMC",
       y = "ISI",
       fill = "Number\nof fires"
  )

# DMC vs ISI
ggplot(fires, aes(x = DMC, y = ISI)) +
  geom_hex(bins = 30) +
  scale_fill_gradient2(low = "white", mid = "grey70", high = "black") +
  theme_minimal() +
  labs(x = "DMC",
       y = "ISI",
       fill = "Number\nof fires"
  )


# Bagplot

par(mar=c(5, 5, 4, 2) + 0.1)

# temp vs RH
bagplot(cbind(fires$temp,fires$RH),factor=2.5,create.plot=TRUE,approx.limit=300,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=TRUE,
        show.baghull=TRUE,verbose=FALSE,
        ylim=c(0,100),xlim=c(0,40),
        xlab="temp",ylab="RH",bty="L",
        main = "Bagplot of temp vs RH")

# FFMC vs ISI
bagplot(cbind(fires$FFMC,fires$ISI),factor=2.5,create.plot=TRUE,approx.limit=300,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=TRUE,
        ylim=c(0,60),xlim=c(10,100),
        show.baghull=TRUE,verbose=FALSE,
        xlab="FFMC",ylab="ISI",bty="L",
        main = "Bagplot of FFMC vs ISI")

# temp vs DMC
bagplot(cbind(fires$temp,fires$DMC),factor=2.5,create.plot=TRUE,approx.limit=300,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=TRUE,
        show.baghull=TRUE,verbose=FALSE,
        xlab="temp",ylab="DMC",bty="L",
        main = "Bagplot of temp vs DMC")


# 3D Scatterplot

# temp - RH - DC
scatterplot3d(
  fires$temp, fires$RH, fires$DC,
  color = cols,
  pch = 20,
  col.axis = "blue",
  col.grid = "lightblue",
  xlab = "temp",
  ylab = "RH",
  zlab = "DC",
  main = "3D scatterplot: temp - DMC - DC"
)

legend("topright", 
       legend = levels(fires$season), 
       col = 1:length(levels(fires$season)),             
       pch = 20, bty="n",
       inset = c(-0.09, 0.1),
       xpd = TRUE)

# FFMC - DC - ISI
scatterplot3d(
  fires$DC, fires$FFMC, fires$ISI,
  color = cols,
  pch = 20,
  col.axis = "blue",
  col.grid = "lightblue",
  xlab = "DC",
  ylab = "FFMC",
  zlab = "ISI",
  main = "3D scatterplot: ISI - FFMC - DC"
)

legend("topright", 
       legend = levels(fires$season), 
       col = 1:length(levels(fires$season)),             
       pch = 20, bty = "n",
       inset = c(-0.09, 0.15),
       xpd = TRUE)


# Andrews Curves

andrews<-function(X, colors){
  # x is the matrix with the data, we assume that they are in the 
  # correct order with respect the variances.
  if (is.matrix(X)){
    m <- dim(X)[1]
    n <- dim(X)[2]
  }
  else{
    stop("X must be a matrix!")
  }
  p<-dim(X)[2]
  samplesize<-dim(X)[1]
  if (p%%2==0) {
    X<-cbind(X,0)
    p<-p+1}
  t<-seq(-3.14159,3.14159,length=201)
  told<-t
  n<-length(t)
  andr<-matrix(NA,length(t),dim(X)[1])
  
  for (i in 1:n) {
    t<-told
    temp<-  X[,1]/sqrt(2)
    j<-2
    while (j<=p) {
      temp <- temp +  X[ , j] *  sin(t[i]) + X[ , j+1] *  cos(t[i])
      j<-j+2
      t<-t + told
    }
    andr[i,]<-temp
  }
  oria<-c(min(andr),max(andr))
  plot(told,andr[,1],ylim=oria,xlab="t",ylab="f(t)",type="l",lwd=2,col=colors[1])
  for (i in 2:samplesize) {
    lines(told,andr[,i],lwd=2,col=colors[i]) 
  }
}

# Mean values by season
mean_season <- aggregate(.~season, data = data.frame(season=fires$season,scale(fires[,c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind")])),mean)

# Andrews curves plot
andrews(as.matrix(mean_season[,-1]),c("red", "purple", "yellow", "green"))

legend("bottomright", 
       legend = mean_season$season, 
       col = c("red", "purple", "yellow", "green"), 
       lwd = 2,cex = 0.5,box.lty = 0)

title("Andrews Curves by season")


# Descriptive Statistics

describe(fires[,sapply(fires,class)=="numeric"])
describeBy(fires[, c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area")], group = fires$season)

# Mean values
apply(fires[,c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind","rain","area")],2,mean)

# Mean values by season
mean_season

# Covariance matrix 

S <- round((n-1)/n*cov(fires[,sapply(fires,class)=="numeric"]),2)
diag(S) <- round(apply(fires[,sapply(fires,class)=="numeric"],2,var)/(n-1)*n,2) 
# Total variation
tr(S)
# Generalized variance
det(S)

# Correlation matrix
R <- round(cor(fires[,sapply(fires,class)=="numeric"]),2)

# Correlation matrix by season
by(fires[, c("FFMC","DMC","ISI","DC","temp","RH")], fires$season, cor)


# skewness and kurtosis 

mardia <- function(X){
  # Computes Mardia's measures for multivariate
  # skewness and kurtosis
  # X must be a matrix
  if (is.matrix(X)){
    m <- dim(X)[1]
    n <- dim(X)[2]
  }
  else{
    stop("X must be a matrix!")
  }
  # Calculation of VCV matrix. Q is projection matrix, cov is the maximum likelihood estimate of
  # the VCV matrix. I is a m by 1 column vector with all entries being 1.     g is a m by m matrix.
  I <- matrix(1, nrow=m, ncol=1)
  Q <- diag(m)-(1/m)*(I%*%t(I))
  cov <- (1/m)*(t(X)%*%Q%*%X)
  # Needed for calculating the multivariate skewness and multivariate kurtosis.
  g <-Q%*%X%*%solve(cov)%*%t(X)%*%Q
  # Calculation of mardias multivariate skewness measure
  MultiSkewness <- (sum(g^3))/(m*m)
  # Calculation of mardias multivariate kurtosis measure
  # MultiKurtosis <- sum((g*diag(m))^2)/m
  MultiKurtosis <- sum(diag(g)^2)/m
  list(skewness=MultiSkewness, kurtosis=MultiKurtosis)
}

# apply function to numeric variables
mardia(as.matrix(fires[,sapply(fires,class)=="numeric"]))



# 2
# H0: mu(temp, RH, wind, rain) = (19, 45, 4, 2)

p <- 4     # number of variables
m0 <- c(19, 45, 4, 2)
# sample mean 
x.bar <- apply(fires[,c("temp","RH","wind","rain")],2,mean)
# sample covariance matrix
S <- (n-1)*cov(fires[,c("temp","RH","wind","rain")])
Sigma.hat <- S/n
# Hotelling's T^2 
T2 <- ((n-p)/p)*t(x.bar-m0)%*%solve(Sigma.hat)%*%(x.bar-m0)
qf(0.965, p,n-p) # critical value
1-pf(T2, p, n-p) # p-value

# built-in Hotelling test
HotellingsT2(fires[,c("temp","RH","wind","rain")],  mu = m0, conf.level = 0.965)

# Normality assumptions
# QQ plot for temp
qqnorm(fires$temp, main = "Normal Q-Q plot of temp")
qqline(fires$temp,col="red")
shapiro.test(fires$temp)  # Shapiro-Wilk test for temp

# QQ plot for RH
qqnorm(fires$RH, main = "Normal Q-Q plot of RH")
qqline(fires$RH,col="red")
shapiro.test(fires$RH)  # Shapiro-Wilk test for RH

# QQ plot for wind
qqnorm(fires$wind, main = "Normal Q-Q plot of wind")
qqline(fires$wind,col="red")
shapiro.test(fires$wind)  # Shapiro-Wilk test for wind

# QQ plot for rain
qqnorm(fires$rain, main = "Normal Q-Q plot of rain")
qqline(fires$rain,col="red")
shapiro.test(fires$rain)  # Shapiro-Wilk test for rain

# Skewness & Kurtosis
mardia(as.matrix(fires[, c("temp","RH","wind","rain")]))
# mardia Test
MVN::mardia(as.matrix(fires[, c("temp","RH","wind","rain")]))

# multivariate normality check
r <- numeric(n)
for(i in 1:n){
  x <- as.numeric(fires[i, c("temp", "RH", "wind", "rain")])
  r[i] <- ((n-p)/(n*p))*t(x-x.bar)%*%solve(S)%*%(x-x.bar)
}

# theoretical F quantiles
f_theor <- qf(seq(0,0.99,length=n),df1=p,df2=n)
qqplot(f_theor,r)   # QQ-plot observed vs theoretical





# 3

# create week variable (weekend - weekday)
fires$week <- numeric(n)
fires$week[fires$day %in% c("sun","sat")] <- "weekend"
fires$week[fires$week==0] <- "weekday"
fires$week <- as.factor(fires$week)

# group means
aggregate(cbind(FFMC, DMC, DC, ISI) ~ week, data = fires, mean)
x.bar_weekend <- aggregate(cbind(FFMC, DMC, DC, ISI) ~ week, data = fires, mean)[2,-1]
x.bar_weekday <- aggregate(cbind(FFMC, DMC, DC, ISI) ~ week, data = fires, mean)[1,-1]

# Box-M test for homogeneity of covariance matrices
boxM(Y = fires[, c("FFMC", "DMC", "DC", "ISI")], group = fires$week)

# MANOVA
man <- manova(cbind(FFMC, DMC, DC, ISI) ~ week, data = fires)
# Pillai
summary(man, test = "Pillai")
# critical value
qf(p = 1 - 0.035, df1 = 4, df2 = 512)
# Hotellings
HotellingsT2(fires[fires$week=="weekend",c("FFMC","DMC","DC","ISI")], fires[fires$week=="weekday",c("FFMC","DMC","DC","ISI")], conf.level = 0.965)


# Normality assumptions
# QQ plot for FFMC
qqnorm(fires$FFMC, main = "Normal Q-Q plot of FFMC")
qqline(fires$FFMC,col="red")
shapiro.test(fires$FFMC)  # Shapiro-Wilk test for FFMC

# QQ plot for DMC
qqnorm(fires$DMC, main = "Normal Q-Q plot of DMC")
qqline(fires$DMC,col="red")
shapiro.test(fires$DMC)  # Shapiro-Wilk test for DMC

# QQ plot for DC
qqnorm(fires$DC, main = "Normal Q-Q plot of DC")
qqline(fires$DC,col="red")
shapiro.test(fires$DC)  # Shapiro-Wilk test for DC

# QQ plot for ISI
qqnorm(fires$ISI, main = "Normal Q-Q plot of ISI")
qqline(fires$ISI,col="red")
shapiro.test(fires$ISI)  # Shapiro-Wilk test for ISI

# Skewness & Kurtosis
mardia(as.matrix(fires[, c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[, c("FFMC", "DMC", "DC", "ISI")]))

# weekday
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$week=="weekday", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$week=="weekday", c("FFMC", "DMC", "DC", "ISI")]))

# weekend
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$week=="weekend", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$week=="weekend", c("FFMC", "DMC", "DC", "ISI")]))



# 4

# group means by season
x.bar_autumn <- apply(fires[,c("FFMC","DMC","DC","ISI")],2,function(x){by(x,fires$season,mean)})["autumn",]
x.bar_summer <- apply(fires[,c("FFMC","DMC","DC","ISI")],2,function(x){by(x,fires$season,mean)})["summer",]
x.bar_winter <- apply(fires[,c("FFMC","DMC","DC","ISI")],2,function(x){by(x,fires$season,mean)})["winter",]
x.bar_spring <- apply(fires[,c("FFMC","DMC","DC","ISI")],2,function(x){by(x,fires$season,mean)})["spring",]

# Box-M test for homogeneity of covariance matrices
boxM(Y = fires[, c("FFMC", "DMC", "DC", "ISI")], group = fires$season)

# MANOVA
man <- manova(cbind(FFMC, DMC, DC, ISI) ~ season, data = fires)
# Pillai
summary(man, test = "Pillai")
# critical value
qf(p = 1 - 0.035, df1 = 12, df2 = 1536)

# Normality assumptions

# Skewness & Kurtosis
mardia(as.matrix(fires[, c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[, c("FFMC", "DMC", "DC", "ISI")]))

# autumn
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$season=="autumn", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$season=="autumn", c("FFMC", "DMC", "DC", "ISI")]))

# summer
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$season=="summer", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$season=="summer", c("FFMC", "DMC", "DC", "ISI")]))

# winter
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$season=="winter", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$season=="winter", c("FFMC", "DMC", "DC", "ISI")]))

# spring
# Skewness & Kurtosis
mardia(as.matrix(fires[fires$season=="spring", c("FFMC", "DMC", "DC", "ISI")]))
# mardia Test
MVN::mardia(as.matrix(fires[fires$season=="spring", c("FFMC", "DMC", "DC", "ISI")]))




# 5

# FFMC - DMC
Sigma_autumn <- cov(fires[fires$season=="autumn",c("FFMC","DMC","DC","ISI")])
Sigma_summer <- cov(fires[fires$season=="summer",c("FFMC","DMC","DC","ISI")])
Sigma_spring <- cov(fires[fires$season=="spring",c("FFMC","DMC","DC","ISI")])
Sigma_winter <- cov(fires[fires$season=="winter",c("FFMC","DMC","DC","ISI")])

plot(fires$FFMC,fires$DMC,main = "FFMC vs DMC (95% Confidence Ellipsoid by season)",
     xlab = "FFMC",
     ylab = "DMC",cex=0.8)

ellipse(x.bar_autumn[1:2],Sigma_autumn[1:2,1:2],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[1:2],Sigma_summer[1:2,1:2],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[1:2],Sigma_spring[1:2,1:2],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[1:2],Sigma_winter[1:2,1:2],alpha = 0.05,col="green",lwd=2)

legend("topleft",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.7,box.lty = 0)

# FFMC - DC

plot(fires$FFMC,fires$DC,main = "FFMC vs DC (95% Confidence Ellipsoid by season)",
     xlab = "FFMC",
     ylab = "DC",cex=0.8)

ellipse(x.bar_autumn[c(1,3)],Sigma_autumn[c(1,3),c(1,3)],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[c(1,3)],Sigma_summer[c(1,3),c(1,3)],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[c(1,3)],Sigma_spring[c(1,3),c(1,3)],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[c(1,3)],Sigma_winter[c(1,3),c(1,3)],alpha = 0.05,col="green",lwd=2)

legend("topleft",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.6,box.lty = 0)

# FFMC - ISI

plot(fires$FFMC,fires$ISI,main = "FFMC vs ISI (95% Confidence Ellipsoid by season)",
     xlab = "FFMC",
     ylab = "ISI",cex=0.8)

ellipse(x.bar_autumn[c(1,4)],Sigma_autumn[c(1,4),c(1,4)],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[c(1,4)],Sigma_summer[c(1,4),c(1,4)],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[c(1,4)],Sigma_spring[c(1,4),c(1,4)],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[c(1,4)],Sigma_winter[c(1,4),c(1,4)],alpha = 0.05,col="green",lwd=2)

legend("topleft",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.7,box.lty = 0)

# DMC - DC

plot(fires$DMC,fires$DC,main = "DMC vs DC (95% Confidence Ellipsoid by season)",
     xlab = "DMC",
     ylab = "DC",cex=0.8)

ellipse(x.bar_autumn[c(2,3)],Sigma_autumn[c(2,3),c(2,3)],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[c(2,3)],Sigma_summer[c(2,3),c(2,3)],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[c(2,3)],Sigma_spring[c(2,3),c(2,3)],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[c(2,3)],Sigma_winter[c(2,3),c(2,3)],alpha = 0.05,col="green",lwd=2)

legend("bottomright",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.6,box.lty = 0)

# DMC - ISI

plot(fires$DMC,fires$ISI,main = "DMC vs ISI (95% Confidence Ellipsoid by season)",
     xlab = "DMC",
     ylab = "ISI",cex=0.8)

ellipse(x.bar_autumn[c(2,4)],Sigma_autumn[c(2,4),c(2,4)],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[c(2,4)],Sigma_summer[c(2,4),c(2,4)],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[c(2,4)],Sigma_spring[c(2,4),c(2,4)],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[c(2,4)],Sigma_winter[c(2,4),c(2,4)],alpha = 0.05,col="green",lwd=2)

legend("topright",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.7,box.lty = 0)

# DC - ISI

plot(fires$DC,fires$ISI,main = "DC vs ISI (95% Confidence Ellipsoid by season)",
     xlab = "DC",
     ylab = "ISI",cex=0.8)

ellipse(x.bar_autumn[c(3,4)],Sigma_autumn[c(3,4),c(3,4)],alpha = 0.05,col="red",lwd=2)
ellipse(x.bar_summer[c(3,4)],Sigma_summer[c(3,4),c(3,4)],alpha = 0.05,col="purple",lwd=2)
ellipse(x.bar_spring[c(3,4)],Sigma_spring[c(3,4),c(3,4)],alpha = 0.05,col="yellow",lwd=2)
ellipse(x.bar_winter[c(3,4)],Sigma_winter[c(3,4),c(3,4)],alpha = 0.05,col="green",lwd=2)

legend("topright",
       legend = c("Autumn", "Summer", "Spring", "Winter"),
       col = c("red", "purple", "yellow", "green"),
       lwd = 2,cex=0.7,box.lty = 0)


