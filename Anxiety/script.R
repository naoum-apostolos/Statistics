setwd("C:/Users/naoum/OneDrive/Documents/Στατιστικη/Πολυμεταβλητή Ανάλυση/ergasies/Εργασία 2")

library(readxl)
library(corrgram)
library(nortest)
library(psych)

data_beck <- read_excel("data_beck.xls")

X <- data_beck[,2:22]

###  PCA
  
corrgram(X)
round(cor(X),3)
eigen(cov(X))   # eigen values and eigenvectors from covariance matrix
eigen(cor(X))     # eigen values and eigenvectors from correlation matrix

eigen(cov(X))$values 
cumsum(prop.table(eigen(cov(X))$values))
eigen(cor(X))$values 
cumsum(prop.table(eigen(cor(X))$values))

### usage of covariance matrix 

mymodel<-princomp(X, scores=TRUE)

## see other options using ?princomp

mymodel
mymodel$loadings
print(mymodel$loadings, cutoff=0.3)
## cut off selects which values to hide
mymodel$scores  # the new variables: the principal components

a1<-solve(mymodel$loadings)
nX<-mymodel$scores%*%a1  #fully reconstruct the data
plot(X$A101,nX[,1]) 
plot(X$A102,nX[,2])

### note the mean value is different since we do not use the
### standadized values

##keep less PC
### keep only 3
nX<-mymodel$scores[,-c(4:21)]%*%a1[-c(4:21),]
plot(X$A101,nX[,1])

### keep only 4
nX<-mymodel$scores[,-c(5:21)]%*%a1[-c(5:21),]
plot(X$A101,nX[,1])

### keep only 5
nX<-mymodel$scores[,-c(6:21)]%*%a1[-c(6:21),]
plot(X$A101,nX[,1])

### keep only 6
nX<-mymodel$scores[,-c(7:21)]%*%a1[-c(7:21),]
plot(X$A101,nX[,1])

### keep only 7
nX<-mymodel$scores[,-c(8:21)]%*%a1[-c(8:21),]
plot(X$A101,nX[,1])

### keep only 8
nX<-mymodel$scores[,-c(9:21)]%*%a1[-c(9:21),]
plot(X$A101,nX[,1])

### keep only 9
nX<-mymodel$scores[,-c(10:21)]%*%a1[-c(10:21),]
plot(X$A101,nX[,1])

### keep only 10
nX<-mymodel$scores[,-c(11:21)]%*%a1[-c(11:21),]
plot(X$A101,nX[,1])

### keep only 11
nX<-mymodel$scores[,-c(12:21)]%*%a1[-c(12:21),]
plot(X$A101,nX[,1])

### keep only 12
nX<-mymodel$scores[,-c(13:21)]%*%a1[-c(13:21),]
plot(X$A101,nX[,1])



###
cumsum(prop.table(eigen(cov(X))$values))


# Kaiser
sum(eigen(cov(X))$values > mean(eigen(cov(X))$values))


# broken stick
p <- ncol(X)
g <- numeric(p)

for(i in 1:p){
  g[i] <- 1/p*sum(1/(i:p))
}

prop.table(eigen(cov(X))$values) > g


# scree plot
screeplot(mymodel,type="lines")



#### some bootstrap to show the variability of the eigenvalues
res<-NULL
B<-1000
for (i in 1:B) {
  ind<-sample(1:nrow(X),nrow(X),replace=TRUE)
  bootX<- X[ind,]
  res<-rbind(res,eigen(cov(bootX))$values)
}

apply(res,2,function(x){quantile(x,probs=c(0.025,0.975))})
mean(eigen(cov(X))$values)


# or with correlation matrix
res<-NULL
B<-1000
for (i in 1:B) {
  ind<-sample(1:nrow(X),nrow(X),replace=TRUE)
  bootX<- X[ind,]
  res<-rbind(res,eigen(cor(bootX))$values)
}

apply(res,2,function(x){quantile(x,probs=c(0.025,0.975))})



### plot the PC
boxplot(mymodel$scores[,1],mymodel$scores[,2],
        mymodel$scores[,3],mymodel$scores[,4],
        mymodel$scores[,5],mymodel$scores[,6],
        mymodel$scores[,7],mymodel$scores[,8],
        mymodel$scores[,9],mymodel$scores[,10],
        mymodel$scores[,11],mymodel$scores[,12],
        mymodel$scores[,13],mymodel$scores[,14],
        mymodel$scores[,15],mymodel$scores[,16],
        mymodel$scores[,17],mymodel$scores[,18],
        mymodel$scores[,19],mymodel$scores[,20],
        mymodel$scores[,21])





##################################################
##################################################

k <- 6

PCA_scores <- mymodel$scores[,1:k]

demographics <- data_beck[,c("A301_01", "A302", "A303", "A307", "A308", "A309", "A310", "A311_01", "A312_01", "A313")]

df <- data.frame(PCA_scores, demographics)

weighted_scores <- PCA_scores %*% prop.table(eigen(cov(X))$values)[1:k]
hist(weighted_scores)

df$weighted_scores <- weighted_scores



###  Factor Analysis

# Correlation
round(cor(X),3)
corrgram(X)

# Kaiser-Meyer-Olkin
KMO(X)
# Bartlett test
cortest.bartlett(X)


# Remove A111 and A119
X <- X[,-c(11,19)]
p <- ncol(X)
n <- nrow(X)

# Correlation
round(cor(X),3)
corrgram(X)

# Kaiser-Meyer-Olkin
KMO(X)
# Bartlett test
cortest.bartlett(X)

# Estimate model with ML
k <- 4

mymodel<- factanal(X, factors=k, rotation = "varimax", scores = "regression")
mymodel
# Loadings of factors
print(mymodel$loadings,digits=3,cutoff=0.3)

# Estimate correlation matrix
fittedcor<-(mymodel$loadings)%*%t(mymodel$loadings)+ diag(mymodel$uniqueness)
observed<-cor(X)
# deviation of the estimate
round(fittedcor-observed,3)

corrgram(fittedcor)
corrgram(observed)

# plot loadings
plot(mymodel$loadings)
# scores
mymodel$scores


##################

# Akaike Information Criterion

# log-likelihood
loglik <- -n/2*(p*log(2*pi)+log(det(fittedcor)) + tr(solve(fittedcor)%*%cov(X)))
# number of free parameters
params_k <- (p * k) + p - ((k^2 - k) / 2)
# AIC
aic <- -2*loglik + 2*params_k

# deviation of the estimate
sum(abs(fittedcor - observed) > 0.05)


###################

# Comparison with demographic characteristics

df <- data.frame(mymodel$scores,demographics)


## age 

# factor 1
plot(df$Factor1, df$A301_01)
cor(df$Factor1, df$A301_01)

# factor 2
plot(df$Factor2, df$A301_01)
cor(df$Factor2, df$A301_01)

# factor 3
plot(df$Factor3, df$A301_01)
cor(df$Factor3, df$A301_01)

# factor 4
plot(df$Factor4, df$A301_01)
cor(df$Factor4, df$A301_01)


## gender 

table(df$A302)
wilcox.test(df$Factor2 ~ df$A302)

boxplot(Factor2 ~ A302, data = df)


## year of study
table(df$A303)

anova_result <- aov(df$Factor1 ~ df$A303)
shapiro.test(anova_result$residuals)

qqnorm(anova_result$residuals)
qqline(anova_result$residuals)

kruskal.test(df$Factor1, df$A303)


## average score
plot(df$Factor1,df$A311_01)
cor(df$Factor1,df$A311_01)


# I feel the need to consult a mental health professional
table(df$A313)

anova_result <- aov(df$Factor1 ~ df$A313)

shapiro.test(anova_result$residuals)
qqnorm(anova_result$residuals)
qqline(anova_result$residuals)

summary(anova_result)
kruskal.test(df$Factor1, df$A313)

boxplot(Factor1 ~ A313, data = df)

pairwise.t.test(df$Factor1, df$A313)


# Total hours of lecture attendance
table(df$A310)

anova_result <- aov(df$Factor4 ~ df$A310)
shapiro.test(anova_result$residuals)

kruskal.test(df$Factor4, df$A310)
boxplot(df$Factor4, df$A310)


# degree
plot(df$Factor1,df$A312_01)
cor(df$Factor1,df$A312_01)


# How many hours do you spend online
table(df$A308)

anova_result <- aov(df$Factor1 ~ df$A308)
summary(anova_result)
shapiro.test(anova_result$residuals)

kruskal.test(df$Factor4, df$A308)
boxplot(df$Factor4, df$A308)


# parents' educational level
table(df$A307)

kruskal.test(df$Factor4, df$A307)
boxplot(df$Factor4 ~ df$A307)


# Are you working during this period
table(df$A309)

anova_result <- aov(df$Factor3 ~ df$A309)
summary(anova_result)
shapiro.test(anova_result$residuals)

kruskal.test(df$Factor3, df$A309)
boxplot(df$Factor3 ~ df$A309)


