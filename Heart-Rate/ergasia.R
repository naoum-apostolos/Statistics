
library(haven)
library(psych) 
library(nortest)
library(Hmisc)
library(car)
library(gplots)
library(ggplot2)

heartrate <- read_sav("08_heartrate.sav") 

# Check type of data and make any changes
head(heartrate)
str(heartrate)  

heartrate$Gender <- factor(heartrate$Gender)
heartrate$mi <- factor(heartrate$mi)
heartrate$smoker <- factor(heartrate$smoker)
heartrate$asthma <- factor(heartrate$asthma)
heartrate$oralhypoglcemic <- factor(heartrate$oralhypoglcemic)
heartrate$vitMulti <- factor(heartrate$vitMulti)
heartrate$white <- factor(heartrate$white)
heartrate$famhyper <- factor(heartrate$famhyper)
heartrate$typediabetes <- factor(heartrate$typediabetes)


# Descriptive statistics for quantitative variables
num <- which(sapply(heartrate,class)=="numeric")
heartrate_num <- heartrate[,num]

sapply(heartrate_num,summary)
psych::describe(heartrate_num)

# Plots for quantitative variables
# weight
ggplot(heartrate_num, aes(x = weight)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15, fill = "lightblue",color = "white") +
  labs(title = "Histogram of weight",
       x = "weight",
       y = "Density") + theme_minimal()

ggplot(heartrate_num, aes(y = weight)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of weight",
       y = "weight") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# HeartRate
ggplot(heartrate_num, aes(x = HeartRate)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15, fill = "lightblue",color = "white") +
  labs(title = "Histogram of HeartRate",
       x = "HeartRate",
       y = "Density") + theme_minimal()

ggplot(heartrate_num, aes(y = HeartRate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of HeartRate",
       y = "HeartRate") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# height
ggplot(heartrate_num, aes(x = height)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15, fill = "lightblue",color = "white") +
  labs(title = "Histogram of height",
       x = "height",
       y = "Density") + theme_minimal()

ggplot(heartrate_num, aes(y = height)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of height",
       y = "height") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# age 
ggplot(heartrate_num, aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 15, fill = "lightblue",color = "white") +
  labs(title = "Histogram of age",
       x = "age",
       y = "Density") + theme_minimal()

ggplot(heartrate_num, aes(y = age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of age",
       y = "age") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


# Descriptive statistics for qualitative variables
categ <- which(sapply(heartrate,class)=="factor")
heartrate_categ <- heartrate[,categ]
freq <- sapply(heartrate_categ,table)   # frequency tables
rel_freq <- lapply(freq,prop.table)   # relative frequency tables

# Plots for qualitative variables
# Function that returns a barplot of qualitative variables 
brpl <- function(data, variable) {
  df <- as.data.frame(prop.table(table(data[[variable]])))
  colnames(df) <- c("Category", "freq")
  
  ggplot(df, aes(x = Category, y = freq)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = paste(round(freq*100,1), "%")),
              vjust = -0.3) +
    labs(title = paste("Barplot of", variable),
         x = variable,
         y = "Relative Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

brpl(heartrate,"Gender")

brpl(heartrate,"mi")

brpl(heartrate,"smoker")

brpl(heartrate,"asthma")

brpl(heartrate,"oralhypoglcemic")

brpl(heartrate,"vitMulti")

brpl(heartrate,"white")

brpl(heartrate,"typediabetes")

brpl(heartrate,"famhyper")


# Function that returns a grouped barplot of relative frequencies for two categorical variables
brpl_2 <- function(tab) {
  
  df <- as.data.frame(tab)
  colnames(df) <- c("Var1", "Var2", "freq")
  
  ggplot(df, aes(x = Var2, y = freq, fill = Var1)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(names(dimnames(tab))[2], "~", names(dimnames(tab))[1]),
         x = names(dimnames(tab))[2],
         y = "Relative Frequency",
         fill = names(dimnames(tab))[1]) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_fill_manual(values = c("#a6cee3", "#fb9a99"))
}

# Relationships between quantitative variables
# Scatterplots and Pearson correlation coefficients

# HeartRate vs weight
plot(heartrate_num$weight,heartrate_num$HeartRate,xlab="weight",ylab="HeartRate",main="HeartRate vs weight")
cor(heartrate$weight,heartrate$HeartRate)

# HeartRate vs height
plot(heartrate_num$height,heartrate_num$HeartRate,xlab="height",ylab="HeartRate",main="HeartRate vs height")
cor(heartrate$height,heartrate$HeartRate)

# HeartRate vs age
plot(heartrate_num$age,heartrate_num$HeartRate,xlab="age",ylab="HeartRate",main="HeartRate vs age")
cor(heartrate$age,heartrate$HeartRate)

# height vs weight
plot(heartrate_num$weight,heartrate_num$height,xlab="weight",ylab="height",main="height vs weight")
cor(heartrate$weight,heartrate$height)

# height vs age
plot(heartrate_num$age,heartrate_num$height,xlab="age",ylab="height",main="height vs age")
cor(heartrate$age,heartrate$height)

# age vs weight
plot(heartrate_num$age,heartrate_num$weight,xlab="age",ylab="weight",main="weight vs age")
cor(heartrate$age,heartrate$weight)


# Relationships between quantitative and categorical variables

bxpl <- function(name1, name2, data = heartrate) {
  
  df <- data.frame(
    y = unlist(data[, name1]),
    x = factor(unlist(data[, name2]))
  )
  
  means <- aggregate(y ~ x, data = df, FUN = mean)
  
  ggplot(df, aes(x = x, y = y, fill = x)) +
    geom_boxplot(alpha = 0.7) +
    geom_point(data = means, aes(x = x, y = y),
               color = "red", size = 3, shape = 16) +
    labs(title = paste(name1, "~", name2),
         x = name2,
         y = name1,
         fill = name2) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
}

# Boxplots for each quantitative variable against all categorical variables
# weight
bxpl(names(heartrate_num)[1],names(heartrate_categ)[1])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[2])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[3])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[4])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[5])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[6])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[7])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[8])
bxpl(names(heartrate_num)[1],names(heartrate_categ)[9])

# HeartRate
bxpl(names(heartrate_num)[2],names(heartrate_categ)[1])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[2])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[3])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[4])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[5])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[6])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[7])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[8])
bxpl(names(heartrate_num)[2],names(heartrate_categ)[9])

# height
bxpl(names(heartrate_num)[3],names(heartrate_categ)[1])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[2])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[3])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[4])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[5])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[6])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[7])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[8])
bxpl(names(heartrate_num)[3],names(heartrate_categ)[9])

# age
bxpl(names(heartrate_num)[4],names(heartrate_categ)[1])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[2])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[3])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[4])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[5])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[6])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[7])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[8])
bxpl(names(heartrate_num)[4],names(heartrate_categ)[9])



# b

# HeartRate ~ age

# Scatterplot of Heartrate vs age
ggplot(heartrate, aes(x = age, y = HeartRate)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "HeartRate vs Age",
       x = "Age",
       y = "HeartRate") +
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5))

# Check normality of HeartRate
qqnorm(heartrate$HeartRate , main = "Normal Q-Q Plot of HeartRate",cex=0.8)
qqline(heartrate$HeartRate,col="blue",lty=2,lwd=1)
shapiro.test(heartrate$HeartRate) # Shapiro wilk test: normality rejected
lillie.test(heartrate$HeartRate) # Lilliefors test: normality rejected

# Check normality of age
qqnorm(heartrate$age, main = "Normal Q-Q Plot of age",cex=0.8)
qqline(heartrate$age,col="blue",lty=2,lwd=1)
shapiro.test(heartrate$age) # Shapiro wilk test: normality not rejected

# Spearman correlation
cor(heartrate$age,heartrate$HeartRate,method = "spearman")
# Non-parametric correlation test
cor.test(heartrate$age, heartrate$HeartRate, method = "spearman")  # Spearman correlation test: no significant monotonic correlation


# HeartRate ~ typediabetes

# Boxplot of HeartRate by type of diabetes
bxpl("HeartRate","typediabetes")

# ANOVA table
anova_result <- aov(HeartRate ~ typediabetes, data = heartrate)
summary(anova_result)

# Assumptions check
leveneTest(HeartRate ~ typediabetes, data = heartrate)   # Levene's test: homogeneity of variances not rejected
shapiro.test(anova_result$residuals)   # Shapiro wilk test: normality rejected
lillie.test(anova_result$residuals)   #
by(heartrate$HeartRate,heartrate$typediabetes,length)  # Sample size < 50

# QQ-Plot of residuals
qqnorm(anova_result$residuals, ,main="Normal Q-Q Plot",cex=0.8)
qqline(anova_result$residuals,col="blue",lty=2,lwd=1)

# The residuals are not normally distributed and sample sizes are small
# Non parametric test
kruskal.test(HeartRate ~ typediabetes, data=heartrate)



# c

# Weight ~ smoker

# Boxplot of Weight by Smoking Status
bxpl("weight","smoker")

# Normality tests for each smoking group
by(heartrate$weight,heartrate$smoker,shapiro.test)   # Shapiro wilk test: normality rejected 
by(heartrate$weight,heartrate$smoker,lillie.test)   # Lilliefors test: normality rejected
by(heartrate$weight,heartrate$smoker,length)   # Sample size per group: both n > 50

# Histogram of Weight by Smoking Status
df <- data.frame(
  weight = heartrate$weight,
  smoker = factor(heartrate$smoker, labels = c("Non-Smokers", "Smokers"))
)

ggplot(df, aes(x = weight, fill = smoker)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, color = "black") +
  facet_wrap(~smoker, ncol = 1) +
  labs(title = "Histogram of Weight by Smoking Status",
       x = "Weight",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# QQ-Plot of weight for Non-Smokers
qqnorm(heartrate$weight[heartrate$smoker == 0],main="Normal Q-Q Plot of weight for Non-Smokers",cex=0.8)
qqline(heartrate$weight[heartrate$smoker == 0],,col="blue",lty=2,lwd=1)

# QQ-Plot of weight for Non-Smokers
qqnorm(heartrate$weight[heartrate$smoker == 1],main="Normal Q-Q Plot of weight for Smokers",cex=0.8)
qqline(heartrate$weight[heartrate$smoker == 1],col="blue",lty=2,lwd=1)

# Right skewed distribution in both groups
# The mean is not an appropriate measure of central tendency
# Non parametric test
wilcox.test(heartrate$weight~heartrate$smoker)



#d

# Estimate linear model for HeartRate

# Create quadratic terms for age and weight
heartrate$age2 <- heartrate$age^2
heartrate$weight2 <- heartrate$weight^2

# Fit linear model including all variables
model <- lm(HeartRate ~ .,data = heartrate)
summary(model)

round(vif(model), 1)   # Check multicollinearity

# Center variables weight and age to reduce multicollinearity
heartrate$weight_c <- heartrate$weight - mean(heartrate$weight)
heartrate$weight_c2 <- heartrate$weight_c^2

heartrate$age_c <- heartrate$age - mean(heartrate$age)
heartrate$age_c2 <- heartrate$age_c^2

# Fit model with centered variables
model <- lm(HeartRate ~ Gender + mi + smoker + asthma + oralhypoglcemic + vitMulti + height 
            + white + typediabetes + famhyper + weight_c + weight_c2 + age_c + age_c2, data = heartrate)

round(vif(model), 1)   # Check multicollinearity

# Stepwise model selection using AIC
model2 <- step(model,direction = "both")
summary(model2)


# Model diagnostics

# Check heteroscedasticity and outliers

plot(model2$fitted.values,rstandard(model2),ylim = c(-4,4),cex=0.8,ylab = "rstandard",xlab="fitted values",main="Fitted Values vs Standardized Residuals")
abline(h=c(-qnorm(0.975),qnorm(0.975)),col="red",lty=2)

plot(model2$fitted.values,rstandard(model2)^2,ylim = c(-0.5,8),cex=0.8,,ylab = "rstandard^2",xlab="fitted values",main="Fitted Values vs Quadratic Standardized Residuals")
abline(h=qnorm(0.975)^2,,col="red",lty=2)

# Check indipendence
dwt(model2)
acf(model2$residuals, main="ACF plot of residuals")   # no significant autocorrelation

# Residuals vs observation order
plot(residuals(model2),
     type = "l",
     xlab = "Observation Order",
     ylab = "Residuals",
     main = "Residuals vs Observation Order")

abline(h = 0, col = "red",lty=2, lwd = 2)

# Levene test for homogeneity of variance
x <- cut(model2$fitted.values, breaks = quantile(model2$fitted.values),include.lowest = T)
table(x)
leveneTest(rstandard(model2),x)    # Levene's test: homogeneity of variances not rejected

# Normality of residuals
qqnorm(model2$residuals,cex=0.8)
qqline(model2$residuals,col="blue",lty=2,lwd=1)

shapiro.test(model2$residuals)   # Shapiro wilk test: normality rejected
lillie.test(model2$residuals)   # Lilliefors test: normality rejected

#Cook's distance
plot(model2,which=4)
abline(h=(4/(length(model2$residuals)-7-1)),lty=2,col=2)
#Rstandard vs leverages
plot(model2,which=5)




################################
################################

# Other Relationships

# Gender ~ MI
tab <- table(heartrate$Gender,heartrate$mi)
names(dimnames(tab)) <- c("Gender","mi")
tab
brpl_2(prop.table(tab))

chisq <- chisq.test(tab,correct = T)
chisq$expected
chisq


# MI ~ famhyper
tab <- table(heartrate$mi,heartrate$famhyper)
names(dimnames(tab)) <- c("mi","famhyper")
tab
brpl_2(prop.table(tab))

chisq <- chisq.test(tab,correct = F)
chisq$expected
chisq


# Gender ~ typediabetes
tab <- table(heartrate$Gender,heartrate$typediabetes)
names(dimnames(tab)) <- c("Gender","typediabetes")
tab
brpl_2(prop.table(tab))

fisher.test(tab)

# oralhypoglcemic ~ typediabetes
tab <- table(heartrate$oralhypoglcemic,heartrate$typediabetes)
names(dimnames(tab)) <- c("oralhypoglcemic","typediabetes")
tab
brpl_2(prop.table(tab))

fisher.test(tab)


# MI ~ Smoker
tab <- table(heartrate$mi,heartrate$smoker)
names(dimnames(tab)) <- c("mi","smoker")
tab
brpl_2(prop.table(tab))

chisq <- chisq.test(tab,correct = F)
chisq$expected
chisq


# Asthma ~ Smoker
tab <- table(heartrate$asthma,heartrate$smoker)
names(dimnames(tab)) <- c("asthma","smoker")
tab
brpl_2(prop.table(tab))

fisher.test(tab)


# oralhypoglcemic ~ famhyper
tab <- table(heartrate$oralhypoglcemic,heartrate$famhyper)
names(dimnames(tab)) <- c("oralhypoglcemic","famhyper")
tab
brpl_2(prop.table(tab))

chisq <- chisq.test(tab,correct = F)
chisq$expected
chisq

# typediabetes ~ famhyper
tab <- table(heartrate$famhyper,heartrate$typediabetes)
names(dimnames(tab)) <- c("famhyper","typediabetes")
tab
brpl_2(prop.table(tab))

chisq <- chisq.test(tab,correct = F)
chisq$expected
chisq


# weight ~ typediabetes
# Boxplot of weight by type of diabetes
bxpl("weight","typediabetes")

# ANOVA table
anova_result <- aov(weight ~ typediabetes, data = heartrate)
summary(anova_result)

# Assumptions check
leveneTest(weight ~ typediabetes, data = heartrate)   # Levene's test: homogeneity of variances not rejected
shapiro.test(anova_result$residuals)   # Shapiro wilk test: normality rejected
by(heartrate$weight,heartrate$typediabetes,length)  # Sample size < 50

# QQ-Plot of residuals
qqnorm(anova_result$residuals, ,main="Normal Q-Q Plot",cex=0.8)
qqline(anova_result$residuals,col="blue",lty=2,lwd=1)

# Non parametric test
kruskal.test(weight ~ typediabetes, data=heartrate)


# HeartRate ~ smoker
# Boxplot of HeartRate by Smoking Status
bxpl("HeartRate","smoker")

# Normality tests for each smoking group
by(heartrate$HeartRate,heartrate$smoker,shapiro.test)   # Shapiro wilk test: normality rejected 
by(heartrate$HeartRate,heartrate$smoker,lillie.test)   # Lilliefors test: normality not rejected
by(heartrate$HeartRate,heartrate$smoker,length)   # Sample size per group: both n > 50

# Histogram of HeartRate by Smoking Status
df <- data.frame(
  HeartRate = heartrate$HeartRate,
  smoker = factor(heartrate$smoker, labels = c("Non-Smokers", "Smokers"))
)

ggplot(df, aes(x = HeartRate, fill = smoker)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, color = "black") +
  facet_wrap(~smoker, ncol = 1) +
  labs(title = "Histogram of HeartRate by Smoking Status",
       x = "HeartRate",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# QQ-Plot of HeartRate for Non-Smokers
qqnorm(heartrate$HeartRate[heartrate$smoker == 0],main="Normal Q-Q Plot of HeartRate for Non-Smokers",cex=0.8)
qqline(heartrate$HeartRate[heartrate$smoker == 0],,col="blue",lty=2,lwd=1)

# QQ-Plot of weight for Non-Smokers
qqnorm(heartrate$HeartRate[heartrate$smoker == 1],main="Normal Q-Q Plot of HeartRate for Smokers",cex=0.8)
qqline(heartrate$HeartRate[heartrate$smoker == 1],col="blue",lty=2,lwd=1)

# F-test for equality of variances 
var.test(HeartRate~smoker, data = heartrate)    # The assumption of equal variances is not rejected
# Independent samples t-test
t.test(HeartRate~smoker, data = heartrate,var.equal = T)

# error bar
plotmeans(HeartRate ~ smoker, data = heartrate, 
          xlab = "smoker", ylab = "HeartRate",ylim=c(62,74), 
          main = "HeartRate ~ smoker",connect = F)


# HeartRate ~ famhyper
# Boxplot of HeartRate by famhyper
bxpl("HeartRate","famhyper")

# Normality tests for each famhyper group
by(heartrate$HeartRate,heartrate$famhyper,shapiro.test)   # Shapiro wilk test: normality rejected 
by(heartrate$HeartRate,heartrate$famhyper,lillie.test)   # Lilliefors test: normality not rejected
by(heartrate$HeartRate,heartrate$famhyper,length)   # Sample size per group: both n > 50

# Histogram of HeartRate by famhyper
df <- data.frame(
  HeartRate = heartrate$HeartRate,
  famhyper = factor(heartrate$famhyper, labels = c("0", "1"))
)

ggplot(df, aes(x = HeartRate, fill = famhyper)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, color = "black") +
  facet_wrap(~famhyper, ncol = 1) +
  labs(title = "Histogram of HeartRate by famhyper",
       x = "HeartRate",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# QQ-Plot of HeartRate for famhyper=0
qqnorm(heartrate$HeartRate[heartrate$famhyper == 0],main="Normal Q-Q Plot of HeartRate for famhyper=0",cex=0.8)
qqline(heartrate$HeartRate[heartrate$famhyper == 0],,col="blue",lty=2,lwd=1)

# QQ-Plot of HeartRate for famhyper=1
qqnorm(heartrate$HeartRate[heartrate$famhyper == 1],main="Normal Q-Q Plot of HeartRate for famhyper=1",cex=0.8)
qqline(heartrate$HeartRate[heartrate$famhyper == 1],col="blue",lty=2,lwd=1)

# F-test for equality of variances 
var.test(heartrate$HeartRate~heartrate$famhyper)   # The assumption of equal variances is not rejected
# Independent samples t-test
t.test(HeartRate ~ famhyper,data = heartrate,var.equal=T)

# Mean of HeartRate by group of famhyper
tapply(heartrate$HeartRate, heartrate$famhyper, mean)

# error bar
plotmeans(HeartRate ~ famhyper, data = heartrate, 
          xlab = "famhyper", ylab = "HeartRate",ylim=c(58,74), 
          main = "HeartRate ~ famhyper",connect = F)


# weight ~ oralhypoglcemic
bxpl("weight","oralhypoglcemic")

# Normality tests for each oralhypoglcemic group
by(heartrate$weight,heartrate$oralhypoglcemic,shapiro.test)   # Shapiro wilk test: normality rejected 
by(heartrate$weight,heartrate$oralhypoglcemic,lillie.test)   # Lilliefors test: normality rejected
by(heartrate$weight,heartrate$oralhypoglcemic,length)   # Sample size of group oralhypoglcemic=1:  < 50

# Histogram of weight by oralhypoglcemic 
df <- data.frame(
  weight = heartrate$weight,
  oralhypoglcemic = factor(heartrate$oralhypoglcemic, labels = c("oralhypoglcemic=0", "oralhypoglcemic=1"))
)

ggplot(df, aes(x = weight, fill = oralhypoglcemic)) +
  geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, color = "black") +
  facet_wrap(~oralhypoglcemic, ncol = 1) +
  labs(title = "Histogram of weight by oralhypoglcemic",
       x = "weight",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# QQ-Plot of weight for oralhypoglcemic=0
qqnorm(heartrate$weight[heartrate$oralhypoglcemic == 0],main="Normal Q-Q Plot of weight for oralhypoglcemic=0",cex=0.8)
qqline(heartrate$weight[heartrate$oralhypoglcemic == 0],,col="blue",lty=2,lwd=1)

# QQ-Plot of weight for oralhypoglcemic=1
qqnorm(heartrate$weight[heartrate$oralhypoglcemic == 1],main="Normal Q-Q Plot of weight for oralhypoglcemic=1",cex=0.8)
qqline(heartrate$weight[heartrate$oralhypoglcemic == 1],col="blue",lty=2,lwd=1)

# Non parametric test
wilcox.test(weight~oralhypoglcemic,data = heartrate)

# Median of weight by group of oralhypoglcemic
tapply(heartrate$weight, heartrate$oralhypoglcemic, median)


# age ~ typediabetes
# Boxplot of age by type of diabetes
bxpl("age","typediabetes")

# ANOVA table
anova_result <- aov(age ~ typediabetes, data = heartrate)
summary(anova_result)

# Assumptions check
leveneTest(age ~ typediabetes, data = heartrate)   # Levene's test: homogeneity of variances not rejected
shapiro.test(anova_result$residuals)   # Shapiro wilk test: normality not rejected
lillie.test(anova_result$residuals)   # Lilliefors test: normality not rejected
by(heartrate$age,heartrate$typediabetes,length)  # Sample size < 50

# QQ-Plot of residuals
qqnorm(anova_result$residuals, ,main="Normal Q-Q Plot",cex=0.8)
qqline(anova_result$residuals,col="blue",lty=2,lwd=1)

# Since the assumptions are satisfied, the ANOVA results are valid
summary(anova_result)

# Error bar of age by type of diabetes
plotmeans(age ~ smoker, data = heartrate, 
          xlab = "smoker", ylab = "age",ylim=c(60,68), 
          main = "age ~ smoker",connect = F)



# age ~ MI
# Boxplot of age by mi
bxpl("age","mi")

# Normality tests for each mi group
by(heartrate$age,heartrate$mi,shapiro.test)   # Shapiro wilk test: normality not rejected 
by(heartrate$age,heartrate$mi,lillie.test)   # Lilliefors test: normality not rejected

# QQ-Plot of age for mi=0
qqnorm(heartrate$age[heartrate$mi == 0],main="Normal Q-Q Plot of age for mi=0",cex=0.8)
qqline(heartrate$age[heartrate$mi == 0],,col="blue",lty=2,lwd=1)

# QQ-Plot of age for mi=1
qqnorm(heartrate$age[heartrate$mi == 1],main="Normal Q-Q Plot of age for mi=1",cex=0.8)
qqline(heartrate$age[heartrate$mi == 1],col="blue",lty=2,lwd=1)

# F-test for equality of variances 
var.test(heartrate$age~heartrate$mi)   # The assumption of equal variances is not rejected
# Independent samples t-test
t.test(age ~ mi,data = heartrate,var.equal=T)

# error bar
plotmeans(age ~ mi, data = heartrate, 
          xlab = "mi", ylab = "age",ylim=c(59,68), 
          main = "age ~ mi",connect = F)
