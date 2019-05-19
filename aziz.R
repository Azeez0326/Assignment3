
# reading diabetes related data 
df <- read.csv("health.csv", na.strings = c("","na"))
str(df)
head(df)
summary(df)

# reaserch question : difference in percent of male and female diabetic patience 

# variables considered for hypothesis are male and female data.

health_data <- subset (df, select=c(Sex, Year, Number.of.adults.with.diabetes ))

health_data

str(health_data)
summary(health_data)

# studu=ying the data set 
  
library("lattice")
# The histogram uses a 1 sided formula, so we
# dont specify anything on left side of ~
# and on right side we specify which variable is in the histogram
# ie temp.
# After the vertical line we show the factor by which the data
# is split ie "activ"
histogram(~Number.of.adults.with.diabetes | Sex, data = health_data)

# Quantile-quantile plot allows us to 
# data is distributed normally
# Compare the quantiles of both samples 
# We use square brackets to select the cases we want

with(health_data,
     qqplot(Number.of.adults.with.diabetes[Sex == "Men"],
            Number.of.adults.with.diabetes[Sex == "Women"], 
            main = "Comparing male and femlae dibetic patience", 
            xlab = "Male",
            ylab =  "Female"))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution
with(health_data, {
  qqnorm(Number.of.adults.with.diabetes[Sex == "Men"], 
         main = "Diabetes in male")
})

# We can add normailty line 
# to the plot to evaluate normality
# for active period = no
with(health_data, {
  qqnorm(Number.of.adults.with.diabetes[Sex == "Men"], 
         main = "Diabetes in male")
  qqline(Number.of.adults.with.diabetes[Sex == "Men"])
})


# We can add normailty line 
# to the plot to evaluate normality
# for active period = no
with(health_data, {
  qqnorm(Number.of.adults.with.diabetes[Sex == "Women"], 
         main = "Diabetes in Female")
  qqline(Number.of.adults.with.diabetes[Sex == "Women"])
})

# test

# normality test to determing if both the groups have normally distributed data

normality_test <- shapiro.test(health_data$Number.of.adults.with.diabetes)
normality_test$p.value

# From the above test we have determined the p value is less than 0.05 hence the data is normalized 

# We can use the Wilcox.text() function 
# for data that deviates from normality
# In this test we get the test statistic (W)
# as well as the p value


# H0= there is no difference between the number of male and female patience rate during the year 1980 to 2014
# H1= there is a difference between the number of male and female patience rate during the year 1980 to 2014

# power testing to find the power of coeefecient 
install.packages("pwr")
library(pwr)
power_changes <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                            sig.level = 0.05,
                            power = 0.80)
                       
plot(power_changes)

#  2-sample proportion test - pwr.2p.tes to prove the null hypothesis

effect_size <- pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.05), sig.level = 0.05,
            power = .80)
plot(effect_size)
nrow(health_data)
# n = 20
 # for  80% power and a significance level of 0.05



# We can use the Wilcox.text() function 
# for data that deviates from normality
# In this test we get the test statistic (W)
# as well as the p value
# This test examines whether the centre of the data
# differs for both samples
wilcox.test(health_data$Number.of.adults.with.diabetes, data = df1)

# data:  health_data$Number.of.adults.with.diabetes
# V = 2485, p-value = 3.637e-13
# alternative hypothesis: true location is not equal to 0


# the value of p is less than 0.05 hence we can conclude that alternate hypothesis is correct and reject the null hypotheis 


  