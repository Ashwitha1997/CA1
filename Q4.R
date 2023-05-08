#Q4

#Using pairs() to examine correlations between variables
pairs(heart_data, labels = colnames(heart_data), main = "Heart failure dataset correlation plot")

# We can use libraries to help improve 
# the chart. Also includes correlations between variables
install.packages("psych")
library(psych)

pairs.panels(heart_data,
             smooth = TRUE,      # If TRUE, draws  smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

#showing the categorical data in a plot
attach(heart_data)
plot(platelets, DEATH_EVENT, pch=19, col= "lightblue")

install.packages("ggplot2")
library(ggplot2)
ggplot(heart_data, aes(x= DEATH_EVENT, y= platelets)) + geom_boxplot()

library("lattice")
attach(heart_data)
histogram(platelets ,DEATH_EVENT, 
          data = heart_data, 
          main = "Distribution of heart failure data", 
          xlab = "Platelets count of Patient", 
          ylab = "Risk of heart failure")
detach(heart_data)


#-----------------------------------------------------------------
#Appropriate testing 
#-----------------------------------------------------------------
# Quantile-quantile plot allows us to check if the
# data is distributed normally
attach(heart_data)
qqnorm(platelets)

#line represents normal distribution
qqline(platelets, col = "red")

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$platelets)
normality_test$p.value
# p-value tells us the chances that the sample comes 
# from a normal distribution 
# p-value is clearly lower than 0.05
# so not normally distributed


#wilcox-test 
wilcox.test(platelets~DEATH_EVENT)


#chi-squared test
attach(heart_data)
table_test <- table(platelets, DEATH_EVENT)
result <- chisq.test(table_test)
result
detach(heart_data)

