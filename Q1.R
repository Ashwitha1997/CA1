#Q1

#Load the Heart Failure dataset
heart_data1 <- read.csv("HeartFailure.csv")
heart_data1

#removes any rows that contains NA 
heart_data <- na.omit(heart_data1)

#drops any duplicate rows
heart_data <- unique(heart_data)

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

#Q1

#showing the categorical data in a plot
attach(heart_data)
plot(diabetes, DEATH_EVENT, pch=19, col= "lightblue")

# labels starts with what is assigned to lower value first
# eg 0 = no, 1 = yes
heart_data$death <- factor(heart_data$DEATH_EVENT, labels = c("no", "yes"))
heart_data

library("lattice")
attach(heart_data)
histogram(~diabetes | death, 
          data = heart_data, 
          main = "Distribution of heart failure data", 
          xlab = "Patients having Diabetes or not", 
          ylab = "Risk of heart failure")
detach(heart_data)

#-----------------------------------------------------------------
#Appropriate testing 
#-----------------------------------------------------------------


opar <- par(no.readonly = TRUE)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(heart_data, {
  qqnorm(diabetes[death == "yes"], 
         main = "Risk of heart failure because of diabetes")
  qqline(diabetes[death == "yes"])
})

with(heart_data, {
  qqnorm(diabetes[death == "no"], 
         main = "Risk of heart failure not due to diabetes")
  qqline(diabetes[death == "no"])
})

par(opar)

# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(heart_data$diabetes)
normality_test$p.value
# p-value tells us the chances that the sample comes 
# from a normal distribution 
# p-value is clearly lower than 0.05
# so not normally distributed

#wilcox test
wilcox.test(diabetes~death)

#Wilcoxon rank sum test with continuity correction



#chi-squared test
attach(heart_data)
table <- table(diabetes, DEATH_EVENT)
result <- chisq.test(table)
result
detach(heart_data)

