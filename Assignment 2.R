#Load Libraries
library(ggplot2)
#View Dataset
print(CholestrolLevel, n=155)
#Histogram showing Normal Curve
histNC <- ggplot(CholestrolLevel, aes(x=After)) +
  labs(title="Distribution of Patient's Cholesterol Level after applying 
       6 months of a certain diet")+ 
  theme(plot.title = element_text(hjust=0.5)) + xlab('Cholesterol Level')
histNC <- histNC + geom_histogram(binwidth=0.25, colour="black",
                                  aes(y=after_stat(density), fill=..count..))
histNC <- histNC + scale_fill_gradient("Count", low="yellow", high="green")
histNC <- histNC + stat_function(fun=dnorm, color="red",
                                 args=list(mean=mean(CholestrolLevel$After),
                                           sd=sd(CholestrolLevel$After)))
histNC

# Conducting Normality Test - Shapiro-Wilk
shapiro.test(CholestrolLevel$After)

# Step 1:
# State the Null and alternative hypotheses
H0 <- mu0 <- 5.95
HA <- mu0 not <- 5.95

# Step 2:
#Define your significance level
Alpha <- the significance level
[1] 0.05
# Step 3:
# Conduct test statistics
# conduct two-tail test
# Define parameters need to solve for z-score
mu0 <- population mean
[1] 5.95
# Sample mean
mu <- mean(CholestrolLevel$After)
[1] 5.148
sigma <- population standard deviation
[1] 0.897
# sample size
n <- nrow(CholestrolLevel)
[1] 155
s.e = (sigma/sqrt(n)) # sample standard error
[1] 0.07204877
# Solve for z
z = (mu-mu0)/s.e
[1] -11.1313

# Step 4:
# Arrive at a conclusion and make a decision
#Solve for p-value
p-value = 2*pnorm(abs(z), lower.tail=FALSE)#multiply by 2 as we are conducting two tail test.
[1] 8.828976e-29
Given that the p-value is 8.828976e-29, which is less than the 0.05 significance level, the null hypothesis that mu <- 5.95 is rejected.

#Step 5:
# With a 95% confidence level, we reject the null hypothesis that, following a certain diet program correctly followed for six months on a group of people, the population cholesterol level mean is equal to 5.95.
