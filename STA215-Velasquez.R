#set working Directory 
setwd("h:/sta215")

# Install "haven" packages 
install.packages("haven")

# load "haven" package 
library ('haven')

# load fifth grader data 
raw_data<- read.csv("raw_data.csv")

################################################################################
########################### Descriptive statistics #############################
################################################################################

mean(raw_data$number_characters)
mean(raw_data$dexter_inner_thoughts)
mean(raw_data$gruesomeness)
mean(raw_data$tension)

sd(raw_data$number_characters)
sd(raw_data$dexter_inner_thoughts)

table(raw_data$gruesomeness)
27/147 
120/147
table(raw_data$tension)
59/147
49/147
39/147

summary(raw_data$number_characters)
summary(raw_data$dexter_inner_thoughts)

#table 1
table(raw_data$gruesomeness,raw_data$tension)

################################################################################
############################## Figure 1: Box Plot ##############################
################################################################################
#boxplot 
anova <- aov(raw_data$number_characters ~ raw_data$gruesomeness)
summary(anova)
boxplot(raw_data$number_characters ~ raw_data$gruesomeness)

################################################################################
############################ Figure 2: Scatter Plot ############################
################################################################################
#scatter plot 
linear_plot <- plot(raw_data$number_characters, raw_data$dexter_inner_thoughts)
print(linear_plot)

# add x line and y line for means
meany <- mean(raw_data$dexter_inner_thoughts)
meanx <- mean(raw_data$number_characters)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(dexter_inner_thoughts ~ number_characters, data = raw_data)
summary(linear_relationship)
abline(linear_relationship)

################################################################################
########################## Figure 3: Residual Plot #############################
################################################################################
#Residual Plot 
plot(raw_data$number_characters, residuals(linear_relationship))
abline(h = 0, col = "black")