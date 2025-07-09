#libraries
library(Amelia)
library(corrplot)
library(corrgram)
library(ppcor)
library(psych)
library(car)
library(nFactors)

#setting a working directory:
setwd(dirname(file.choose()))
getwd()

#reading the csv file and just going through all the variables in dataset
mydata = read.csv("u2614311_DS7006_CW2_data_notnew.csv", stringsAsFactors = FALSE)
head(mydata)
str(mydata)

#getting the number of null values for each column and plot of it.
apply(mydata, MARGIN = 2, FUN = function(x) sum(is.na(x)))
missmap(mydata, col = c("black", "grey"), legend = FALSE)

#remove la code and la name as they are not numeric and are unused for further testing and processing
mydata <- subset(mydata, select = -c(Geography_Code, Geography_Name))
str(mydata)

#attaching the table to work in this table only
attach(mydata)

summary(mydata)

#Here deaths per 1000 people is dependent variable and others are independent variable
#Now lets check for all the outliers using the boxplot in dependent variable
boxplot(Deaths.per.1000.people, xlab="Deaths per 1000 people", ylab="Count")

#Now check the boxplot for all our independent variables 
boxplot(No_car, X1_car, X2_or_more_cars,One_person_household, One_person_over_65, One_person_till_65,
        names=c("No_car","X1_car","X2_or_more_cars","One_person_household" , 
                "One_person_over_65", "One_person_till_65"),xlab="independent variables")

boxplot(One_family, One_family_All_over_65, Other_household_types,
        Unemployed_house, Dependent_children_in_house,health_problem_or_disability,
        names=c("One_family", "One_family_All_over_65", "Other_household_types","Unemployed_household", 
                "Dependent_children_in_house","health_problem_or_disability"),xlab="independent variables")

boxplot(not_deprived , deprived_in_1_dimension, deprived_in_2_dimensions,
        deprived_in_3_dimensions, deprived_in_4_dimensions,
        names=c("not_deprived" ,"deprived_in_1_dimensions", "deprived_in_2_dimensions",
        "deprived_in_3_dimensions", "deprived_in_4_dimensions"),xlab="independent variables")

qqnorm(Deaths.per.1000.people, xlab = "deaths" )
qqline(Deaths.per.1000.people, col=2)

# Create histogram to test normality
hist(Deaths.per.1000.people, col = 'lightblue', main = 'Histogram with Normal Distribution Line', xlab = 'Death value per 1000 people', ylab = 'Frequency')

# Overlay normal distribution line on histogram for better understanding
mu <- mean(Deaths.per.1000.people)
sigma <- sd(Deaths.per.1000.people)
x <- seq(min(Deaths.per.1000.people), max(Deaths.per.1000.people), length = 100)
y <- dnorm(x, mean = mu, sd = sigma)
y <- y * diff(hist(Deaths.per.1000.people, plot = FALSE)$breaks[1:2]) * length(Deaths.per.1000.people) #add scaling to get better values
lines(x, y, col = 'red', lwd = 2)
# Add legend to differentiate
legend('topright', legend = c('Histogram', 'Normal Distribution'), fill = c('lightblue', 'red'))

rm(mu,sigma,x,y)

ks.test(Deaths.per.1000.people, "pnorm", mean(Deaths.per.1000.people), sd(Deaths.per.1000.people))


#correlation matrix to lightly get the idea of how every variable is related to each other.
correlation_matrix <- cor(mydata)
correlation_matrix

#now to get the exact idea of correlation with only our dependent variable
correlation_with_deaths <- cor(mydata[, "Deaths.per.1000.people"],mydata)
correlation_with_deaths

#now i will start removing variables
#i can add more info later for this
#lets get p value of all variables and then remove variables
cor.test(Deaths.per.1000.people, X1_car)
cor.test(Deaths.per.1000.people, No_car)
cor.test(Deaths.per.1000.people, X2_or_more_cars)
cor.test(Deaths.per.1000.people, One_person_household)
cor.test(Deaths.per.1000.people, One_person_over_65)
cor.test(Deaths.per.1000.people, One_person_till_65)
cor.test(Deaths.per.1000.people, One_family_All_over_65)
cor.test(Deaths.per.1000.people, deprived_in_1_dimension)


#after getting p values
#removing the variables that dont have any significant correlation
mydata1 <- subset(mydata, select = -c(No_car, One_person_household, One_family_All_over_65, deprived_in_1_dimension))

summary(mydata1)



#next step is to go to independent variable analysis

#for this lets remove dependent variable to get proper analysis
mydata2 <- subset(mydata1, select = -c(Deaths.per.1000.people))
head(mydata2)
independent_correlation <- cor(mydata2)
independent_correlation

corrgram(mydata2, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="All variables")

#here we observe that some variables are very highly correlated with some of the other variables
#hence we will remove those variables that are making lower impact on dependent variable out of those variables
#as we can see first that unemployed_household has 92 percent correlation with long.term_health_problem_or_disability_in_household
#so we check both their partial correlation:
pcor.test(Deaths.per.1000.people, Unemployed_house, health_problem_or_disability)
pcor.test(Deaths.per.1000.people, health_problem_or_disability, Unemployed_house)
#as we see estimate value and p value is higher for both
#we keep both
pcor.test(Deaths.per.1000.people, One_person_till_65, One_family) ##
pcor.test(Deaths.per.1000.people, One_family, One_person_till_65)
pcor.test(Deaths.per.1000.people, X2_or_more_cars, One_family) 
pcor.test(Deaths.per.1000.people, One_family, X2_or_more_cars)
pcor.test(Deaths.per.1000.people, One_family, Other_household_types)
pcor.test(Deaths.per.1000.people, not_deprived, deprived_in_2_dimensions)##
pcor.test(Deaths.per.1000.people, deprived_in_2_dimensions, not_deprived)
pcor.test(Deaths.per.1000.people, X2_or_more_cars, deprived_in_3_dimensions) 
pcor.test(Deaths.per.1000.people, deprived_in_3_dimensions, X2_or_more_cars)
pcor.test(Deaths.per.1000.people, not_deprived, deprived_in_3_dimensions)
pcor.test(Deaths.per.1000.people, deprived_in_3_dimensions, not_deprived)##
pcor.test(Deaths.per.1000.people, X2_or_more_cars, deprived_in_4_dimensions)##
pcor.test(Deaths.per.1000.people, deprived_in_4_dimensions, X2_or_more_cars)

#as we see estimate value and p value is better for deprived in 2 dimensions , in all the cases where it is used
#we remove not deprived and deprived in 3 dimensions

mydata3 <- subset(mydata2, select = -c( not_deprived, deprived_in_3_dimensions, One_person_till_65, X2_or_more_cars))
summary(mydata3)
head(mydata3)
distinct_variable_corr <- cor(mydata3)
distinct_variable_corr

corrgram(mydata3, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="All variables")


KMO(cor(mydata3))
#we will use factor analysis
# get eigenvalues
ev <- eigen(cor(mydata3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col= "blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum <- 0
for(i in 1:length(ev$value)){
  ev.sum <- ev.sum+ev$value[i]
}
ev.list1 <- 1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i] <- ev$value[i]/ev.sum
}
ev.list2 <- 1:length(ev$value)
ev.list2[1] <- ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i] <- ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type = "b", col = "red", xlab = "number of components", ylab = "cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

fit <- principal(mydata3, nfactors = 4, rotate = "varimax")
fit

# weed out further variables after first factor analysis
myvars <- names(mydata3) %in% c("One_person_over_65")
mydata4 <- mydata3[!myvars]
str(mydata4)
rm(myvars)

# get eigenvalues
ev <- eigen(cor(mydata4))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type = "b", col = "blue", xlab = "variables")

fit <- principal(mydata4, nfactors = 4, rotate = "varimax")
fit

# create four variables to represent the rotated components
fit$scores
fit.data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(fit.data, method = "pearson")
cor.df2 <- as.data.frame(cor.matrix2)
round(cor.df2, 2)



#Multiple linear regression model
model1 <- lm(Deaths.per.1000.people ~ X1_car + One_person_over_65 + Other_household_types + Dependent_children_in_house +
               One_family + Unemployed_house + health_problem_or_disability + deprived_in_4_dimensions + deprived_in_2_dimensions)
summary(model1)
# calculate variance inflation factor
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high


model2 <- lm(Deaths.per.1000.people ~ X1_car + One_person_over_65 + Other_household_types + Dependent_children_in_house + One_person_over_65 + 
               One_family + health_problem_or_disability + deprived_in_2_dimensions)

summary(model2)
# calculate variance inflation factor
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high


