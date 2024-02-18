#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport

# Load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport

# Convert Ord.Factor variables to Factor variables
data$countries <- factor(data$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE)
data$sanctions <- factor(data$sanctions, levels = c("None", "5%", "15%", "20%"), ordered = FALSE)

# Fit logistic regression model
model <- glm(choice ~ countries + sanctions, data = data, family = binomial)

# Display summary output
summary(model)

stargazer(model)

# Fit null model with only intercept
null_model <- glm(choice ~ 1, data = data, family = binomial)

# Fit full model with predictors
full_model <- glm(choice ~ countries + sanctions, data = data, family = binomial)

# Perform likelihood ratio test using ANOVA
lr_test <- anova(null_model, full_model, test = "Chisq")

# Display the results
print(lr_test)

stargazer(lr_test)

# Predict probability for 80 of 192 countries with no sanctions
prob <- predict(model, newdata = data.frame(countries = "80 of 192", sanctions = "None"), type = "response")

# Display the estimated probability
print(prob)


# Fit the model with interaction term
model_interaction <- glm(choice ~ countries * sanctions, data = data, family = binomial)

# Perform likelihood ratio test
lr_test_2 <- anova(model, model_interaction, test = "Chisq")

# Display the results
print(lr_test_2)

stargazer(lr_test_2)

