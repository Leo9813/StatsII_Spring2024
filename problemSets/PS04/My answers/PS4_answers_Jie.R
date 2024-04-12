# Download the dataset
library('eha')
library(survival)
install.packages(strag)
library(stargazer)
library(ggplot2)
data(child)
print(child)

# Create a object with time-to-live and event state information
child_surv <- with(child, Surv(enter, exit, event))

# The Kaplan-Meier method was used to estimate survival functions and plot survival curves. 
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))


# Fita Cox Proportional Hazard model using mother's age and infant's gender
cox <- coxph(child_surv ~ m.age + sex, data = child)
summary(cox)
stargazer(cox)
