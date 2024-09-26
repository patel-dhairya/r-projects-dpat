setwd("C:/Users/dpat5/Desktop/r-projects-dpat/Heart Disease")
library("ggplot2")
library("vioplot")
library("Metrics")
library("MASS")
library("lmtest")
library("faraway")
library(dplyr)

ddata = read.csv("Doctor Data.txt", sep=" ", header=FALSE)
names(ddata) <- c("Observation", "Age", "Deaths", "Person-Years", "Smoker")
ddata$Smoker <- factor(ddata$Smoker)

smoker_data <- ddata[c(2,3,4,5)]
smoker_data$Age2 <- smoker_data$Age ** 2
attach(smoker_data)

# Part-I
# No interaction model

model1.glm <- glm(Deaths ~ 1, family = poisson, data = smoker_data)
model1.step <- stepAIC(model1.glm, Deaths~Age+log(`Person-Years`)+Smoker+Age2,
                       direction=c("both","backward","forward"))

model1.glm <- glm(Deaths ~ Age + Age2 + log(`Person-Years`) , family = poisson)
summary(model1.glm)

# Interaction model

model2.glm <- glm(Deaths~1, family=poisson, data=smoker_data)
model2.step <- stepAIC(model2.glm, Deaths~(Age+Age2+Smoker+
                                             log(`Person-Years`))^2 , direction=c("both"))


model2.glm <- glm(Deaths ~ Smoker + Age + Age2 + Smoker*Age + log(`Person-Years`)
                  , family = poisson)
summary(model2.glm)

# Plots
par(mfrow=c(2,2))
plot(model2.glm)
 
# Part-ii
# Interaction model
# V4 as offset
model3.glm <- glm(Deaths ~ Smoker + Age + Age2 + Smoker*Age, family = poisson, offset=log(`Person-Years`))
summary(model3.glm)

# Part-iii
# No interaction model
# V4 as covariate
model4.glm <- glm.nb(Deaths ~ 1)
model4.step <- stepAIC(model4.glm, Deaths~Age+log(`Person-Years`)+Smoker+Age2, direction=c("forward"))
model4.glm <- glm.nb(Deaths ~ Age + Age2 + log(`Person-Years`), 
                     control=glm.control(maxit=500))
summary(model4.glm)

# No interaction model
# V4 as covariate
model5.glm <- glm.nb(Deaths ~ 1)
model5.step <- stepAIC(model5.glm, Deaths~(Age+Age2+Smoker+log(`Person-Years`))^2
                       ,  direction=c("forward"))

model5.glm <- glm.nb(Deaths ~ Smoker + Age + Age2 + Smoker*Age + log(`Person-Years`),
                     control=glm.control(maxit=500))
summary(model5.glm)

# No interaction term model
# V4 as offset
model6.glm <- glm.nb(Deaths ~ Age + Age2+offset(log(`Person-Years`)), 
                     control = glm.control(maxit=200), data = smoker_data)
summary(model6.glm)

# Interaction term model
# V4 as offset
model7.glm <- glm.nb(Deaths ~ Smoker + Age + Age2 + Smoker*Age + 
                       offset(log(`Person-Years`)), control=glm.control(maxit=500))
summary(model7.glm)
