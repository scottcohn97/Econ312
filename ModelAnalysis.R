# Scott Cohn and Samuel Hostetter
# ResEc 312
# 2018

# Libraries
library(rlang)
library(readxl)
library(dplyr)
library(randomForest)
library(car)
library(ggplot2)
library(ggthemes)
library(sjPlot)
library(forecast)
library(xtable)
library(broom)
library(knitr)
library(lmtest)
library(het.test)
library(plotly)
library(ggfortify)
library(party)
library(dynlm)


# Import

ModelData <- read_excel("data.xlsx")
D_Outcome <- read_excel("data.xlsx", sheet = "Actual Value")

# Trim and Clean
WorkSet <- ModelData[11:35, 1:10]
WorkSet <- WorkSet[, -WorkSet$rfpred]
names(WorkSet)[names(WorkSet) == 't'] <- 'Year'


dplyr::inner_join(WorkSet, D_Outcome, by = "Year")

WorkSet <-
  data.frame(lapply(WorkSet, function(x)
    as.numeric(as.character(x))))

# Convert VP and actualVP to percentages
WorkSet$VP <- (WorkSet$VP / 100)
WorkSet$actualVP <- (WorkSet$actualVP / 100)


# Models
# Generalized Model --- Weighted Least Sqs Model --- R^2 is meaningless --- Error should be 1 --- used for groupwise heterosk
model_glm <-
  glm(actualVP ~ G:I + P:I + Z:I + DPER + DUR + I + WAR,
      data = explore_data,
      family=binomial(link="logit")
      )
summary(model_glm)

# Feasible Gen Model


Pres.ts <- ts(ModelData)
Pres.ar2 <- dynlm(actualVP~L(actualVP)+L(actualVP,2), data=Pres.ts)
knitr::kable(broom::tidy(Pres.ar2), digits=4,
      caption="Autoregressive model of order 2 using the dataset $pres$")

res.ar2 <- resid(Pres.ar2)
acf(res.ar2) # New: Acf() from package forecast
#The results of the (non-) autocorrelation test are usually summarized in a correlogram, a bar diagram that visualizes the values of the test statistic  
#T‾‾√rk for several lags as well as the  95%
#confidence interval. A bar ( T‾‾√rk)
# that exceedes (upward or downward) the limits of the confidence interval indicates autocorrelation for the corresponding lag.

# Linear Model
model_lm <-
  lm(VP ~ G:I + P:I + Z:I + DPER + DUR + I + WAR, data = WorkSet)
summary(model_lm)
# Can use model to predict probabilities

# Define a test_dataset
test_data <- WorkSet

WorkSet$glm_pred <- predict(model_glm, WorkSet,
                            type = "response")
WorkSet$lm_pred <- predict(model_lm, WorkSet,
                           type = "response")
summary(model_glm)

summary(model_lm)


car::vif(model_lm) # These are high. I is 38. ??

model_no_I <- lm(VP ~ G + P + Z + DPER + DUR + WAR, data = WorkSet)
anova(model_no_I, model_lm)
summary(model_no_I)

model_I <- lm(VP ~ I + P + DPER + DUR, data = WorkSet)
summary(model_I)



# Sampling training set from data

nrow(WorkSet) * 0.50
sample_set <- sample_n(WorkSet, 10)


# building a simple random forest

m <-
  randomForest(
    VP ~ G * I + P * I + Z * I + DPER + DUR + I + WAR,
    data = WorkSet,
    # number of trees in the forest
    ntree = 500,
    # number of predictors (p) per tree
    mtry = sqrt(7)
  ) 

# making predictions from a random forest
sample_set$randForestPred <- predict(m, sample_set)
WorkSet$randForestPred <- predict(m, WorkSet)

summary(WorkSet$rfpred)
summary(WorkSet$VP)
summary(WorkSet$actualVP)
summary(WorkSet$randForestPred)

# Large gap in 1992 --- Is Perot Significant? Added Dummy for Perot var

WorkSet$Perot <- ifelse(WorkSet$Year == 1992, 1, 0)
perot_model <-
  lm(VP ~ G * I + P * I + Z * I + DPER + DUR + I + WAR + Perot, data = WorkSet)
WorkSet$perot_pred <- predict(perot_model, WorkSet)

summary(perot_model)
anova(perot_model, model_lm)

# *** dummy var for inertia for percentage of voters of voted the same (use previous var values) ---> potentially leads to multicollinearity
# "lag"
WorkSet$lag <- lag(WorkSet$actualVP, 1)
WorkSet$lag <- WorkSet$actualVP + WorkSet$lag

# Regress on lag --- taking lag into account, we lose an observation (no bueno given 19 var)
lag_model <-
  lm(VP ~ G * I + P * I + Z * I + DPER + DUR + I + WAR + lag, data = WorkSet)
WorkSet$lag_pred <- predict(lag_model, WorkSet)
summary(lag_model)
anova(lag_model, model_lm) # To run ANOVA, need to drop the 1940 obs given loss of var from lag

# Plot Prediction Models
# Make geom_line or geom_smooth
Pred_gg <- ggplot(WorkSet, aes(Year)) +
  ggtitle("Prediction Models: 1940 - 2012") +
  ylab("Democratic Share of Presidential Vote (Percentage)") +
  geom_line(aes(y = randForestPred, color = "Random Forest Model")) +
  geom_line(aes(y = actualVP, color = "True Vote Share")) +
  geom_line(aes(y = VP, color = "Fair Model")) +
  geom_line(aes(y = glm_pred, color = "Generalized Linear Model")) +
  geom_line(aes(y = lm_pred, color = "Linear Model")) +
  geom_line(aes(y = perot_pred, color = "Perot Model")) +
Pred_gg + theme_tufte() 
Pred_gg + theme_fivethirtyeight()




# Pred_gg + theme_economist() <--- This could just be funny

# Observe as time series object
timeser <-
  ts(
    data = WorkSet[,1:10],
    start = 1940,
    end = 2012,
    frequency = 1
  )

# Correlation Matrix
WorkSet[, 4:10] %>% as.data.frame() %>% GGally::ggpairs()



# Forecasting

predpoint <-
  data.frame(
    I = -1,
    DPER = -1,
    DUR = 0,
    WAR = 0,
    G = 1.62,
    P = 1.36,
    Z = 5
  )
kable(tidy(predict(
  model_lm, newdata = predpoint,
  interval = "prediction"
)),
caption = "Forecasting in the quadratic linear model")

# Heteroskedasticity --- Figure out how these are calculated

dwtest(model_lm)         # Durbin Watson Test for AutoCorrelation
bptest(model_lm)         # White's Test
het.test::show(model_lm) # Breusch-Pagan Test

# ON THE RESIDUAL-FITTED PLOT and SCALE-LOCATION
# We should see a random distribution on the x-axis and straight red line if we dont have heterosk. We have curvature. Is this because of Heterosk, or 
# or is this due to a small sample size. Our graph does suggest heterosk. 


library(summarytools)
summarytools::descr(test_data)

# autoplot(uschange[, 1]) + ylab("% change in US consumption") +
#   autolayer(fcast.up, PI = TRUE, series = "increase") +
#   autolayer(fcast.down, PI = TRUE, series = "decrease") +
#   guides(color = guide_legend(title = "Scenario"))

# Test Model Table
# sjPlot is good for HTML
# sjt.lm(model_lm, perot_model, lag_model)

# Reg Plots
plot(model_lm)
autoplot(model_lm) + theme_fivethirtyeight()

plot(ctree(actualVP ~ ., data = test_data))

# modify WAR var?? To suit closest years ---> including non-war events

# endogeneity --- > Instrumental var ?? ---> dep var affect independent ---> cyclicality (may not tackle bc small data)
# can discuss, maybe not regress


# GDP Predictions -- from gov't agency (?) --> can use to estimate future vote

# Stress test --- How? More regressions w/ change in var 

# Talk about why we cut to 1940 and onward
# Lots of war, tech advances, party shifts??

# Watch the signs --- look for changes

# Power of test ... few data points 
# There are many things we want to test for. We are unable to do this given the few data points we have available. The Power of
# our test is inhibited (increasing liklihood for Type 2 Error) given our small n

# Should WAR be something else --- parallel var?
# Capture something like terror? ---> at what point is it too broad? Can we even test this?
# Maybe not regress on it, but we can def discuss

# present different models in parallel in tables (similar to how Fair has done for VP, CC, House)

# Look at graphs for weird stuff

# Trees
treeset <- explore_data
library(tree)
tree.model <- tree(actualVP ~ ., data = treeset)
plot(tree.model)
text(tree.model)
summary(tree.model)

partition.tree(tree.model, label = "VP", add = TRUE)

library(party)
class_tree <- ctree(tree.model, data = treeset)
summary(class_tree)

library(rpart)
rpart.tree <- rpart(actualVP ~., data = treeset)
plot(rpart.tree)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Classification Tree")



# Recode I to 0 and 1 for repub

I_Recode <- explore_data
I_Recode$DemI <- NA
I_Recode$RepI <- NA
I_Recode$DemI[I_Recode$I == "1"] <- 1
I_Recode$DemI[I_Recode$I != "1"] <- 0
I_Recode$RepI[I_Recode$I == "-1"] <- 1
I_Recode$RepI[I_Recode$I != "-1"] <- 0


model_DemI <- lm(actualVP ~ DemI + DPER + DUR + WAR + G*DemI + P*DemI + Z*DemI, data = I_Recode)
fair_model <- lm(actualVP ~ I + DPER + DUR + WAR + G:I + P:I + Z:I, data= explore_data)
