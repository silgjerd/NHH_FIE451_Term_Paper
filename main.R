library(tidyverse)
library(zoo)
library(lubridate)
library(vroom)

library(TTR)
library(PerformanceAnalytics)

library(xgboost)
library(ranger)
library(caret)
rm(list=ls())
options(scipen=999,stringsAsFactors=F)

mom <- vroom("mom.CSV", col_types = cols(date = col_date(format = "%Y%m%d")))
ff  <- vroom("ff3f.CSV", col_types = cols(date = col_date(format = "%Y%m%d")))

# Merge
df <- left_join(mom, ff, by = "date")


# Add yearmon
df <- mutate(df, yearmon = paste(year(date), ifelse(nchar(month(date))==1, paste("0", month(date), sep = ""), month(date)), sep = ""))



ndf <- df %>%
  mutate( #price
    pmom = tail(cumprod(c(1, (1 + (mom/100)))),(nrow(df))),
    pmkt = tail(cumprod(c(1, (1 + (`Mkt-RF`/100)))),(nrow(df))),
    psmb = tail(cumprod(c(1, (1 + (SMB/100)))),(nrow(df))),
    phml = tail(cumprod(c(1, (1 + (HML/100)))),(nrow(df))),
    
    momlogret = log((1 + (mom/100)))
  ) %>%
  mutate( #sma
    sma10mom = EMA(pmom, 5),
    sma30mom = EMA(pmom, 40)
  ) %>%
  mutate( #diff sma
    dmom1030  = log(sma10mom) - log(sma30mom)
  ) %>%
  group_by(yearmon) %>%
  summarise(m1030  = mean(dmom1030),
            monret = exp(sum(momlogret))-1) %>%
  ungroup %>% na.omit


# Realized variances
rv <- df %>%
  group_by(yearmon) %>%
  summarise(rvmom = sd(mom),
            rvmkt = sd(`Mkt-RF`),
            rvsmb = sd(SMB),
            rvhml = sd(HML))

rv <- left_join(rv, ndf, by = "yearmon")

origdata <- rv %>%
  select(-yearmon) %>%
  mutate(y = lead(rvmom),
         lag1rvmom = lag(rvmom, 1),
         lag4rvmom = lag(rvmom, 4)
         
         #,log1 = log(rvmom),
         #log2 = log(rvmkt),
         #loglag1 = log(lag1rvmom),
         #loglag2 = log(lag4rvmom)
         ) %>%
  na.omit


data <- origdata %>% select(-rvsmb, -rvhml, -monret)

##########################################################################################

# Train test split
idx <- floor(nrow(data)*.8)
train <- data[1:idx,]
test  <- data[(idx+1):nrow(data),]

# Normalization
ycol <- which(colnames(data) == "y")
normParam <- preProcess(train[,-ycol], method = "range")
train <- bind_cols(predict(normParam, train[,-ycol]), y = train$y)
test  <- bind_cols(predict(normParam, test[,-ycol]), y = test$y)

# LM on train data
fit <- lm(y ~ ., data = train)
summary(fit)



pred <- predict(fit, newdata = test)

# Evaluation
e <- (test$y - pred)

lm(test$y ~ pred) %>% summary

cat("MSE:", round(mean(e^2), 4)*100, "\n",
    "MAE:", round(mean(abs(e)), 4), "\n",
    "Rsq:", round(cor(test[,1], pred)^2, 4), "\n")


comp <- tibble(pred = pred, truth = test$y)
plot(comp)


# CV
# Define training control
#train.control <- trainControl(method = "LOOCV", verboseIter = T)
train.control <- trainControl(method = "timeslice",
                              initialWindow = 240,
                              horizon = 1,
                              fixedWindow = F,
                              verboseIter = T)
# Train the model
model <- caret::train(y ~ ., data = data, method = "lm",
                      trControl = train.control)
# Summarize the results
print(model)

# ======================================================================================

preds <- NULL
start_mon <- 240

for (i in (start_mon+1):nrow(data)) { #expanding window
  
  train <- data[1:(i-1),]
  test <- data[i,]
  
  fit <- lm(y ~ rvmom + lag4rvmom, data = train)
  pred <- predict(fit, test) %>% unname
  
  preds <- c(preds, pred)
  
}

preds <- c(rep(NA, start_mon), preds)

target_vol <- 0.58
weights <- target_vol / preds

res <- tibble(pred = preds,
              weight = weights)

res <- bind_cols(origdata, res) %>% select(monret, y, pred, weight) %>% na.omit

# Scaled returns
res <- mutate(res, scaledmonret = monret * weight)

# Normalizing
normweightparam <- preProcess(as.data.frame(res$weight), method = "range")
normweight <- predict(normweightparam, as.data.frame(res$weight))[,1]
res <- bind_cols(res, normweight = normweight)
res <- mutate(res, scaledmonret = monret * normweight)


msharpe <- function(x) {(mean(x) * 12) / (sd(x) * sqrt(12))}

cat("Regular momentum:", round(msharpe(res$monret), 2))
cat("Scaled momentum:", round(msharpe(res$scaledmonret), 2))




# PLOT
# RET2PRICE Momentum
pricereg <- cumprod(c(1, (1 + (res$monret[500:875]))))
pricescl <- cumprod(c(1, (1 + (res$scaledmonret[500:875]))))
prices <- tibble(pricereg = pricereg, pricescl = pricescl)

prices %>%
  ggplot(aes(x = seq_along(pricereg))) +
  geom_line(aes(y = pricereg)) +
  geom_line(aes(y = pricescl), col = "red") +
  theme_bw()



ggplot(res) +
  geom_line(aes(x = seq_along(y), y = monret)) +
  #geom_line(aes(x = seq_along(y), y = pred), col = "red") +
  geom_line(aes(x = seq_along(y), y = weight), col = "red") +
  theme_bw()





momdf <- tail(momdf, 1115)
weights <- c(rep(NA, 240), weights)
withw <- bind_cols(momdf, volw = weights) %>% na.omit
withw <- mutate(withw, scaled = monret * volw)

prc <- cumprod(c(100, (1 + (withw$scaled/100)) ))
plot(prc, type = "l")


# SHARPE
mean(withw$scaled) / sd(withw$scaled)
sd(withw$scaled)

(mean(withw$scaled)*12) / (sd(withw$scaled)*sqrt(12))

SharpeRatio.annualized(withw$scaled, scale = 12)
SharpeRatio(withw$scaled)




###################################
# XGBOOST #
###################################

ycol <- which(colnames(data) == "y")

xtrain <- xgb.DMatrix(as.matrix(train[,-ycol]), label = as.matrix(train[,ycol]))
xtest  <- xgb.DMatrix(as.matrix(test[,-ycol]),  label = as.matrix(test[,ycol]))


mse <- NULL

for (i in seq_len(1000)){
  
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    
    eta =              sample(c(.001,.01,.05,.1,.2,.3), 1),
    gamma =            sample(c(.05,.1,.3,.5,.7,.9,1), 1),
    max_depth =        sample(c(3:10), 1),
    min_child_weight = sample(c(1:7), 1),
    subsample =        sample(c(.6,.7,.8,.9,1), 1),
    colsample_bytree = sample(c(.6,.7,.8,.9,1), 1),
    lambda =           sample(c(c(1:10*.01,1)), 1),
    alpha =            sample(c(0,.1,.5,1), 1),
    tree_method = "auto"
  )
  
  mxgb <- xgb.train(
    params = params,
    data = xtrain,
    nrounds = 500,
    early_stopping_rounds = 50,
    watchlist = list(val = xtest, train = xtrain),
    eval_metric = "rmse",
    verbose = 1
  )
  
  pred <- predict(mxgb, xtest)
  e <- (test$y - pred)
  mse <- c(mse, mean(e^2))
}

# Evaluation
e <- (test$y - pred)

cat("Rsq:", cor(test[,1], pred)^2)
cat("MSE:", mean(e^2))

comp <- tibble(pred = pred, truth = test$y)
plot(comp)





# Investigating correlations ####################


hmlstrat <- cumprod(c(100, (1 + (df$HML[10000:24582]/100)) ))
plot(hmlstrat, type = "l")

smbstrat <- cumprod(c(100, (1 + (df$SMB[10000:24582]/100)) ))
plot(smbstrat, type = "l")

momstrat <- cumprod(c(100, (1 + (df$mom[1:24582]/100)) ))
plot(momstrat, type = "l")



#################################################

# KERAS

library(keras)
library(tensorflow)

normParam <- preProcess(train[,-ycol], method = "range")
train <- bind_cols(predict(normParam, train[,-ycol]), y = train$y)
test  <- bind_cols(predict(normParam, test[,-ycol]), y = test$y)

xtrain <- train %>% select(-y) %>% as.matrix
ytrain <- train %>% select(y) %>% as.matrix


model <- keras_model_sequential()

model %>%
  layer_dense(units = 5, input_shape = c(5)) %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 1, activation = "relu")


summary(model)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(),
  metrics = c("mse")
)



history <- model %>% fit(
  xtrain, ytrain, 
  epochs = 100, batch_size = 64, 
  validation_split = 0.2
)

xtest <- test %>% select(-y) %>% as.matrix
pred <- predict(model, xtest)
e <- (test$y - pred)
lm(test$y ~ pred) %>% summary
cat("MSE:", round(mean(e^2), 4)*100, "\n",
    "Rsq:", round(cor(test[,1], pred)^2, 4), "\n")
comp <- tibble(pred = pred, truth = test$y)
plot(comp)







