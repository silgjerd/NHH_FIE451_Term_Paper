library(tidyverse);library(zoo);library(lubridate);library(vroom)
library(TTR);library(PerformanceAnalytics)
library(xgboost);library(ranger);library(caret)
rm(list=ls());options(scipen=999,stringsAsFactors=F)

annsharpe <- function(x) {(mean(x) * 12) / (sd(x) * sqrt(12))}

crsp <- vroom("crsp.csv", col_types = cols(RET = col_double(), date = col_date(format = "%Y%m%d")))

data <- crsp %>%
  filter(EXCHCD %in% c(1,2,3)) %>%
  mutate(PRC = abs(PRC),
         me = PRC * SHROUT,
         LOGRET = log(1 + RET),
         yearmon = as.numeric(paste(year(date), ifelse(nchar(month(date))==1, paste("0", month(date), sep = ""), month(date)), sep = ""))) %>%
  select(PERMNO, yearmon, PRC, RET, LOGRET, me) %>%
  na.omit


# ====================================================================================

mdata <- data %>%
  group_by(PERMNO) %>%
  filter(me >= 10000) %>%
  filter(n() >= 13) %>%
  
  
  mutate(
    mom1nl = lag(LOGRET, 1),
    mom2nl = lag(c(rep(NA, 2-1), rollsum(LOGRET, 2)), 1),
    mom4nl = lag(c(rep(NA, 4-1), rollsum(LOGRET, 4)), 1),
    
    mom2  = lag(c(rep(NA, 2-1),  rollsum(LOGRET, 2)), 2),
    mom4  = lag(c(rep(NA, 4-1),  rollsum(LOGRET, 4)), 2),
    mom6  = lag(c(rep(NA, 6-1),  rollsum(LOGRET, 6)), 2),
    mom8  = lag(c(rep(NA, 8-1),  rollsum(LOGRET, 8)), 2),
    mom10 = lag(c(rep(NA, 10-1), rollsum(LOGRET, 10)), 2),
    mom12 = lag(c(rep(NA, 12-1), rollsum(LOGRET, 12)), 2)
  ) %>%
  ungroup %>% na.omit

# ====================================================================================


sharpes <- NULL

for (i in 1:20) {
  
  cat(i, "\n")
  
  returns <- NULL
  for (month in sort(unique(mdata$yearmon))) { #for each month
    
    cat(month, "\n")
    
    csubset <- mdata %>% filter(yearmon == month)
    
    
    retd1 <- csubset %>% arrange(mom1nl) %>% select(RET) %>% tail(i) %>% colMeans %>% unname
    #retd2 <- csubset %>% arrange(mom2nl) %>% select(RET) %>% tail(5) %>% colMeans %>% unname
    #retd3 <- csubset %>% arrange(mom4nl) %>% select(RET) %>% tail(5) %>% colMeans %>% unname
    
    retu1 <- csubset %>% arrange(mom1nl) %>% select(RET) %>% head(i) %>% colMeans %>% unname
    #retu2 <- csubset %>% arrange(mom2nl) %>% select(RET) %>% head(5) %>% colMeans %>% unname
    #retu3 <- csubset %>% arrange(mom4nl) %>% select(RET) %>% head(5) %>% colMeans %>% unname
    
    
    creturn <- (0.5*retu1) - (0.5*retd1)
    
    returns <- c(returns, creturn)
    
    
  }
  
  sharpes <- bind_rows(sharpes, bind_cols(i = i,
                                          sharpe = annsharpe(returns)))
  
  
}










# RET2PRICE
prc <- cumprod(c(1, (1 + returns[200:431])))
plot(prc, type = "l")





