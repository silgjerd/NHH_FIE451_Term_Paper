library(tidyverse);library(zoo);library(lubridate);library(vroom)
library(TTR);library(PerformanceAnalytics)
library(xgboost);library(ranger);library(caret)
rm(list=ls());options(scipen=999,stringsAsFactors=F)
source("funcs.R")

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
  filter(yearmon <= 200412) %>%
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

moms <- c("mom1nl", "mom2nl", "mom4nl", "mom2", "mom4", "mom6", "mom8", "mom10", "mom12")
rets <- NULL
for (month in sort(unique(mdata$yearmon))) {
  cat(month, "\n")
  
  # Select current month
  cdat <- mdata %>% filter(yearmon == month)
  
  # DOWN LEG ===
  dnum <- 5
  momd <- tibble("mom1nl" = cdat %>% arrange(-mom1nl) %>% select(RET) %>% head(dnum) %>% pull,
                 "mom2nl" = cdat %>% arrange(mom2nl) %>% select(RET) %>% head(dnum) %>% pull,
                 "mom4nl" = cdat %>% arrange(mom4nl) %>% select(RET) %>% head(dnum) %>% pull,
                 "mom2"   = cdat %>% arrange(mom2)   %>% select(RET) %>% head(dnum) %>% pull,
                 "mom4"   = cdat %>% arrange(mom4)   %>% select(RET) %>% head(dnum) %>% pull,
                 "mom6"   = cdat %>% arrange(mom6)   %>% select(RET) %>% head(dnum) %>% pull,
                 "mom8"   = cdat %>% arrange(mom8)   %>% select(RET) %>% head(dnum) %>% pull,
                 "mom10"  = cdat %>% arrange(mom10)  %>% select(RET) %>% head(dnum) %>% pull,
                 "mom12"  = cdat %>% arrange(mom12)  %>% select(RET) %>% head(dnum) %>% pull)
  
  # Which mom measure has smallest SD
  momsdd    <- sapply(momd, sd)
  mind      <- moms[which(momsdd==min(momsdd))][1]
  momminsdd <- cdat %>% select(mind)
  
  # Return of stocks sorted by min SD mom measure
  retd <- cdat[order(pull(momminsdd)),] %>% select(RET) %>% head(dnum) %>% pull %>% mean
  
  # UP LEG ===
  unum <- 15
  momu <- tibble("mom1nl" = cdat %>% arrange(mom1nl) %>% select(RET) %>% head(unum) %>% pull,
                 "mom2nl" = cdat %>% arrange(-mom2nl) %>% select(RET) %>% head(unum) %>% pull,
                 "mom4nl" = cdat %>% arrange(-mom4nl) %>% select(RET) %>% head(unum) %>% pull,
                 "mom2"   = cdat %>% arrange(-mom2)   %>% select(RET) %>% head(unum) %>% pull,
                 "mom4"   = cdat %>% arrange(-mom4)   %>% select(RET) %>% head(unum) %>% pull,
                 "mom6"   = cdat %>% arrange(-mom6)   %>% select(RET) %>% head(unum) %>% pull,
                 "mom8"   = cdat %>% arrange(-mom8)   %>% select(RET) %>% head(unum) %>% pull,
                 "mom10"  = cdat %>% arrange(-mom10)  %>% select(RET) %>% head(unum) %>% pull,
                 "mom12"  = cdat %>% arrange(-mom12)  %>% select(RET) %>% head(unum) %>% pull)
  
  # Which mom measure has smallest SD
  momsdu    <- sapply(momu, sd)
  minu      <- moms[which(momsdu==min(momsdu))][1]
  momminsdu <- cdat %>% select(minu)
  
  # Return of stocks sorted by min SD mom measure
  retu <- cdat[order(pull(momminsdu)),] %>% select(RET) %>% head(unum) %>% pull %>% mean
  
  # Return of strategy (both legs)
  ret = (0.5*retu) - (0.5*retd)
  
  # Append results
  rets <- bind_rows(rets, bind_cols(yearmon = month,
                                    mind = mind,
                                    minu = minu,
                                    
                                    retd = retd,
                                    retu = retu,
                                    ret = ret))
  
}

# SUMMARY
annsharpe(rets$ret)

rets %>%
  group_by(mind) %>%
  summarise(r = mean(retd),
            n = n())
rets %>%
  group_by(minu) %>%
  summarise(r = mean(retu),
            n = n())


plotRetAsPrice(rets$ret, 100)










