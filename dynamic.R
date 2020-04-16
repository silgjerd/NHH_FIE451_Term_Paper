library(tidyverse);library(zoo);library(lubridate);library(vroom)
library(TTR);library(PerformanceAnalytics)
library(xgboost);library(ranger);library(caret)
rm(list=ls());options(scipen=999,stringsAsFactors=F)

annsharpe <- function(x) {(mean(x) * 12) / (sd(x) * sqrt(12))}

crsp <- vroom("crsp.csv", col_types = cols(RET = col_double(), date = col_date(format = "%Y%m%d")))
mom <- vroom("mom.CSV", col_types = cols(date = col_date(format = "%Y%m%d")))

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

rv <- mom %>%
  mutate(yearmon = paste(year(date), ifelse(nchar(month(date))==1, paste("0", month(date), sep = ""), month(date)), sep = "")) %>%
  group_by(yearmon) %>%
  summarise(rvmom = sd(mom)) %>%
  ungroup %>%
  mutate(y = lead(rvmom)) %>%
  na.omit


# ====================================================================================

rets <- NULL

for (month in sort(unique(mdata$yearmon))) { #for each month
  
  cat(month, "\n")
  
  csubset <- mdata %>% filter(yearmon == month)
  
  # all top n momentum
  # select the one with least dispersion (lowest SD)
  
  # DOWN
  dnum <- 5
  moms <- c("mom1nl", "mom2nl", "mom4nl", "mom2", "mom4", "mom6", "mom8", "mom10", "mom12")
  momd <- bind_cols("mom1nl" = csubset %>% arrange(mom1nl) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom2nl" = csubset %>% arrange(mom2nl) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom4nl" = csubset %>% arrange(mom4nl) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom2" = csubset %>% arrange(mom2) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom4" = csubset %>% arrange(mom4) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom6" = csubset %>% arrange(mom6) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom8" = csubset %>% arrange(mom8) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom10" = csubset %>% arrange(mom10) %>% select(RET) %>% head(dnum) %>% unname,
                    "mom12" = csubset %>% arrange(mom12) %>% select(RET) %>% head(dnum) %>% unname)
  
  momdsd <- sapply(momd, sd)
  minsd <- moms[which(momdsd==min(momdsd))][1]
  
  minsdmom <- csubset %>% select(minsd)
  
  retd <- csubset[order(pull(minsdmom)),] %>% select(RET) %>% head(dnum) %>% colMeans %>% unname
  
  # UP
  unum <- 15
  moms <- c("mom1nl", "mom2nl", "mom4nl", "mom2", "mom4", "mom6", "mom8", "mom10", "mom12")
  momu <- bind_cols("mom1nl" = csubset %>% arrange(-mom1nl) %>% select(RET) %>% head(unum) %>% unname,
                    "mom2nl" = csubset %>% arrange(-mom2nl) %>% select(RET) %>% head(unum) %>% unname,
                    "mom4nl" = csubset %>% arrange(-mom4nl) %>% select(RET) %>% head(unum) %>% unname,
                    "mom2" = csubset %>% arrange(-mom2) %>% select(RET) %>% head(unum) %>% unname,
                    "mom4" = csubset %>% arrange(-mom4) %>% select(RET) %>% head(unum) %>% unname,
                    "mom6" = csubset %>% arrange(-mom6) %>% select(RET) %>% head(unum) %>% unname,
                    "mom8" = csubset %>% arrange(-mom8) %>% select(RET) %>% head(unum) %>% unname,
                    "mom10" = csubset %>% arrange(-mom10) %>% select(RET) %>% head(unum) %>% unname,
                    "mom12" = csubset %>% arrange(-mom12) %>% select(RET) %>% head(unum) %>% unname)
  
  momusd <- sapply(momu, sd)
  minsd <- moms[which(momusd==min(momusd))][1]
  
  minsdmom <- csubset %>% select(minsd)
  
  retu <- csubset[order(pull(minsdmom)),] %>% select(RET) %>% head(unum) %>% colMeans %>% unname
  
  rets <- bind_rows(rets, bind_cols(yearmon = month,
                                    dsd = which(momdsd==min(momdsd))[1],
                                    usd = which(momusd==min(momusd))[1],
                                    
                                    
                                    retd = retd,
                                    retu = retu,
                                    ret  = (0.5*retu) - (0.5*retd)))
  
}

bind_cols(shr = annsharpe(rets$ret),
          shu = annsharpe(rets$retu),
          shd = annsharpe(rets$retd))




# RET2PRICE
startidx <- 200
prc <- cumprod(c(1, (1 + rets$ret[1:431])))
plot(prc, type = "l")



rets %>%
  group_by(dsd) %>%
  summarise(r = mean(retd),
            n = n())
rets %>%
  group_by(usd) %>%
  summarise(r = mean(retu),
            n = n())

rets[rets$usd==7,]$retu %>% hist




################################################################################# OLD

rets <- NULL

for (month in sort(unique(mdata$yearmon))) { #for each month
  
  cat(month, "\n")
  
  csubset <- mdata %>% filter(yearmon == month)
  
  # all top n momentum
  # select the one with least dispersion (lowest SD)
  
  retd <- bind_cols("mom1nl" = csubset %>% arrange(mom1nl) %>% select(RET) %>% head(14) %>% unname,
                    "mom2nl" = csubset %>% arrange(mom2nl) %>% select(RET) %>% head(14) %>% unname,
                    "mom4nl" = csubset %>% arrange(mom4nl) %>% select(RET) %>% head(14) %>% unname,
                    "mom2" = csubset %>% arrange(mom2) %>% select(RET) %>% head(14) %>% unname,
                    "mom4" = csubset %>% arrange(mom4) %>% select(RET) %>% head(14) %>% unname,
                    "mom6" = csubset %>% arrange(mom6) %>% select(RET) %>% head(14) %>% unname,
                    "mom8" = csubset %>% arrange(mom8) %>% select(RET) %>% head(14) %>% unname,
                    "mom10" = csubset %>% arrange(mom10) %>% select(RET) %>% head(14) %>% unname,
                    "mom12" = csubset %>% arrange(mom12) %>% select(RET) %>% head(14) %>% unname)
  retdsd <- sapply(retd, sd)
  which(retdsd==min(retdsd))
  
  
  
  retd1 <- csubset %>% arrange(mom1nl) %>% select(RET) %>% tail(14) %>% colMeans %>% unname
  #retd2 <- csubset %>% arrange(mom2nl) %>% select(RET) %>% tail(5) %>% colMeans %>% unname
  #retd3 <- csubset %>% arrange(mom4nl) %>% select(RET) %>% tail(5) %>% colMeans %>% unname
  
  retu1 <- csubset %>% arrange(mom1nl) %>% select(RET) %>% head(14) %>% colMeans %>% unname
  #retu2 <- csubset %>% arrange(mom2nl) %>% select(RET) %>% head(5) %>% colMeans %>% unname
  #retu3 <- csubset %>% arrange(mom4nl) %>% select(RET) %>% head(5) %>% colMeans %>% unname
  
  
  creturn <- (0.5*retu1) - (0.5*retd1)
  
  # COMOMENTUM
  comd1 <- csubset %>% arrange(mom1nl) %>% select(mom1nl) %>% tail(14) %>% sapply(sd) %>% unname
  comu1 <- csubset %>% arrange(mom1nl) %>% select(mom1nl) %>% head(14) %>% sapply(sd) %>% unname
  
  rets <- bind_rows(rets, bind_cols(yearmon = month,
                                          return = creturn,
                                          retd = retd1,
                                          retu = retu1,
                                          comd = comd1,
                                          comu = comu1))
  
  
}

rets <- rets %>%
  mutate(
    dscale = .1 / comd,
    uscale = .1 / comu
  )

rets <- rets %>%
  mutate(
    retdscl = retd * dscale,
    retuscl = retu * uscale,
    retscl  = (0.5*retuscl) - (0.5*retdscl)
  )


annsharpe(rets$retscl)






# RET2PRICE
startidx <- 1
prc <- cumprod(c(1, (1 + rets$return[startidx:431])))
plot(prcs, type = "l")
prcs <- cumprod(c(1, (1 + rets$retscl[startidx:431])))
matplot(prc, type = "l", add = T, col = "red")



# Volatility scaling
pvol <- NULL
for (i in 241:nrow(rv)) {
  
  cat(i, "\n")
  train <- rv[1:(i-1),]
  test  <- rv[i,]
  
  fit <- lm(y ~ rvmom, data = train)
  pred <- predict(fit, test) %>% unname
  
  pvol <- bind_rows(pvol, bind_cols(yearmon = test$yearmon,
                                    volt1   = pred))
  
}

target_vol <- mean(rv$rvmom)
pvol <- mutate(pvol, weight = target_vol / volt1)

rets$yearmon <- as.character(rets$yearmon)
rets <- left_join(rets, pvol, by = "yearmon")

rets <- mutate(rets,
                  scaledret = return * weight)

prices <- tibble(prcreg = (cumprod(c(1, (1 + rets$return)))),
                 prcscl = (cumprod(c(1, (1 + rets$scaledret)))))

ggplot(prices, aes(x = seq_along(prcreg))) +
  geom_line(aes(y = prcreg)) +
  geom_line(aes(y = prcscl), col = "red") +
  theme_bw()






