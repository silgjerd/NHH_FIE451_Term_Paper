annsharpe <- function(x) {(mean(x) * 12) / (sd(x) * sqrt(12))}

plotRetAsPrice <- function(x, start = 1, end = length(x)){
  p <- cumprod(c(1, (1 + x[start:end])))
  plot(p, type = "l")
  cat("Sharpe:", annsharpe(x[start:end]))
}
