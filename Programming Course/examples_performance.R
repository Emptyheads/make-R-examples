library(ggplot2)


fun1 <- function(iter) {
  force(iter)
  out <- list()
  f <- function() {
    for (i in 1:iter) {
      out[[i]] <<- cbind(rnorm(1000), rnorm(1000), rnorm(1000))
    }
  }
  return(f)
}

fun2 <- function(iter) {
  force(iter)
  out <- NULL
  f <- function() {
    out <<- lapply(as.list(1:iter), function(x) cbind(rnorm(1000), rnorm(1000), rnorm(1000)))  
  }
  return(f)
}

repetitions <- seq(10000, 100000, 10000)


list.fun1 <- lapply(as.list(repetitions), function(x) fun1(x))
list.fun2 <- lapply(as.list(repetitions), function(x) fun2(x))

list.time.fun1 <- lapply(list.fun1, function(fun) system.time(fun()))
list.time.fun2 <- lapply(list.fun2, function(fun) system.time(fun()))

time.fun1 <- as.data.frame(do.call(rbind, list.time.fun1))
time.fun2 <- as.data.frame(do.call(rbind, list.time.fun2))

time.fun1$indicator <- "for"
time.fun2$indicator <- "lapply"

df <- rbind(cbind(time.fun1, repetitions), cbind(time.fun2, repetitions))
save(df, file = "Programming Course/run_time.RData")

qplot(x = repetitions, user.self, data = df, colour = as.factor(indicator), geom = "line") +
  scale_x_continuous("iterations", breaks = repetitions)



