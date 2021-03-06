Performance of for loops and lapply in R
========================================================

Function 1 and 2 are do doing the same task. Both generate some random numbers and repeat this for iter-times.


```r
library(ggplot2)

out <- list()
fun1 <- function(iter) {
    force(iter)
    f <- function() {
        for (i in 1:iter) {
            out[[i]] <- cbind(rnorm(1000), rnorm(1000), rnorm(1000))
        }
    }
    return(f)
}

fun2 <- function(iter) {
    force(iter)
    f <- function() {
        lapply(as.list(1:iter), function(x) cbind(rnorm(1000), rnorm(1000), 
            rnorm(1000)))
    }
    return(f)
}

repetitions <- seq(10000, 1e+05, 10000)


list.fun1 <- lapply(as.list(repetitions), function(x) fun1(x))
list.fun2 <- lapply(as.list(repetitions), function(x) fun2(x))

list.time.fun1 <- lapply(list.fun1, function(fun) system.time(fun()))
list.time.fun2 <- lapply(list.fun2, function(fun) system.time(fun()))

time.fun1 <- as.data.frame(do.call(rbind, list.time.fun1))
time.fun2 <- as.data.frame(do.call(rbind, list.time.fun2))

time.fun1$indicator <- "for"
time.fun2$indicator <- "lapply"

df <- rbind(cbind(time.fun1, repetitions), cbind(time.fun2, repetitions))
```


lapply seems to be linear in the amount of time it needs for more iterations. for might have an exponential behavior.




```r
qplot(x = repetitions, user.self, data = df, colour = as.factor(indicator), 
    geom = "line") + scale_x_continuous(breaks = repetitions) + theme_bw() + 
    labs(x = "iterations", colour = "function")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


