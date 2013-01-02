##############################################################################################
#Wrapper functions in R - http://rsnippets.blogspot.de/2012/12/wrapper-functions-in-gnu-r.html

f <- function (x, a) { 
  (x - a) ^ 2 
}

optimize(f, c(0, 1), tol = 0.0001, a = 1/3)

#add tracing - print
f <- function (x, a) {
  print(x)
  (x - a) ^ 2
}
optimize(f, c(0, 1), tol = 0.0001, a = 1/3)

#Wrapper - return list-object - elements are function - envirenment contains calls (another object)
#See S3 and S4 objects
wrap.f <- function(f, minimum = TRUE) {
  calls <- NULL
  return(list(f = function(x, ...) {
    calls <<- c(calls, x)
    ifelse(minimum,f(x, ...), -f(x, ...))  
    },
              getter = function() return(calls),
              reset = function() calls <<- NULL ))
}

wf <- wrap.f(function (x, a) { (x - a) ^ 2 })
print(optimize(wf$f, c(0, 1), tol = 0.0001, a = 1/3))
wf$getter()
wf$reset()
print(wf$getter())
