#############################################################
#Mitschriften/Übungen - für R

#Creating Vectors (atomic)
x <- c(0.5, 0.6) #numeric
x <- c(TRUE, FALSE) #logical
x <- c(T, F) #logical
x <- c("a", "b", "c") #character
x <- 9:29 #integer
x <- c(1+0i, 2+4i) #complex

#What is the class of x?
x <- 4
class(x)

x <- c(4, TRUE)
class(x)

#What are the dimension/class of z?
x <- c(1,2,3)
y <- c(2,3,4)
z <- rbind(x,y)
class(z)

#What is the result of x + y?
x <- 1:4
y <- 2
x+y

#Suppose I have a vector x <- c(3, 5, 1, 10, 12, 6) and I want to set all elements of this vector that are less than 6 to be equal to zero. What R code achieves this?
x[x >= 6] <- 0
x[x %in% 1:5] <- 0
x[x == 6] <- 0
x[x > 6] <- 0

#Propose an alternative:
x[x<6] <- 0


