library(ggplot2)
data(CO2)

str(CO2)
gg.scatter <- ggplot(aes(x = uptake, y = conc, colour = Treatment), data = CO2) + geom_point()

gg.scatter + scale_color_brewer(palette = "Blues")

#ggplot2 - Elegant Graphics for Data Analysis
#Chapter 2
##############################################################################################
#qplot-examples

data(diamonds)

#Scatterplot
qplot(carat, price, data = diamonds)

qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data = diamonds)

#scatterplot with colour/shape
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

#scatterplot with alpha
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))

#Other geoms: "point", "smooth", "boxplot", "path" - continous data
#"histogram", "density" - distribution
#"bar" - discrete
#smooth
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 1)
#smooth for generalized additive models
library(mgcv)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x, bs = "cs"))

library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ ns(x,5))

library(MASS)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "rlm")

#geom jitter/boxplot
qplot(color, price / carat, data = diamonds)
qplot(color, price / carat, data = diamonds, geom = "boxplot")
qplot(color, price / carat, data = diamonds, geom = "jitter")
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1/5))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1/50))

#geom - histogram/density
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

qplot(carat, data = diamonds, geom = "histogram", binwidth = 1)
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1)
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01)

#density/histogram with colour/fill
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)

#geom - bar
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")

#geom - line
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics, geom = c("path"), colour = year(date))

#faceting
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


#Chapter 4
##############################################################################################
#Build a plot layer by layer
#layer-function
library(scales)
library(ggplot2)
library(mlmRev)
p <- ggplot(data = diamonds, aes(carat, price, colour = cut))
p <- p + layer(geom = "point") #layer(geom, geom_params, stat, stat_params, data, mapping, position)

p <- ggplot(diamonds, aes(x = carat))
p + layer(geom = "bar", 
          geom_params = list(fill = "steelblue"),
          stat = "bin",
          stat_params = list(binwidth = 2))
#...the same as:
p + geom_histogram(binwidth = 2, fill = "steelblue")

#Compare with qplot
data(msleep)
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()
qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()
qplot(sleep_rem / sleep_total, awake, data = msleep, geom = c("point", "smooth"))
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()

p <- qplot(sleep_rem / sleep_total, awake, data = msleep)
summary(p)

#Regular R-Objekts
bestfit <- geom_smooth(method = "lm", colour = alpha("steelblue", .5), se = F, size = 2)
p + bestfit

qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit

#Aesthetic mapping
data(mtcars)
p <- ggplot(mtcars)
p + aes(wt, hp) + geom_point()

p <- ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()

p + geom_point(aes(colour = factor(cyl))) #+ geom_smooth(method = "lm", se = F, colour = "black")
p + geom_point(aes(y = disp))

#Setting vs. Mapping
p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "dodgerblue")
p + geom_point(aes(colour = "dodgerblue"))

#Multiple Groups
data(Oxboys)
p <- ggplot(Oxboys, aes(age, height, group = Subject)) 
p + geom_line()
p + geom_line(aes(group = 1))

#Different groups different layers
p + geom_line() + geom_smooth(method = "lm", se = F)
p + geom_line() + geom_smooth(aes(group = 1), method = "lm", size = 2, se = F)

#Overriding default grouping
boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox + geom_line(aes(group = Subject), colour = "dodgerblue") # alpha("dodgerblue", 0.5)

#More
#Position
ggplot(diamonds, aes(x = clarity, y = ..count.., fill = cut)) + geom_bar(position = "stack")
ggplot(diamonds, aes(x = clarity, y = ..count.., fill = cut)) + geom_bar(position = "fill")
ggplot(diamonds, aes(x = clarity, y = ..count.., fill = cut)) + geom_bar(position = "dodge") 
# +
#   scale_fill_hue(h = c(1,100))
#   scale_fill_grey()
#   scale_fill_manual(values = rainbow(7))

ggplot(diamonds, aes(x = clarity, y = ..count.., fill = cut)) + geom_bar(position = "identity")
ggplot(diamonds, aes(x = clarity, y = ..count.., colour = cut)) + geom_freqpoly(aes(group = cut))












