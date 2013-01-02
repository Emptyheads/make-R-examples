library(ggplot2)
data(CO2)

str(CO2)
gg.scatter <- ggplot(aes(x = uptake, y = conc, colour = Treatment), data = CO2) + geom_point()

gg.scatter + scale_color_brewer(palette = "Blues")

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

