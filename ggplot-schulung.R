library(ggplot2)
library(mgcv)
library(splines)
library(MASS)
library(mlmRev)
library(scales)
library(plyr)
library(maps)

data(diamonds)
qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x *y * z, data = diamonds)

set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, size = cut, colour = color)

qplot(carat, price, data = diamonds, alpha = I(1/100))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ poly(x, 3))

qplot(color, price / carat, data = diamonds, geom = "jitter")

qplot(carat, data = diamonds, geom = c("density", "histogram"), fill = color, 
      alpha = I(1/3))

qplot(color, data = diamonds, geom = "bar")

qplot(carat, ..count.., data = diamonds, geom = "histogram",
      binwidth = 0.1, xlim = c(0,3))

qplot(carat, ..density.., data = diamonds, geom = "histogram",
      binwidth = 0.1, xlim = c(0,3))

#############################################################

p <- ggplot(data = diamonds, aes(x = carat, y = price, colour = cut))
p + layer(geom = "point")

p <- ggplot(diamonds, aes(carat))
p + layer(geom = "bar",
          geom_params = list(fill = "dodgerblue4"),
          stat = "bin",
          stat_params = list(binwidth = 2))

p + geom_histogram(binwidth = 2, fill = "dodgerblue4")

qplot(carat, price, data = dsmall, geom = "point")
ggplot(dsmall, aes(carat, price)) + geom_point()
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))


p <- ggplot(dsmall, aes(carat, price)) + geom_point() + geom_smooth()

bestfit <- geom_smooth(method = "lm", 
                       colour = alpha("dodgerblue4", 0.5),
                       se = F, size = 2)

ggplot(dsmall, aes(price, carat)) + geom_point() + bestfit


data(mtcars)

p <- ggplot(mtcars)
p <- p + aes(wt, hp)

p + geom_point(colour = "dodgerblue4")
p + geom_point(aes(colour = "dodgerblue4"))

ggplot(diamonds, aes(x = clarity, y = ..density.., fill = cut)) +
  geom_bar(position = "fill")


d <- ggplot(diamonds, aes(carat)) + xlim(0,3)
d + stat_bin(aes(ymax = ..count..), 
              binwidth = 0.1, geom = "point")

data(msleep)

p <- ggplot(msleep, aes(sleep_total, sleep_cycle, colour = vore)) +
  geom_point()

p + scale_colour_hue(name = "Legend Title",
                     breaks = c("herbi", "carni", "omni"),
                     label = c("A", "B", "C"))

p + scale_colour_brewer(palette = "Set1")

p + scale_colour_manual(values = c("blue", "green", "yellow", "black"))

mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))
p <- ggplot(mpg2)
p + geom_point(aes(cty, hwy, colour = cyl)) + facet_grid(.~cyl, margins = T)


p <- ggplot(data = dsmall, aes(carat, price, colour = cut)) + geom_point() + 
  scale_colour_brewer(palette = "Set1")+
  scale_y_continuous(breaks = c(0, 2500, 17500)) +
  labs(title = "Title", x = "x-A", colour = "JSD")
p + theme_bw()
p + theme_minimal()

p  + theme_bw() + theme(axis.title.y = element_text(angle = 180),
          legend.position = "bottom")

ggsave(filename = "test.pdf", plot = p)










