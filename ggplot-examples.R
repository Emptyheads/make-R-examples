library(ggplot2)
library(mgcv)
library(splines)
library(MASS)
library(mlmRev)
library(scales)
library(plyr)
library(maps)


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
#qplot(carat, price, data = dsmall, size = cut)

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

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x, bs = "cs"))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ poly(x, 3))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ ns(x,5))

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
qplot(carat, data = diamonds, geom = "density", fill = color, alpha = I(1/3))
qplot(carat, data = diamonds, geom = "histogram", fill = color)

#geom - bar
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + ylab("carat") #sum(diamonds[diamonds$color == "H",]$carat)

#geom - line
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics, geom = c("path"), colour = year(date))

#faceting
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


qplot(x = displ, y = hwy, data = mpg, colour = factor(cyl))


#Chapter 4
##############################################################################################
#Build a plot layer by layer
#layer-function

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
# data(msleep)
# ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()
# qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()
# qplot(sleep_rem / sleep_total, awake, data = msleep, geom = c("point", "smooth"))
# ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()
# 
# p <- qplot(sleep_rem / sleep_total, awake, data = msleep)
# summary(p)

qplot(carat, price, data = dsmall, geom = "point")
ggplot(dsmall, aes(x = carat, y = price)) + geom_point()
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = dsmall, geom = "point") + geom_smooth()
ggplot(dsmall, aes(x = carat, y = price)) + geom_point() + geom_smooth()

p <- qplot(carat, price, data = dsmall, geom = "point")
summary(p)

#Regular R-Objekts
bestfit <- geom_smooth(method = "lm", colour = alpha("steelblue", .5), se = F, size = 2)
p + bestfit
ggplot(dsmall, aes(x = carat, y = price)) + geom_point() + bestfit
ggplot(dsmall, aes(x = log(carat), y = log(price))) + geom_point() + bestfit

# qplot(sleep_rem, sleep_total, data = msleep) + bestfit
# qplot(awake, brainwt, data = msleep, log = "y") + bestfit
# qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit

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


#Combine geoms and stats
d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")

d + stat_bin(aes(size = ..density..), binwidth = 0.1, geom = "point")
d + stat_bin(aes(y = 1, fill = ..count..), binwidth = 0.1, geom = "tile", position = "identity")

#Chapter 5
##############################################################################################
#Toolbox
#Basic plot types

df <- data.frame(x = c(3,1,5), y = c(2, 4, 6), label = c("a", "b", "c"))

p <- ggplot(df, aes(x,y, label = label)) + xlab(NULL) + ylab(NULL)
p + geom_point() + ggtitle("geom_point")
p + geom_bar(stat = "identity") + ggtitle("geom_bar - \"identity\"")
p + geom_line() + ggtitle("geom_line")
p + geom_area() + ggtitle("geom_area")
p + geom_path() + ggtitle("geom_path")
p + geom_text() + ggtitle("geom_text")
p + geom_tile() + ggtitle("geom_tile")
p + geom_polygon() + ggtitle("geom_polygon")

#Displaying distributions
depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), binwidth = 0.1)

ggplot(diamonds, aes(x = cut, y = depth)) + geom_boxplot()
ggplot(diamonds, aes(x = carat, y = depth, group = round_any(carat, 0.1, floor))) + geom_boxplot() + xlim(0,3)

ggplot(diamonds, aes(x = depth)) + geom_density(aes(fill = cut), alpha = 0.2) + xlim(54, 70)

#Overplotting
df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x,y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".")

norm + geom_point(colour = alpha("black", 1/10))
norm + geom_point(colour = "red", alpha = 1/10)

td <- ggplot(diamonds, aes(table, depth)) + xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
td + geom_jitter(position = position_jitter(width = 0.5))
td + geom_jitter(position = position_jitter(width = 0.5), alpha = 1/50)

############
#Drawing maps
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
ggplot(big_cities, aes(x = long, y = lat)) + geom_point() + borders("state", size = 0.5)

tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) + geom_point() + borders("county", "texas", colour = "grey70")

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, by = "region")
choro <- choro[order(choro$order),]
ggplot(choro, aes(x = long, y = lat, group = group, fill = assault)) + geom_polygon()
ggplot(choro, aes(x = long, y = lat, group = group, fill = assault/murder)) + geom_polygon()


#Chapter 6
##############################################################################################
#scales, axes and legends
p <- ggplot(msleep, aes(sleep_total, sleep_cycle, colour = vore)) + geom_point()
p + scale_color_hue(name = "What does\nit eat?", 
                    breaks = c("herbi", "carni", "omni"), 
                    labels = c("plants", "meat", "both"))

p + scale_color_hue(guide = guide_legend(title = "What does\nit eat?", 
                    breaks = c("herbi", "carni", "omni"), 
                    labels = c("plants", "meat", "both")))

p + scale_colour_brewer(palette = "Set1")

p + labs(x = "X", y = "Y", colour = "What does\nit eat", title = "Titel")

p <- ggplot(mtcars, aes(cyl, wt)) + geom_point()
p + scale_x_continuous(breaks = c(5.5, 6.5))
p + scale_x_continuous(limits = c(5.5, 6.5))

p <- ggplot(mtcars, aes(wt, cyl, colour = cyl)) + geom_point()
p + scale_colour_gradient(breaks = c(5.5, 6.5))
p + scale_colour_gradient(limits = c(5.5, 6.5))

p + xlim(5,2)


point <- ggplot(msleep, aes(log(brainwt), log(bodywt), colour = vore)) + geom_point()
point + scale_colour_brewer(palette = "Set1")
point + scale_colour_brewer(palette = "Set2")
point + scale_colour_brewer(palette = "Pastel1")

bar <- ggplot(diamonds, aes(x = clarity, y = ..count.., fill = cut)) + geom_bar(position = "fill")
bar + scale_fill_brewer(palette = 2)
bar + scale_fill_brewer(palette = 3)
bar + scale_fill_brewer(palette = "Set1")
bar + scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue"))
bar + scale_fill_manual(values = c("Fair" = "red", "Good" = "orange", "Premium" = "yellow", "Ideal" = "green", "Very Good" = "blue"))

data(LakeHuron)
huron <- data.frame(year = 1875:1972, level = LakeHuron)
ggplot(huron, aes(year)) + 
  geom_line(aes(y = level - 5), colour = "blue") +
  geom_line(aes(y = level + 5), colour = "red")

ggplot(huron, aes(year)) + 
  geom_line(aes(y = level - 5, colour = "below")) +
  geom_line(aes(y = level + 5, colour = "above"))

gg.huron <- ggplot(huron, aes(year)) + 
  geom_line(aes(y = level - 5, colour = "below")) +
  geom_line(aes(y = level + 5, colour = "above"))

gg.huron  + scale_colour_manual(values = c("below" = "blue", "above" = "red")) + labs(colour = "Direction")


#Chapter 7
##############################################################################################
#Faceting
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))
p <- ggplot(mpg2)
p + geom_point(aes(cty, hwy)) + facet_grid(. ~ cyl)
p + geom_histogram(aes(x = cty, y = ..count..)) + facet_grid(cyl ~ .)
p + geom_point(aes(cty, hwy)) + facet_grid(drv ~ cyl)

p + geom_point(aes(cty, hwy)) + facet_grid(drv ~ cyl, margins = T)
p + geom_point(aes(cty, hwy)) + facet_grid(drv ~ cyl, margins = c("cyl"))

p + geom_point(aes(cty, hwy)) + facet_wrap(drv ~ cyl, nrow = 2, ncol = 4, scales = "free")

#Chapter 8
##############################################################################################
#Themes
p <- ggplot(data = dsmall, aes(carat, price, colour = cut)) +
  geom_point() +
  scale_colour_brewer("Legend", palette = "Set1", labels = c("Label1", "Label2", "Label3", "Label4", "Label5")) + 
  scale_x_continuous(breaks = pretty_breaks(n=5)) + 
  labs(title = "Title", x = "X-Label", y = "Y-Label") + 
  scale_y_continuous(breaks = c(0,5000,10000, 15000, 17500)) +
  theme_bw() 
p + theme(axis.title.y = element_text(colour = "yellow", face="bold"),
                     axis.text.y = element_text(colour = "black", angle=10),
        #legend.position = c(0.1, 0.5))
        legend.position = "bottom")

p + theme_bw()
p + theme_classic()
p + theme_gray()
p + theme_minimal()


#Offene Übung
##############################################################################################
#Beispiel 1
set.seed(1000)
diamonds.sample <- diamonds[sample(rownames(diamonds),2000),]

p <- ggplot(diamonds.sample, aes(color, price)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(colour = "Black", alpha = 1/15, shape = "|", size = 12) +
  labs(x = "Price", y = "Color", title = "Übung") +
  scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000)) +
  coord_flip() +
  theme_classic() 
ggsave(filename = "example_plot_1.pdf", plot = p, width = 10, height = 5)

b <- ggplot(diamonds.sample, aes(x = log(price), y = log(carat))) + 
  geom_point(aes(colour = color)) + 
  facet_grid(cut~.) +
  scale_colour_manual(values = c("blue", "grey", "grey", "grey", "grey", "grey", "grey"), breaks = c("D", "E"), labels = c("D","else")) +
  geom_smooth(method = "lm", colour = "black", linetype = 2, se = F) + 
  labs(title = "Übung 2", x = "Log price", y = "Log carat", colour = "Colour legend\ncolor") +
  theme_bw() + theme(legend.position = "bottom", axis.title.y = element_text(angle = 0))

ggsave(filename="example_plot_2.pdf", plot = b, width = 10, height = 10)




