x <- rnorm(100, mean = 2, sd = 2)
y <- 0.5 * x + rnorm(100)

df <- data.frame(x = x, y = y, id = 1:100)
df <- df[order(df$x),]

write.csv(df, "Programming Course/Data/t_data.csv")
dir("Programming Course/Data/")