library(ggplot2)
#For reproduceability
set.seed(2)
#Generating data
df <- data.frame(Number = rnorm(200), Obs =1:200)
#Calculating the cumulative Mean
df$c.sum <- cumsum(df$Number) / df$Obs
#Constructing the plot
p <- ggplot(df, aes(Obs, Number)) + 
  geom_point(alpha = 0.3) + 
  geom_line(aes(y = c.sum, colour = "Observed Mean")) + 
  geom_hline(aes(yintercept = 0, colour = "True Mean"), show_guide = TRUE) +
  labs(y = "Random Number from N(0,1)", colour = "Legend", title = "Illustration of the mean") +
  scale_color_manual(values = c("black", "red"), breaks = c("Observed Mean", "True Mean")) +
  theme_bw()


