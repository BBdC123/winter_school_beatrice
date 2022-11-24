# gg dogs because why not 

library(ggdogs)
library(ggplot2)

grid <- expand.grid(1:5, 3:1)

df <- data.frame(x = grid[, 1],
                 y = grid[, 2],
                 image = c("doge", "doge_strong", "chihuahua",
                           "eyes", "gabe", "glasses",
                           "tail", "surprised", "thisisfine",
                           "hearing", "pug", "ears",
                           "husky", "husky_2", "chilaquil"))

ggplot(df) +
  geom_dog(aes(x, y, dog = image), size = 5) +
  geom_text(aes(x, y - 0.5, label = image), size = 2.5) +
  xlim(c(0.25, 5.5)) + 
  ylim(c(0.25, 3.5))


# create a plot

# Sample data
set.seed(1)
df <- data.frame(x = 1:10, y = rnorm(10))

# Plot
ggplot(df, aes(x = x, y = y)) +
  geom_dog(size = 5)




# Plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 3, color = 4) +
  geom_dog(aes(x = 7, y = -0.5), dog = "thisisfine", size = 5) +
  geom_label(aes(x = 7.75, y = -0.1, label = "This is fine"))
