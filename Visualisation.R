library(ggplot2)
library(dplyr)

set.seed(123)
data <- data.frame(
  x = rnorm(100, mean = 50, sd = 10),
  y = rnorm(100, mean = 50, sd = 10)
)

model <- lm(y ~ x, data = data)

data <- data %>%
  mutate(pred = predict(model),
         resid = residuals(model))

p <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  ggtitle("Graphique combiné avec tendance linéaire et histogramme des résidus") +
  theme_minimal()

print(p)

p_resid <- ggplot(data, aes(x = resid)) +
  geom_histogram(binwidth = 1, fill = 'gray', color = 'black') +
  ggtitle("Histogramme des résidus") +
  theme_minimal()

print(p_resid)
