# Plot using GDI

pairs(mtcars)

# Plot using HTML in an external browser

library(shiny)
library(ggvis)

# Plot some graphs ---

mtcars %>%
    ggvis( ~ wt, ~ mpg) %>%
    layer_points() %>%
    layer_smooths()

# ----

# Dynamic plotting with random values

dat <- data.frame(
  g1 = rep(letters[1:4], 3),
  g2 = rep(LETTERS[1:3], each = 4),
  value = runif(12))
ddat <- reactive({
    invalidateLater(2000, NULL)
    dat$value <<- runif(12)
    dat
    })
ddat %>% ggvis(x = ~g1, y = ~value, fill = ~g2, fillOpacity := 0.5) %>%
  layer_bars()
