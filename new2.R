#libraries needed
library(tidyverse)
library(mixtools)
library(ghibli) #' if I want to look less professional by having 
#' studio ghibli themed plots on my blog that's my choice, okay?

proportional_gauss <- function(x, mean, sd, lambda, n, binwidth) {
  (dnorm(x, mean = mean, sd = sd)) * n * binwidth * lambda
}


proportional_gauss_sum <- function(x, mean, sd, lambda, n, binwidth) {
  apply(mapply(proportional_gauss,
               x = x,
               MoreArgs = list( mean, sd, lambda, n, binwidth)
  ), 
  2, sum)
}

ggplot(observations, aes(x = value)) +
  geom_histogram(binwidth = 0.05, fill = ghibli_palettes$PonyoMedium[2]) +
  stat_function(
    fun = proportional_gauss_sum,
    args = list(
      mean = my_mix[["mu"]],
      sd = my_mix[["sigma"]],
      lambda = my_mix[["lambda"]],
      n = length(observations$value),
      binwidth = 0.05
    ),
    colour = ghibli_palettes$PonyoMedium[5], size = 1
  )
