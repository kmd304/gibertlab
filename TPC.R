library(tidyverse)
library(ggplot2)
library(nls.multstart)


#| label: plot-xtemp-yfinal

ggplot(TPC, aes(x = temp, y = final, color = rep, shape = bact)) +
  geom_point () +
  geom_smooth(se = FALSE) +
  labs (
    x = "Temperature",
    y = "Count",
    title = "Thermal Performance Curve",
    color = "Replicate",
    shape = "Bacteria"
  )


#| label: plot-xtemp-yfinal

ggplot(TPC, aes(x = temp, y = final, color = rep)) +
  geom_point () +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs (
    x = "Temperature",
    y = "Count",
    title = "Thermal Performance Curve",
    color = "Replicate"
  )


#| label: plot-xtemp-yfinalavg

TPC <- TPC |>
  group_by(bact, temp) |>
  mutate(finalavg = mean(final, na.rm = TRUE)) 

ggplot(TPC, aes(x = temp, y = finalavg, shape = bact)) +
  geom_point() +
  labs (
    x = "Temperature",
    y = "Average Count",
    title = "Thermal Performance Curve",
    shape = "Bacteria"
  )



