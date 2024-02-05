library(tidyverse)
library(ggplot2)
library(nls.multstart)

### MUTATIONS ###
TPC <- TPC |>
  group_by(bact, temp) |>
  mutate(finalavg = mean(final, na.rm = TRUE)) |>
  mutate(ravg = mean(r, na.rm = TRUE)) |>
  mutate(rlogavg = mean(rlog, na.rm = TRUE)) |>
  ungroup(bact, temp)
  
TPC <- TPC |>
  filter(final >= 1) |>
  mutate(r = log(final / initial))
  mutate(rscale = 10, rlog = log(r + rscale))
  ungroup

TPC <- TPC |>
  mutate(rscaleavg = NULL)

### SCATTERPLOTS ###
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

ggplot(TPC, aes(x = temp, y = finalavg, shape = bact)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs (
    x = "Temperature",
    y = "Average Count",
    title = "Thermal Performance Curve",
    shape = "Bacteria"
  )

ggplot(TPC, aes(x = temp, y = finalavg)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs (
    x = "Temperature",
    y = "Average Count",
    title = "Thermal Performance Curve"
  )


#| label: plot-xtemp-yr

ggplot(TPC, aes(x = temp, y = r, color = rep, shape = bact)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Temperature",
    y = "R",
    title = "Thermal Performance Curve",
    color = "Replicate",
    shape = "Bacteria"
  )

ggplot(TPC, aes(x = temp, y = r, color = rep)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs(
    x = "Temperature",
    y = "R",
    title = "Thermal Performance Curve",
    color = "Replicate",
  )


#| label: plot-xtemp-yravg

ggplot(TPC, aes(x = temp, y = ravg, shape = bact)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Temperature",
    y = "R Average",
    title = "Thermal Performance Curve",
    shape = "Bacteria"
  )

ggplot(TPC, aes(x = temp, y = ravg)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs(
    x = "Temperature",
    y = "R Average",
    title = "Thermal Performance Curve",
  )


#| label: plot-xtemp-yrlog

ggplot(TPC, aes(x = temp, y = rlog, color = rep, shape = bact)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Temperature",
    y = "R Log",
    title = "Thermal Performance Curve",
    color = "Replicate",
    shape = "Bacteria"
  )

ggplot(TPC, aes(x = temp, y = rlog, color = rep)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs(
    x = "Temperature",
    y = "R Log",
    title = "Thermal Performance Curve",
    color = "Replicate"
  )


#| label: plot-xtemp-yrlogavg

ggplot(TPC, aes(x = temp, y = rlogavg, shape = bact)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Temperature",
    y = "R Log Average",
    title = "Thermal Performance Curve",
    shape = "Bacteria"
  )

ggplot(TPC, aes(x = temp, y = rlogavg)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~bact) +
  labs(
    x = "Temperature",
    y = "R Log Average",
    title = "Thermal Performance Curve",
  )
