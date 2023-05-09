## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----setup--------------------------------------------------------------------
library(tidygam)
library(mgcv)
library(dplyr)
library(ggplot2)
theme_set(theme_light())

## ----gest---------------------------------------------------------------------
data("gest")
gest

## ----gs-----------------------------------------------------------------------
gs <- gam(
  count ~ s(months, k = 3),
  data = gest,
  family = poisson
)

summary(gs)

## ----gs-pred------------------------------------------------------------------
gs_pred <- predict_gam(gs)
gs_pred

## ----gs-pred-plot-------------------------------------------------------------
gs_pred %>%
  plot(series = "months")

## ----gs-pred-exp--------------------------------------------------------------
predict_gam(gs, tran_fun = exp) %>%
  plot(series = "months")

## ----gs-by--------------------------------------------------------------------
gs_by <- gam(
  count ~ s(months, by = background, k = 3),
  data = gest,
  family = poisson
)

summary(gs_by)

## ----gs-by-comp---------------------------------------------------------------
gs_by %>%
  predict_gam(length_out = 20, series = "months", tran_fun = exp) %>%
  plot(comparison = "background")

## ----gs-by-comp-2-------------------------------------------------------------
gs_by %>%
  predict_gam(length_out = 20, series = "months", tran_fun = exp) %>%
  plot(comparison = "background") +
  scale_color_brewer(type = "qual") + scale_fill_brewer(type = "qual")

## ----gs-by-2------------------------------------------------------------------
gs_by_2 <- gam(
  count ~ s(months, by = background, k = 3) +
    s(months, by = gesture, k = 3),
  data = gest,
  family = poisson
)

summary(gs_by_2)

## ----gs-by-2-plot-------------------------------------------------------------
gs_by_2 %>%
  predict_gam(length_out = 20, series = "months", tran_fun = exp) %>%
  plot(comparison = "gesture") +
  scale_color_brewer(type = "qual") + scale_fill_brewer(type = "qual") +
  facet_grid(~ background)

## ----gs-i---------------------------------------------------------------------
gest <- gest %>%
  mutate(back_gest = interaction(background, gesture))

gs_i <- gam(
  count ~ s(months, by = back_gest, k = 3),
  data = gest,
  family = poisson
)

summary(gs_i)

## ----gs-i-plot----------------------------------------------------------------
predict_gam(
  gs_i, tran_fun = exp,
  separate = list(back_gest = c("background", "gesture"))
) %>%
  plot(series = "months", comparison = "gesture") +
  facet_grid(~ background)

## ----struct-------------------------------------------------------------------
data("struct")
struct

## ----st-----------------------------------------------------------------------
struct <- struct %>%
  mutate(stim_gram = interaction(stimulus.condition, grammar.condition))

st <- bam(
  voltage ~
    s(t, by = stim_gram, k = 5) +
    s(t, subject, bs = "fs", m = 1),
  data = struct
)

summary(st)

## ----st-plot------------------------------------------------------------------
predict_gam(
  st,
  length_out = 50,
  series = "t",
  exclude_terms = "s(t,subject)",
  separate = list(stim_gram = c("stimulus", "grammar"))
) %>%
  plot(comparison = "grammar") +
  geom_hline(yintercept = 0) +
  facet_grid(~ stimulus)

## ----st-plot-2----------------------------------------------------------------
predict_gam(
  st,
  length_out = 50,
  series = "t",
  separate = list(stim_gram = c("stimulus", "grammar"))
) %>%
  plot(comparison = "grammar") +
  geom_hline(yintercept = 0) +
  facet_grid(~ stimulus)

