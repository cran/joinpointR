## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  warning = FALSE,
  collapse = TRUE,
  dev = "ragg_png",
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(joinpointR)

# Load example data
data(hiv_data)

## -----------------------------------------------------------------------------
## Create a reduced dataset
data_sex <- hiv_data |>
  filter(admin == "ARG")

## Fit the joinpoint model
mods_sex <- model_jp_grid(
  data = data_sex,
  rate = hiv_rate,
  time = year,
  group = "sex"
)

## -----------------------------------------------------------------------------
## Create a reduced dataset
data_admin <- hiv_data |>
  filter(between(admin, "Buenos Aires", "Chaco"))

## Fit the joinpoint model
mods_admin <- model_jp_grid(
  data = data_admin,
  rate = hiv_rate,
  time = year,
  group = c("admin", "sex")
)

## -----------------------------------------------------------------------------
# Fit the model using the weighted BIC
mods_wbic <- model_jp_grid(
  data = data_admin,
  rate = hiv_rate,
  time = year,
  group = c("sex", "admin"),
  method = "wbic"
)

## -----------------------------------------------------------------------------
# BIC table for all the models
bic_jp(mods_admin)

# BIC table for a single model
bic_jp(mods_admin[1])

## -----------------------------------------------------------------------------
get_summary(mods_sex)

## -----------------------------------------------------------------------------
get_summary(mods_sex, stats = "apc")

get_apc(mods_sex)

## -----------------------------------------------------------------------------
get_summary(mods_sex, stats = "aapc")

get_aapc(mods_sex)

## -----------------------------------------------------------------------------
#|id: get_summary-4
# Hide the confidence interval
get_summary(mods_sex, hide = "ci")

# Hide the significance stars
get_summary(mods_sex, hide = "sig")

## -----------------------------------------------------------------------------
get_summary(mods_sex, level.ci = .9)

## -----------------------------------------------------------------------------
get_summary(mods_sex, as.ft = TRUE)

## -----------------------------------------------------------------------------
gg_jpoint(mods_sex)

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin)

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid")

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2")

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", color.by = "trend")

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", color.by = "segment")

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", color.by = "period")

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", aapc = TRUE)

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", jp = FALSE)

## -----------------------------------------------------------------------------
gg_jpoint(mods_admin, facets = "grid2", geom = "line")

## -----------------------------------------------------------------------------
gg_jpoint_line(mods_admin, facets = "grid2")

## -----------------------------------------------------------------------------
gg_jpoint(mods_sex, facets = "grid2", geom = "area")

## -----------------------------------------------------------------------------
gg_jpoint_area(mods_sex, facets = "grid2")

## -----------------------------------------------------------------------------
gg_jpoint_line(mods_sex) +
  scale_cbpal_color(palette = "algae")

gg_jpoint_area(mods_sex, color.by = "trend") +
  scale_cbpal_fill(palette = "blue_fluoride")

## -----------------------------------------------------------------------------
plot_cbpal(type = "div")

plot_cbpal(series = "scico")

