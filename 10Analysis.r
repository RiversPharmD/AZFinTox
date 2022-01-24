###############################################################################
## Purpose: this file loads joined datasets and analyzes them.

## Depends:
## O7COSTTidyJoin.r
## 06PredictorJoin.r
## tidyverse

## Inputs:
## IntData/cost.csv
## IntData/pred_wide.csv
## IntData/pred.csv

## Outputs:
##

###############################################################################

## Packages
library(tidyverse)

## Functions


## Data In

### COST
cost <- read_csv("IntData/cost.csv",
  col_types = "ffdddd"
)

## Partner vs Caregiver Scatters

### Demographics



### COST

ggplot(
  data = cost,
  mapping = aes(
    x = facit_cost_sum_p,
    y = facit_cost_sum_c
  )
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(facets = "redcap_event_name") +
  ggtitle("Correlation between partner and caregiver COST")

## Convergence over Time

### COST

#### Directional
ggplot(
  data = cost,
  mapping = aes(
    x = redcap_event_name,
    y = facit_cost_con_dir
  )
) +
  geom_boxplot() +
  ggtitle("Directional concordance between partner and caregiver COST")

#### Absolute
  ggplot(
    data = cost,
    mapping = aes(
      x = redcap_event_name,
      y = facit_cost_con_abs
    )
  ) +
    geom_boxplot() +
    ggtitle("Absolute concordance between partner and caregiver COST")
