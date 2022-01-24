## Analysis

#### Trends over time

##### COST

###### Patient
ggplot(
  data = cost,
  mapping = aes(
    x = redcap_event_name,
    y = facit_cost_sum_p,
    group = redcap_event_name
  )
) +
  geom_boxplot()


  ### Visualisations


  ggplot(
    data = dat_long,
    aes(x = value)
  ) +
    geom_histogram() +
    facet_wrap(vars(observation))

  ### Individual Vars

  #### Children and childrennum

  children <- dat %>%
    select(children, childrennum) %>%
    group_by(children, childrennum) %>%
    summarise(count = n())

  #### Education

  education <- dat %>%
    select(education) %>%
    mutate(education = as_factor(education)) %>%
    group_by(education) %>%
    count()

  #### Ethnicity

  ethnicity <- dat %>%
    select(ethnicity) %>%
    mutate(ethnicity = as_factor(ethnicity)) %>%
    group_by(ethnicity) %>%
    count()

  #### Gender

  gender <- dat %>%
  select(gender) %>%
  mutate(gender = as_factor(gender)) %>%
  group_by(gender) %>%
  count()

  #### Income

  income <- dat %>%
  select(income) %>%
  mutate(income = as_factor(income)) %>%
  group_by(income) %>%
  count()

  #### Marital

  marital <- dat %>%
  select(marital) %>%
  mutate(marital = as_factor(marital)) %>%
  group_by(marital) %>%
  count()

  #### race

  race <- dat %>%
  select(race) %>%
  mutate(race = as_factor(race)) %>%
  group_by(race) %>%
  count()

  #### Age

  age <- dat %>%
  select(age) %>%
  mutate(age = if_else(age < 30, "<30",
  if_else(age < 40, "30-39",
  if_else(age < 50, "40-49",)))) %>%
  group_by(age) %>%
  count()
