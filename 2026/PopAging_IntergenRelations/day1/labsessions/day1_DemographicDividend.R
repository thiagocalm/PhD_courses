#'--------------
#'HANDS-ON DEMOGRAPHIC DIVIDEND
#'--------------


# packages ----------------------------------------------------------------

library(pacman)
p_load(readxl, tidyverse)

# Importing data ----------------------------------------------------------

# consumption and production
cons_prod <- readxl::read_xlsx(
  file.path(here::here(),"courses","2026","PopAging_IntergenRelations","day1","labsessions","Demographic Dividend.xlsx"),
            sheet = 1
)

# population
pop <- readxl::read_xlsx(
  file.path(here::here(),"courses","2026","PopAging_IntergenRelations","day1","labsessions","Demographic Dividend.xlsx"),
  sheet = 2
)


# data handle -------------------------------------------------------------

cons_prod <- cons_prod |>
  pivot_longer(`0`:`100`, names_to = "age", values_to = "cons_prod_value")

pop <- pop |>
  select("Country" = `Region, subregion, country or area *`, everything()) |>
  pivot_longer(`0`:`100+`, names_to = "age", values_to = "pop")

# joining data

df <- pop |>
  left_join(
    cons_prod |> select(-Year) |> rename("ConsProd" = "VarName"),
    by = join_by(Country,age)
  )


# Implementing calculation ------------------------------------------------


###
# Selecting countries
###

# country
cntry = "Ethiopia"

df <- df |>
  filter(Country %in% cntry)

###
# Calculating by age
###

df <- df |>
  mutate(
    value_weighted = pop * cons_prod_value
  )

###
# Aggregating values for the total
###

df_tot <- df |>
  summarise(
    value = sum(value_weighted),
    .by = c(Country, Year,ConsProd)
  ) |>
  na.omit() |>
  pivot_wider(
    names_from = ConsProd,
    values_from = value
  )

###
# Computing ESR
###

df_tot <- df_tot |>
  mutate(
    ESR = YL / C
  )

###
# Growth Rates
###

df_tot <- df_tot |>
  mutate(
    ESR_lag = lag(ESR)
  ) |>
  summarise(
    ESR = ESR,
    lnESR = log(ESR),
    lnESR_lag = log(ESR_lag),
    rate = lnESR - lnESR_lag,
    .by = c(Country, Year)
  )
