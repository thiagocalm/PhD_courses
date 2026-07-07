#'--------------
#'HANDS-ON LIFECYCLE DEFICITS
#'--------------


# packages ----------------------------------------------------------------

library(pacman)
p_load(readxl, tidyverse)

# Importing data ----------------------------------------------------------

# lcd dataset
lcd <- readxl::read_xlsx(
  file.path(here::here(),"courses","2026","PopAging_IntergenRelations","day2","labsessions","Exercise 2 2026.xlsx"),
  sheet = 1
)

# Implementing calculation ------------------------------------------------

# Computing LCD

lcd <- lcd |>
  mutate(LCD = C - LY)

# Weighting pop

lcd_w <- lcd |>
  mutate(
    C = C * pop,
    LY = LY * pop,
    TG = TG * pop,
    TF = TF * pop,
    RA = RA * pop,
    LCD_pop = LCD * pop,
    LCD_pc = LCD / pop
  ) |>
  select(-pop)

# discovering the age limits

lcd_w |>
  ggplot() +
  aes(x = Age, y = LCD_pc, color = Education) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  theme_classic()

# decomposition of transfers by type of transfer

lcd_w |>
  select(-C, -LY, -LCD, -LCD_pop, -LCD_pc) |>
  pivot_longer(
    TG:RA,
    names_to = "type",
    values_to = "value"
  ) |>
  ggplot() +
  aes(x = Age, y = value, fill = type) +
  geom_area() +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 1.1) +
  facet_wrap(. ~ Education) +
  theme_classic()
