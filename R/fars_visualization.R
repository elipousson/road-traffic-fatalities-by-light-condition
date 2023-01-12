library(dplyr)
library(ggplot2)


fips <- crashapi:::fips_state_table |> 
  transmute(
    STATE = as.integer(fips),
    state_name = stringr::str_to_title(name)
  )

fars <- readr::read_csv(here::here("output", "fars.csv"))

fars_select <- fars |> 
  select(
    STATE, YEAR, LGT_COND, A_PERINJ, A_PTYPE
  ) |> 
  mutate(
    light_condition = case_when(
      LGT_COND == 1 ~ "Daylight",
      LGT_COND == 2 ~ "Dark – Not Lighted",
      LGT_COND == 3 ~ "Dark – Lighted",
      LGT_COND == 4 ~ "Dawn",
      LGT_COND == 5 ~ "Dusk",
      LGT_COND == 6 ~ "Dark – Unknown Lighting",
      LGT_COND == 7 ~ "Other",
      LGT_COND == 8 ~ "Not Reported",
      LGT_COND == 9 ~ "Unknown"
    ),
    person_type = case_when(
      A_PTYPE == 1 ~ "Driver",
      A_PTYPE == 2 ~ "Occupant",
      A_PTYPE == 3 ~ "Pedestrian",
      A_PTYPE == 4 ~ "Pedalcyclist",
      A_PTYPE == 5 ~ "Other/Unknown Non-Occupants"
  ),
  injury_type = case_when(
    A_PERINJ == 1 ~ "Fatal",
    A_PERINJ == 6 ~ "Survivor in Fatal Crash"
  ),
  YEAR = as.Date(paste0(YEAR, "-01-01"))
  ) |> 
  left_join(fips) |> 
  janitor::clean_names()
  

fatal_light_condition <- fars_select |> 
  filter(
    injury_type == "Fatal",
    !(light_condition %in% c("Unknown", "Not Reported"))
  ) |> 
  mutate(
    light_condition = forcats::fct_collapse(
      light_condition,
      "Light" = "Daylight",
      "Dark" = c("Dark – Not Lighted", "Dark – Lighted", "Dark – Unknown Lighting"),
      "Other" = c("Dawn", "Dusk", "Other")
    )
  )

fatal_light_condition_state_pct <- fatal_light_condition |> 
  count(year, state_name, person_type, light_condition, name = "count") |> 
  left_join(count(fatal_light_condition, year, state_name, person_type, name = "type_count")) |> 
  mutate(
    pct_count = count / type_count
  )

fatal_light_condition_state_pct |> 
  filter(
    light_condition == "Dark"#,
    # state_name == "California"
  ) |> 
  ggplot(aes(year, pct_count, color = person_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~ person_type) +
  guides(color = "none") +
  cols4all::scale_color_discrete_c4a_cat(palette = "tol.vibrant") +
  labs(
    title = "Share of fatal crashes for each state in dark light conditions by person type"
  )

fatal_light_condition_pct <- fatal_light_condition |> 
  count(year, person_type, light_condition, name = "count") |> 
  left_join(count(fatal_light_condition, year, person_type, name = "type_count")) |> 
  mutate(
    pct_count = count / type_count
  )

labs_x_yr <- function(...) {
  list(
    scale_x_date(),
    ggplot2::labs(
      ...,
      x = "Year",
      caption = "Source: NHTSA FARS, 2011-2020"
    )
  )
}

fatal_light_condition_pct |> 
  filter(
    light_condition == "Dark"
    ) |> 
  ggplot(aes(year, pct_count, color = person_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~ person_type) +
  guides(color = "none") +
  cols4all::scale_color_discrete_c4a_cat(palette = "tol.vibrant") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs_x_yr(
    y = "% of fatal crashes in dark light conditions",
    color = "Person type"
  )

fars_select |> 
  count(year, person_type) |> 
  ggplot(aes(year, n, color = person_type)) +
  ggshadow::geom_shadowline(size = 1.25) +
  ggshadow::geom_shadowpoint(size = 1.75) +
  cols4all::scale_color_discrete_c4a_cat(palette = "tol.vibrant") +
  scale_y_log10() +
  facet_wrap(~ person_type) +
  theme_light() +
  labs_x_yr(
    y = "Fatal crashes (#)",
    color = "Person type"
  )
