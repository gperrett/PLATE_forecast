library(baseballr)
library(tidyverse)

ip_to_decimal <- function(ip) {
  ip    <- as.numeric(as.character(ip))
  whole <- floor(ip)
  whole + round((ip - whole) * 10) / 3
}

# Pull and clean one level. Check args(mlb_stats) if sport_ids errors on your version.
pull_stats <- function(sport_id, league, season = 2025) {
  mlb_stats(
    stat_type   = "season",
    stat_group  = "pitching",
    season      = season,
    sport_ids   = sport_id,
    player_pool = "All"
  ) |>
    select(player_id, player_full_name, earned_runs, era, whip, home_runs,
           base_on_balls, hit_by_pitch, strike_outs, innings_pitched,
           strikeouts_per9inn, games_pitched, games_started, age, walks_per9inn, home_runs_per9) |>
    mutate(
      across(-c(player_full_name, innings_pitched), as.numeric),
      innings_pitched = ip_to_decimal(innings_pitched),
      league = league, 
      season = season
    )
}

pitching_stats <- bind_rows(
  pull_stats(1,  "mlb", season = 2026),
  pull_stats(11, "aaa", season = 2026),
  pull_stats(1,  "mlb", season = 2025),
  pull_stats(11, "aaa", season = 2025),  
  pull_stats(1,  "mlb", season = 2024),
  pull_stats(11, "aaa", season = 2024),
  pull_stats(1,  "mlb", season = 2023),
  pull_stats(11, "aaa", season = 2023),
  pull_stats(1,  "mlb", season = 2022),
  pull_stats(11, "aaa", season = 2022),
  pull_stats(1,  "mlb", season = 2021),
  pull_stats(11, "aaa", season = 2021)
)

# Sanity checks
pitching_stats |> count(league, season)                        # rows per level look right?
pitching_stats |> count(player_id, league, season) |> filter(n > 1)   # split-team duplicate rows?

# FIP with a per-league constant
pitching_stats <- pitching_stats |>
  group_by(league, season) |>
  mutate(
    lg_era   = 9 * sum(earned_runs, na.rm = TRUE) / sum(innings_pitched, na.rm = TRUE),
    lg_core  = (13 * sum(home_runs, na.rm = TRUE) +
                  3 * (sum(base_on_balls, na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE)) -
                  2 * sum(strike_outs, na.rm = TRUE)) / sum(innings_pitched, na.rm = TRUE),
    constant = lg_era - lg_core
  ) |>
  ungroup() |>
  mutate(
    fip = if_else(
      innings_pitched > 0,
      (13 * home_runs + 3 * (base_on_balls + hit_by_pitch) - 2 * strike_outs) /
        innings_pitched + constant,
      NA_real_
    )
  ) |>
  select(-lg_era, -lg_core)

# Merge on player_id (MLBAM id) + league, not on name
pitching_stats <- pitching_stats |>
  rename(matchup.pitcher.fullName = player_full_name)
