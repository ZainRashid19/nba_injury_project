# ============================================================
# Phase 1: Sumitrodatta — Games-Missed Gap (full-history proxy)
# Source: Kaggle, sumitrodatta/nba-aba-baa-stats
# Files: Player Totals.csv, Team Summaries.csv
# ============================================================

library(dplyr)
library(stringr)

# ---- 1. Load and inspect --------------------------------------------------

player_totals  <- read.csv("C:/Users/zainh/Downloads/Player Totals.csv",  stringsAsFactors = FALSE)
team_summaries <- read.csv("C:/Users/zainh/Downloads/Team Summaries.csv", stringsAsFactors = FALSE)

str(player_totals)
str(team_summaries)

# ---- 2. Restrict to NBA only (both files also contain ABA/BAA rows) ------

player_totals  <- player_totals  %>% filter(lg == "NBA")
team_summaries <- team_summaries %>% filter(lg == "NBA")

# ---- 3. Exclude multi-team summary rows (2TM, 3TM, 4TM...) ---------------
# These duplicate games already counted in the individual team rows -
# recall Moe Becker: 3TM(43) = PIT(17) + BOS(6) + DTF(20)

player_single_team <- player_totals %>%
  filter(!str_detect(team, "^\\dTM$"))

nrow(player_totals)         # before exclusion
nrow(player_single_team)    # after - should be meaningfully smaller

# ---- 4. Build a clean team-season "games scheduled" table -----------------

team_games <- team_summaries %>%
  mutate(team_games_played = w + l) %>%
  select(season, abbreviation, team_games_played)

# ---- 5. Join player games onto their team's season total ------------------

games_gap <- player_single_team %>%
  left_join(team_games, by = c("season" = "season", "team" = "abbreviation")) %>%
  mutate(games_missed = team_games_played - g)

sum(is.na(games_gap$team_games_played))   # rows that failed to match - report this number

# ---- 6. First look: does the trend even move in a plausible direction? ---

games_gap$decade <- (games_gap$season %/% 10) * 10

games_gap %>%
  group_by(decade) %>%
  summarise(
    avg_games_missed = mean(games_missed, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  print()
