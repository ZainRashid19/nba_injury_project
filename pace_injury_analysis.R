# ============================================================
# Phase 6: Pace vs. Injury Rate — Team-Season Join
# Sources:
#   Injuries  - Kaggle, loganlauton/nba-injury-stats-1951-2023
#   Pace/W/L  - Kaggle, sumitrodatta/nba-aba-baa-stats, Team Summaries.csv
#
# Question this script sets up (not answers): does team-season pace
# track team-season injury rate, once the pre-1994/95 reporting
# artifact is excluded and raw counts are normalized to a rate.
# ============================================================

library(dplyr)
library(stringr)

# ---- Part A: Rebuild the classified injury table --------------------------
# Same Phase 1-4 logic already validated. Re-run here rather than assuming
# `rel` still exists in the environment, so this script is self-contained.

injuries <- read.csv("C:/Users/zainh/Downloads/NBA Player Injury Stats(1951 - 2023).csv", stringsAsFactors = FALSE)

str(injuries)          # confirm column names before referencing "Team" below -
# if the actual column is named differently, fix Part C

injuries$Date <- as.Date(injuries$Date, format = "%Y-%m-%d")

rel <- injuries %>% filter(Relinquished != "" & !is.na(Relinquished))
rel$Notes <- tolower(rel$Notes)

specific_injury_kw <- paste(
  "ankle","knee","hamstring","achilles","acl","mcl","meniscus",
  "rotator cuff","shoulder","groin","back","wrist","elbow","hip",
  "calf","quad","hernia","concussion","fracture","sprain","strain",
  "torn","surgery","contusion","bruise","soreness","sore ","plantar",
  "stress","tendinitis","tendonitis","tendinopathy","dislocat",
  "foot","toe","thigh","leg","hand","rib","heel","finger","neck","adductor",
  "thumb","eye","tibia","shin","tailbone","facial","abdominal",
  "cervical","fibula","biceps","spine","nose","chest injury",
  "arm(?!y)","broken",
  sep = "|"
)

rel <- rel %>%
  mutate(category = if_else(str_detect(Notes, specific_injury_kw), "specific_injury", "other"))

# ---- Part B: Restrict to the reliable reporting window ---------------------
# Every non-injury cause is already stripped out (category == "specific_injury").
# Now also drop everything before the 1994/95 structural break, per the
# Phase 5 decision: pre-1994/95 counts are not comparable across years.
# NBA seasons span Oct(year) - Jun(year+1); a season is labeled by its
# ENDING year in Team Summaries.csv (e.g. the 2025-26 season is "2026"),
# so a transaction date is assigned to a season by that convention.

rel <- rel %>%
  mutate(season = if_else(
    as.integer(format(Date, "%m")) >= 8,           # Aug onward = next season's calendar start
    as.integer(format(Date, "%Y")) + 1,
    as.integer(format(Date, "%Y"))
  ))

injury_events <- rel %>%
  filter(category == "specific_injury", season >= 1995, season <= 2023)

# ---- Part C: Team-name diagnostic — do this BEFORE trusting any join ------
# prosportstransactions labels teams by their name AT THE TIME of the
# transaction (SuperSonics, Bobcats, Nets, etc). Team Summaries.csv uses
# current abbreviations. These will NOT match automatically across
# relocations/renames. Inspect both sets before joining anything.

injury_team_names <- sort(unique(injury_events$Team))
print(injury_team_names)   # eyeball this list for old/relocated franchise names

team_summaries <- read.csv("C:/Users/zainh/Downloads/Team Summaries.csv", stringsAsFactors = FALSE)
team_summaries <- team_summaries %>% filter(lg == "NBA")

summary_team_names <- sort(unique(team_summaries$team))
print(summary_team_names)  # full franchise names as of each season, e.g. "Seattle SuperSonics"

# team_summaries$team is season-specific (it already reflects relocations/
# renames per season), which is what we actually want to match against -
# NOT team_summaries$abbreviation, which can also change over time.
# Confirm by checking a known relocation shows up correctly on both sides:
team_summaries %>% filter(str_detect(team, "SuperSonics|Thunder")) %>%
  distinct(season, team) %>% arrange(season)

# ---- Part D: Nickname -> full franchise name mapping -----------------------
# The real problem wasn't relocation, it was simpler: injury_events$Team is
# a bare nickname ("Bulls"), team_summaries$team is "City Nickname"
# ("Chicago Bulls"). An exact-string join between those never matches
# anything, which is why ALL 33 teams failed, not just the relocated ones.
#
# Most nicknames map to exactly one full name for the whole 1995-2023
# window. A few franchises changed identity mid-window and need the
# season to disambiguate: Sonics/Thunder, Nets (NJ/Brooklyn), Grizzlies
# (Vancouver/Memphis), Hornets (Charlotte -> New Orleans -> Charlotte,
# with a 2-season Katrina-displacement name in between).
#
# VERIFY the Hornets/Nets/Grizzlies season boundaries below against your
# own data before trusting them - run the same distinct(season, team)
# check used above for Sonics/Thunder, e.g.:
#   team_summaries %>% filter(str_detect(team, "Hornets|Bobcats|Pelicans")) %>%
#     distinct(season, team) %>% arrange(season)
# The boundary seasons here are my best recollection of the franchise
# history, not something I pulled from your actual file - don't run the
# rest of this script until you've confirmed them.

injury_events <- injury_events %>%
  mutate(match_team = case_when(
    Team == "76ers"        ~ "Philadelphia 76ers",
    Team == "Blazers"      ~ "Portland Trail Blazers",
    Team == "Bobcats"      ~ "Charlotte Bobcats",
    Team == "Bucks"        ~ "Milwaukee Bucks",
    Team == "Bullets"      ~ "Washington Bullets",
    Team == "Bulls"        ~ "Chicago Bulls",
    Team == "Cavaliers"    ~ "Cleveland Cavaliers",
    Team == "Celtics"      ~ "Boston Celtics",
    Team == "Clippers"     ~ "Los Angeles Clippers",
    Team == "Grizzlies" & season <= 2001 ~ "Vancouver Grizzlies",
    Team == "Grizzlies" & season >= 2002 ~ "Memphis Grizzlies",
    Team == "Hawks"        ~ "Atlanta Hawks",
    Team == "Heat"         ~ "Miami Heat",
    Team == "Hornets" & season <= 2002          ~ "Charlotte Hornets",
    Team == "Hornets" & season %in% 2003:2005   ~ "New Orleans Hornets",
    Team == "Hornets" & season %in% 2006:2007   ~ "New Orleans/Oklahoma City Hornets",
    Team == "Hornets" & season %in% 2008:2013   ~ "New Orleans Hornets",
    Team == "Hornets" & season >= 2015          ~ "Charlotte Hornets",
    Team == "Jazz"         ~ "Utah Jazz",
    Team == "Kings"        ~ "Sacramento Kings",
    Team == "Knicks"       ~ "New York Knicks",
    Team == "Lakers"       ~ "Los Angeles Lakers",
    Team == "Magic"        ~ "Orlando Magic",
    Team == "Mavericks"    ~ "Dallas Mavericks",
    Team == "Nets" & season <= 2012 ~ "New Jersey Nets",
    Team == "Nets" & season >= 2013 ~ "Brooklyn Nets",
    Team == "Nuggets"      ~ "Denver Nuggets",
    Team == "Pacers"       ~ "Indiana Pacers",
    Team == "Pelicans"     ~ "New Orleans Pelicans",
    Team == "Pistons"      ~ "Detroit Pistons",
    Team == "Raptors"      ~ "Toronto Raptors",
    Team == "Rockets"      ~ "Houston Rockets",
    Team == "Sonics"       ~ "Seattle SuperSonics",
    Team == "Spurs"        ~ "San Antonio Spurs",
    Team == "Suns"         ~ "Phoenix Suns",
    Team == "Thunder"      ~ "Oklahoma City Thunder",
    Team == "Timberwolves" ~ "Minnesota Timberwolves",
    Team == "Warriors"     ~ "Golden State Warriors",
    Team == "Wizards"      ~ "Washington Wizards",
    TRUE ~ NA_character_
  ))

# Anything still NA here is a nickname this mapping didn't anticipate -
# check this is empty before moving on.
injury_events %>% filter(is.na(match_team)) %>% distinct(Team) %>% print()

# ---- Part E: Team-season injury counts, then the actual join --------------

injury_counts <- injury_events %>%
  group_by(season, match_team) %>%
  summarise(n_injuries = n(), .groups = "drop")

team_season <- team_summaries %>%
  transmute(season, team, abbreviation, pace, x3p_ar, team_games_played = w + l)

pace_injury <- injury_counts %>%
  left_join(team_season, by = c("season" = "season", "match_team" = "team"))

# How many team-seasons failed to match? This number needs to be small
# and explainable (not silently dropped) before anything downstream is
# trustworthy.
sum(is.na(pace_injury$pace))
pace_injury %>% filter(is.na(pace)) %>% distinct(match_team) %>% print()

# ---- Part F: Normalize to a rate, not a raw count --------------------------
# Raw injury counts are not comparable across team-seasons with different
# numbers of games played (strike-shortened seasons, 2020 bubble, etc).
# injury_rate = specific injuries per team-game played.

pace_injury <- pace_injury %>%
  filter(!is.na(pace), !is.na(team_games_played), team_games_played > 0) %>%
  mutate(injury_rate = n_injuries / team_games_played)

# ---- Part G: Look at the shapes before testing anything ---------------------
# Per the plan: eyeball pace-over-time and injury_rate-over-time separately
# first. Don't jump straight to a correlation - you already know pace is
# NOT monotonic across this range (1980s pace was higher than today's),
# so a naive scatter across all seasons pooled could hide or fabricate a
# relationship depending on what each era is doing independently.

season_level <- pace_injury %>%
  group_by(season) %>%
  summarise(
    avg_pace = mean(pace, na.rm = TRUE),
    avg_injury_rate = mean(injury_rate, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)

ggplot(season_level, aes(x = season, y = avg_pace)) +
  geom_line(color = "darkorange") +
  labs(title = "League-average pace by season (1995-2023)", y = "Pace", x = "Season")

ggplot(season_level, aes(x = season, y = avg_injury_rate)) +
  geom_line(color = "steelblue") +
  labs(title = "League-average specific-injury rate per team-game (1995-2023)",
       y = "Injuries per team-game", x = "Season")

# Team-season scatter, the actual unit of analysis for the hypothesis test -
# this is what a correlation/regression would run on next, not yet computed
# here on purpose. Look at this before deciding what test even makes sense.
ggplot(pace_injury, aes(x = pace, y = injury_rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Team-season pace vs. specific-injury rate (1995-2023)",
       x = "Pace (possessions per 48 min)", y = "Injuries per team-game")

# ---- Part H: Is the pace-injury slope just the COVID seasons? -------------
# Season 2020 (2019-20, bubble) and Season 2021 (2020-21, 72-game
# compressed schedule) are known confounds independent of pace - the
# 2021 schedule compression in particular is a documented injury-rate
# driver on its own. Check whether the loess uptick in the chart above
# survives with these seasons excluded before trusting it.

season_level %>% filter(season %in% 2019:2022) %>% print()

pace_injury_no_covid <- pace_injury %>% filter(!season %in% c(2020, 2021))

ggplot(pace_injury_no_covid, aes(x = pace, y = injury_rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Team-season pace vs. injury rate, COVID seasons excluded",
       x = "Pace (possessions per 48 min)", y = "Injuries per team-game")

# Result (2026-09-22 run): slope survives - injury rate still climbs from
# ~0.13 at pace 85-90 to ~0.27 at pace 100+ with 2020/2021 excluded. Not
# a COVID artifact. But season 2022's rate (0.357) matches 2021's spike
# even with the compressed schedule over, which raises a bigger question:
# is this a real pace effect, or is EVERYTHING (pace and injury rate)
# just trending up together over the same 29 years for unrelated reasons?
# That's what Part I tests.

# ---- Part I: Strip the shared time trend before trusting the slope --------
# Both pace and injury_rate have risen over the same 29 years, possibly
# for unrelated reasons (better injury reporting/diagnosis over time,
# load-management culture inflating relinquishment counts without more
# actual tissue damage, rule changes). Pooling raw values across 29
# seasons conflates "faster teams get hurt more" with "later years have
# more of both." De-trend by comparing each team to its OWN season's
# average, so the question becomes within-season, not across-era.

pace_injury_detrended <- pace_injury %>%
  group_by(season) %>%
  mutate(
    pace_vs_season_avg = pace - mean(pace, na.rm = TRUE),
    rate_vs_season_avg = injury_rate - mean(injury_rate, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(pace_injury_detrended, aes(x = pace_vs_season_avg, y = rate_vs_season_avg)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Within-season pace vs. injury rate (de-trended)",
       x = "Pace relative to that season's league average",
       y = "Injury rate relative to that season's league average")

# Result (2026-09-22 run): loess is flat, effectively zero, across the
# full range (-10 to +7). The Part G/H uptick does not survive de-trending.
# This is the actual answer to the research question as currently posed:
# no visible within-season pace effect. Part J puts a number on this
# instead of relying on "the loess looks flat."

# ---- Part J: Season fixed-effects regression -------------------------------
# The de-trend above did the right thing (compare each team to its own
# season) but did it by hand, which only adjusts the point estimates, not
# the standard errors, and gives no p-value. A linear model with season
# as a factor does the same "control for what year it is" job properly:
# factor(season) adds one dummy variable per season, so the pace
# coefficient is estimated AFTER partialling out every season's average
# level of both pace and injury_rate - exactly the within-season
# comparison from Part I, but with a proper test attached.
#
# This is NOT the same as just adding season as a numeric predictor,
# which would only remove a straight-line time trend and miss the
# COVID-era spike and other non-linear year-to-year jumps. factor(season)
# absorbs whatever each individual season actually did, regardless of
# shape.

fe_model <- lm(injury_rate ~ pace + factor(season), data = pace_injury)
summary(fe_model)

# The coefficient to read is the one labeled "pace" - that's the
# within-season effect of a 1-unit pace increase on injury_rate, holding
# season fixed. Check its Estimate, Std. Error, and Pr(>|t|). A small,
# non-significant pace coefficient here (p > 0.05) would confirm what
# Part I's flat loess already suggested: no detectable within-season
# pace effect, once era is controlled for.

# Simple correlation on the de-trended values, as a plain-language
# companion number to report alongside the regression:
cor.test(pace_injury_detrended$pace_vs_season_avg, pace_injury_detrended$rate_vs_season_avg)

# ---- Part K: Does pooling injury types wash out a fatigue-specific signal? -
# The pace-as-fatigue hypothesis is a claim about a SPECIFIC injury
# mechanism (repetitive stress, soft tissue breakdown), not about injuries
# in general. Pooling a broken nose from a flagrant foul in with a
# non-contact hamstring tear adds noise from a mechanism pace has no
# plausible link to. If the pace effect is real but concentrated in
# soft-tissue/overuse injuries, splitting it out could reveal a signal
# the pooled "specific_injury" category washed out.
#
# This split is a judgment call, not a clean partition - many terms
# (ankle, knee, shoulder, back) can be either contact or non-contact
# depending on the actual event, and the Notes text alone can't always
# tell you which. Rather than force ambiguous terms into either bucket,
# they're left OUT of both subsets below and only appear in the original
# pooled specific_injury category. This keeps each subset's classification
# more confident at the cost of a smaller n test - the tradeoff between
# the pooled test (D) and this one is: pooled = noisier signal but keeps
# nearly everyone; soft-tissue = cleaner mechanism-match but excludes
# roughly two-thirds of specific_injury rows judged ambiguous. Read this
# result as a check on whether the pooled null holds up, not as more
# statistically powerful than the pooled test.

soft_tissue_kw <- paste(
  "hamstring","achilles","acl","mcl","meniscus","groin","strain",
  "tendinitis","tendonitis","tendinopathy","plantar","stress",
  "calf","quad","adductor",
  sep = "|"
)

blunt_trauma_kw <- paste(
  "fracture","broken","contusion","bruise","concussion","dislocat",
  "facial","nose","rib","eye",
  sep = "|"
)

rel <- rel %>%
  mutate(injury_mechanism = case_when(
    category != "specific_injury" ~ NA_character_,
    str_detect(Notes, soft_tissue_kw)  ~ "soft_tissue",
    str_detect(Notes, blunt_trauma_kw) ~ "blunt_trauma",
    TRUE ~ "ambiguous"
  ))

rel %>% filter(category == "specific_injury") %>% count(injury_mechanism)

soft_tissue_events <- rel %>%
  filter(injury_mechanism == "soft_tissue", season >= 1995, season <= 2023) %>%
  mutate(match_team = case_when(
    Team == "76ers"        ~ "Philadelphia 76ers",
    Team == "Blazers"      ~ "Portland Trail Blazers",
    Team == "Bobcats"      ~ "Charlotte Bobcats",
    Team == "Bucks"        ~ "Milwaukee Bucks",
    Team == "Bullets"      ~ "Washington Bullets",
    Team == "Bulls"        ~ "Chicago Bulls",
    Team == "Cavaliers"    ~ "Cleveland Cavaliers",
    Team == "Celtics"      ~ "Boston Celtics",
    Team == "Clippers"     ~ "Los Angeles Clippers",
    Team == "Grizzlies" & season <= 2001 ~ "Vancouver Grizzlies",
    Team == "Grizzlies" & season >= 2002 ~ "Memphis Grizzlies",
    Team == "Hawks"        ~ "Atlanta Hawks",
    Team == "Heat"         ~ "Miami Heat",
    Team == "Hornets" & season <= 2002          ~ "Charlotte Hornets",
    Team == "Hornets" & season %in% 2003:2005   ~ "New Orleans Hornets",
    Team == "Hornets" & season %in% 2006:2007   ~ "New Orleans/Oklahoma City Hornets",
    Team == "Hornets" & season %in% 2008:2013   ~ "New Orleans Hornets",
    Team == "Hornets" & season >= 2015          ~ "Charlotte Hornets",
    Team == "Jazz"         ~ "Utah Jazz",
    Team == "Kings"        ~ "Sacramento Kings",
    Team == "Knicks"       ~ "New York Knicks",
    Team == "Lakers"       ~ "Los Angeles Lakers",
    Team == "Magic"        ~ "Orlando Magic",
    Team == "Mavericks"    ~ "Dallas Mavericks",
    Team == "Nets" & season <= 2012 ~ "New Jersey Nets",
    Team == "Nets" & season >= 2013 ~ "Brooklyn Nets",
    Team == "Nuggets"      ~ "Denver Nuggets",
    Team == "Pacers"       ~ "Indiana Pacers",
    Team == "Pelicans"     ~ "New Orleans Pelicans",
    Team == "Pistons"      ~ "Detroit Pistons",
    Team == "Raptors"      ~ "Toronto Raptors",
    Team == "Rockets"      ~ "Houston Rockets",
    Team == "Sonics"       ~ "Seattle SuperSonics",
    Team == "Spurs"        ~ "San Antonio Spurs",
    Team == "Suns"         ~ "Phoenix Suns",
    Team == "Thunder"      ~ "Oklahoma City Thunder",
    Team == "Timberwolves" ~ "Minnesota Timberwolves",
    Team == "Warriors"     ~ "Golden State Warriors",
    Team == "Wizards"      ~ "Washington Wizards",
    TRUE ~ NA_character_
  ))

soft_tissue_counts <- soft_tissue_events %>%
  group_by(season, match_team) %>%
  summarise(n_soft_tissue = n(), .groups = "drop")

pace_soft_tissue <- soft_tissue_counts %>%
  left_join(team_season, by = c("season" = "season", "match_team" = "team")) %>%
  filter(!is.na(pace), !is.na(team_games_played), team_games_played > 0) %>%
  mutate(soft_tissue_rate = n_soft_tissue / team_games_played)

fe_model_soft_tissue <- lm(soft_tissue_rate ~ pace + factor(season), data = pace_soft_tissue)
summary(fe_model_soft_tissue)

# Read the "pace" row exactly as in Part J. If this comes back significant
# where the pooled model (Part J) did not, that supports the theory that
# pooling injury mechanisms washed out a real fatigue-specific signal.
# If it's still null, that's a second independent strike against the
# pace hypothesis using a mechanism-matched subset, not just the pooled one.

# ---- Part L: Is relinquishment count a good proxy for actual health? ------
# The classifier's 99.3% coverage measures classification accuracy, not
# whether "relinquished + injury-keyword note" is a good proxy for real
# health outcomes in the first place. A team can absorb a 3-week injury
# on the active roster in one era and via a complex two-way-contract
# shuffle in another, changing how often a transaction gets logged
# without changing how much health actually changed. games_missed
# (team games played minus player games played) is a proxy that doesn't
# depend on anyone filing a transaction at all - it's a direct
# consequence of a player just not being on the floor.
#
# Rebuild the games-missed table and run the identical pace test against
# it. Convergence between this and the relinquishment-based result would
# be strong evidence the null finding is real, not an artifact of a
# shaky dependent variable.
#
# player_totals$team is bbref-style ABBREVIATIONS (ATL, BOS, CHI...), not
# full names - confirmed by direct inspection, this is NOT the same
# nickname problem as Part D. team_season now carries team_summaries'
# own abbreviation column, which is already season-specific and already
# reflects relocations (CHH/CHA/CHO for Charlotte, NJN/BRK for Nets,
# VAN/MEM for Grizzlies, SEA/OKC for Thunder), so this join uses that
# column directly rather than needing a hand-built crosswalk.

player_totals <- read.csv("C:/Users/zainh/Downloads/Player Totals.csv", stringsAsFactors = FALSE)

player_totals <- player_totals %>% filter(lg == "NBA") %>%
  filter(!str_detect(team, "^\\dTM$"))

# Diagnostic FIRST, on the unfiltered join - checking is.na() AFTER
# already filtering NAs out (the earlier mistake here) always reads 0
# regardless of how badly the join actually went. Confirm the real
# match rate before trusting anything downstream.

games_gap_raw <- player_totals %>%
  left_join(team_season, by = c("season" = "season", "team" = "abbreviation"))

games_gap_raw %>%
  filter(season >= 1995, season <= 2023) %>%
  summarise(pct_unmatched = mean(is.na(pace)))   # should be near 0 - if not,
# print the unmatched abbreviations
# before going further

games_gap_raw %>%
  filter(season >= 1995, season <= 2023, is.na(pace)) %>%
  distinct(team) %>%
  print()

# Only proceed past this point once pct_unmatched is near 0.

games_gap <- games_gap_raw %>%
  mutate(games_missed = team_games_played - g) %>%
  filter(season >= 1995, season <= 2023, !is.na(pace), !is.na(team_games_played))

games_missed_team_season <- games_gap %>%
  group_by(season, team) %>%
  summarise(
    total_games_missed = sum(games_missed, na.rm = TRUE),
    team_games_played = first(team_games_played),
    pace = first(pace),
    .groups = "drop"
  ) %>%
  mutate(games_missed_rate = total_games_missed / team_games_played)

fe_model_games_missed <- lm(games_missed_rate ~ pace + factor(season), data = games_missed_team_season)
summary(fe_model_games_missed)

# Compare this model's "pace" coefficient and p-value directly against
# Part J's. Two different dependent variables (transaction-based
# injury_rate vs. attendance-based games_missed_rate), same conclusion,
# is a materially stronger claim than either result alone.

# Season-level trend comparison, as a plain visual companion:
season_compare <- season_level %>%
  left_join(
    games_missed_team_season %>%
      group_by(season) %>%
      summarise(avg_games_missed_rate = mean(games_missed_rate, na.rm = TRUE), .groups = "drop"),
    by = "season"
  )

ggplot(season_compare, aes(x = season)) +
  geom_line(aes(y = avg_injury_rate, color = "Relinquishment-based injury rate")) +
  geom_line(aes(y = avg_games_missed_rate / max(avg_games_missed_rate, na.rm = TRUE) *
                  max(avg_injury_rate, na.rm = TRUE), color = "Games-missed rate (rescaled)")) +
  labs(title = "Relinquishment-based injury rate vs. games-missed rate by season",
       subtitle = "Games-missed rescaled to the same axis range for shape comparison only - not a shared unit",
       x = "Season", y = "Rate", color = NULL)

# ---- Part M: Does "spatial pace" (3-point attempt rate) matter, even -----
#              though "possession pace" doesn't? ----------------------------
# Possessions-per-48 counts how many times the ball changes hands; it
# does not capture how far players move WITHIN a possession. A 1995
# half-court set with post entries and players spaced 15 feet apart is
# a different physical event than a 2018+ set with five shooters spaced
# behind the arc, closeouts now covering 25 feet instead of 10. True
# distance/speed data (Second Spectrum) isn't available before 2013 and
# isn't in either Kaggle source at all - but x3p_ar (three-point attempt
# rate), already present in Team Summaries.csv, is a reasonable proxy
# for floor spacing and IS available for the full analysis window.
# This also lines up with the timing of the post-2018 injury-rate shift
# documented earlier (Part J/Secondary Finding), which coincides with
# the league-wide 3PAr acceleration - worth testing directly rather
# than left as a coincidence.

pace_injury_spacing <- injury_counts %>%
  left_join(team_season, by = c("season" = "season", "match_team" = "team")) %>%
  filter(!is.na(pace), !is.na(x3p_ar), !is.na(team_games_played), team_games_played > 0) %>%
  mutate(injury_rate = n_injuries / team_games_played)

# Two models: x3p_ar alone (does spacing matter on its own?) and
# x3p_ar alongside pace (does spacing matter ABOVE AND BEYOND
# possession count, once both are in the same model?). The second is
# the more informative one - if x3p_ar is significant even after pace
# is already controlled for, that's real evidence spacing is doing
# something possession-count pace was never going to capture.

fe_model_spacing_only <- lm(injury_rate ~ x3p_ar + factor(season), data = pace_injury_spacing)
summary(fe_model_spacing_only)

fe_model_pace_and_spacing <- lm(injury_rate ~ pace + x3p_ar + factor(season), data = pace_injury_spacing)
summary(fe_model_pace_and_spacing)

# Read the x3p_ar row in both models. A significant x3p_ar coefficient,
# especially in the second model where pace is already accounted for,
# would mean the original hypothesis was aimed at the wrong variable -
# "pace" as commonly discussed conflates possession count with floor
# spacing, and only one of the two may actually matter.

# ---- Part N: Load management as a suppressor - a SUGGESTIVE check only ---
# IMPORTANT CAVEAT before running this: this does NOT resolve the
# endogeneity concern, and no regression on this data can. If teams
# respond to high pace by proactively resting players, pace affects
# rest, and rest affects injury - a simple regression can't cleanly
# separate "pace has no effect" from "pace's effect is fully offset by
# an intervention triggered by pace itself." Properly identifying that
# needs something like an instrumental variable or a natural experiment
# (e.g. a rule change that shifted pace for reasons unrelated to injury
# risk) - neither exists in this dataset. What follows is a plausibility
# check, not a causal test: does average rotation-player minutes trend
# downward more sharply on higher-pace teams? A flat or non-existent
# trend would weaken the suppressor story. A strong one is CONSISTENT
# with it but does not prove it - do not overstate this result either
# direction in the write-up.
#
# Uses player_totals (already loaded in Part L). Column names not yet
# confirmed - run str(player_totals) first and adjust `mp` below if the
# actual minutes-played column is named differently.

str(player_totals)   # CONFIRM the minutes-played column name before running the rest of Part N

rotation_minutes <- player_totals %>%
  filter(season >= 1995, season <= 2023, g >= 10) %>%   # drop cameo/call-up appearances
  group_by(season, team) %>%
  summarise(avg_rotation_minutes = mean(mp / g, na.rm = TRUE), .groups = "drop")   # mp/g = minutes PER GAME

pace_minutes_check <- team_season %>%
  select(season, team = abbreviation, pace) %>%
  inner_join(rotation_minutes, by = c("season", "team"))

fe_model_minutes_trend <- lm(avg_rotation_minutes ~ pace + factor(season), data = pace_minutes_check)
summary(fe_model_minutes_trend)

# Read the "pace" coefficient here as: for a given season, do
# higher-pace teams play their rotation players fewer minutes per game?
# A negative, significant coefficient is consistent with (not proof of)
# a load-management response to pace. Report it that way in the write-up,
# as suggestive context for the null pace-injury finding, not as a
# resolved mechanism.

# ---- Part O: Does the spacing (x3p_ar) effect concentrate in soft- --------
#              tissue/overuse injuries specifically? -----------------------
# x3p_ar survived both robustness checks Part M got (COVID-exclusion,
# de-trended correlation) - see run notes. The proposed mechanism is
# closeout deceleration and greater distance covered per possession,
# which is a fatigue/overuse story, not a blunt-trauma one. If that
# mechanism is real, the x3p_ar effect should hold up - ideally get
# STRONGER, not weaker - when restricted to the same soft-tissue subset
# built in Part K, rather than the pooled specific_injury category
# which also includes unrelated contact injuries.

pace_soft_tissue_spacing <- soft_tissue_counts %>%
  left_join(team_season, by = c("season" = "season", "match_team" = "team")) %>%
  filter(!is.na(pace), !is.na(x3p_ar), !is.na(team_games_played), team_games_played > 0) %>%
  mutate(soft_tissue_rate = n_soft_tissue / team_games_played)

fe_model_soft_tissue_spacing <- lm(soft_tissue_rate ~ pace + x3p_ar + factor(season),
                                   data = pace_soft_tissue_spacing)
summary(fe_model_soft_tissue_spacing)

# De-trended correlation, same discipline as Part M's second check -
# don't skip this just because the regression above might look good.
pace_soft_tissue_spacing_detrended <- pace_soft_tissue_spacing %>%
  group_by(season) %>%
  mutate(
    x3p_ar_vs_season_avg = x3p_ar - mean(x3p_ar, na.rm = TRUE),
    soft_tissue_rate_vs_season_avg = soft_tissue_rate - mean(soft_tissue_rate, na.rm = TRUE)
  ) %>%
  ungroup()

cor.test(pace_soft_tissue_spacing_detrended$x3p_ar_vs_season_avg,
         pace_soft_tissue_spacing_detrended$soft_tissue_rate_vs_season_avg)

# Read the x3p_ar coefficient/correlation here against Part M's pooled
# result. Stronger or comparable = supports the closeout-deceleration
# mechanism specifically. Weaker or null = the pooled result may be
# driven by injury types the mechanism doesn't actually predict, which
# would call for more caution in how the finding gets framed.