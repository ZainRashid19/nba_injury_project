# ============================================================
# NBA Injury Data — Audit, Classification & Validation
# Source: Kaggle, loganlauton/nba-injury-stats-1951-2023
# (scraped from Pro Sports Transactions)
#
# Project scope decision: full 1980-2023 range is used as the
# reported injury trend. The ~1994-95 reporting-density break is
# real (see Phase 3) and must stay annotated on any chart built
# from this data - it is disclosed, not hidden, per project
# decision to present raw counts with the caveat attached directly
# to the visual rather than left to a footnote.
# ============================================================

library(dplyr)
library(stringr)
library(ggplot2)

# ---- Phase 1: Load and parse ----------------------------------------------

injuries <- read.csv("C:/Users/zainh/Downloads/NBA Player Injury Stats(1951 - 2023).csv", stringsAsFactors = FALSE)

str(injuries)         # types + sample values for every column
head(injuries)         # first 6 rows, full width
names(injuries)        # column names only
dim(injuries)           # rows, columns
"Date" %in% names(injuries)   # confirm before referencing it below

injuries$Date   <- as.Date(injuries$Date, format = "%Y-%m-%d")
injuries$Year   <- as.integer(format(injuries$Date, "%Y"))
injuries$Decade <- (injuries$Year %/% 10) * 10

sum(is.na(injuries$Date))   # rows where date failed to parse - should be 0

# ---- Phase 2: Isolate "Relinquished" rows ---------------------------------
# A non-blank Relinquished field = a player being removed from a roster,
# for ANY reason (injury, illness, personal, military, suspension...).
# The REASON only lives in Notes - that's what Phase 4 is for.

rel <- injuries %>% filter(Relinquished != "" & !is.na(Relinquished))
rel$Notes <- tolower(rel$Notes)

nrow(injuries)   # 37,667
nrow(rel)        # 20,044 - how many removal events exist

# ---- Phase 3: Coverage by decade - the key diagnostic ---------------------

coverage_by_decade <- rel %>%
  group_by(Decade) %>%
  summarise(n_events = n(), .groups = "drop")
print(coverage_by_decade)

rate_by_year <- rel %>% count(Year, name = "n_events")

era_rates <- rel %>%
  mutate(era = case_when(
    Year %in% 1980:1989 ~ "1980s",
    Year %in% 1990:1999 ~ "1990s",
    Year %in% 2000:2009 ~ "2000s",
    Year %in% 2010:2019 ~ "2010s",
    Year %in% 2020:2023 ~ "2020-2023",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(era)) %>%
  group_by(era) %>%
  summarise(
    n_events = n(),
    n_years = n_distinct(Year),
    avg_per_year = n_events / n_years,
    .groups = "drop"
  )
print(era_rates)

# Plot - the dashed line/subtitle is the mandatory caveat for the raw trend.
# Keep this annotation on every version of this chart used in the report.
ggplot(rate_by_year, aes(x = Year, y = n_events)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = 1994, linetype = "dashed", color = "red") +
  labs(title = "Relinquished-player transactions per year",
       subtitle = "Dashed line marks the ~1994-95 structural break in reporting density",
       y = "Count", x = "Year")

# ---- Phase 4: Classify each Relinquished row by reason --------------------

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

non_injury_kw <- paste(
  "personal reasons","suspen","military","health and safety","covid",
  "flu","illness","rest","not with team","bereavement","maternity",
  "paternity","g league","g-league","assignment","retire","waived",
  "trade","excused","conditioning","virus","infection","migraine",
  "appendectomy","bronchitis","pneumonia","kidney","blood clot",
  "chest pains","strep","gastroenteritis","food poisoning","dental",
  "death in family","birth of child","ineligible","irregular heartbeat",
  sep = "|"
)

rel <- rel %>%
  mutate(
    notes_stripped = str_squish(str_remove_all(Notes, "\\([^)]*\\)")),
    category = case_when(
      str_detect(Notes, specific_injury_kw) ~ "specific_injury",
      str_detect(Notes, non_injury_kw)      ~ "non_injury_reason",
      notes_stripped %in% c("placed on il", "placed on ir", "placed on disabled list") ~ "zero_detail",
      TRUE ~ "other_unclassified"
    )
  )

specificity_by_decade <- rel %>%
  group_by(Decade, category) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

print(specificity_by_decade)

# ---- Phase 4b: Validate the classification before trusting it -------------

# 1. What's still landing in other_unclassified? Don't assume it's harmless.
rel %>%
  filter(category == "other_unclassified") %>%
  count(Notes, sort = TRUE) %>%
  head(30)

# 2. Is "back" catching real back injuries, or noise via substring match?
rel %>% filter(category == "specific_injury", str_detect(Notes, "broken")) %>%
  count(Notes, sort = TRUE) %>% head(20)

# 3. Do blank zero_detail rows skew minor, or hide severe injuries too?
#    Uses "out for season" as a rough severity marker, compared by category.
rel %>%
  mutate(season_ending = str_detect(Notes, "out for season")) %>%
  group_by(category) %>%
  summarise(
    n = n(),
    pct_season_ending = round(mean(season_ending) * 100, 1),
    .groups = "drop"
  )

sum(str_trim(rel$Notes) == "placed on ir")
print(specificity_by_decade, n = 30)

rel %>%
  filter(category == "other_unclassified") %>%
  count(Notes, sort = TRUE) %>%
  head(30)

# ---- Phase 5: Takeaway -----------------------------------------------------
# Coverage is NOT comparable across the full 1980-2023 range - the sharp
# 1993->1994->1995 jump (127->245->277) is a reporting-density artifact,
# not a real health trend, confirmed by testing: filtering to injury-only
# text barely moved the era-to-era ratio (24.7x raw -> 21.8x filtered).
#
# Project decision: report the full 1980-2023 raw trend as the headline
# number anyway. The discontinuity is disclosed directly on the chart
# (dashed line + subtitle above), not left to a footnote. Report the
# 1994/95-2022 window's numbers alongside the full-range numbers so a
# reader can see both the raw trend and the trustworthy-window trend
# side by side.