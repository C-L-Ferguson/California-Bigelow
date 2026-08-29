library(fixest)
library(dplyr)

# Load data
df <- read.csv("https://raw.githubusercontent.com/C-L-Ferguson/California-Bigelow/claude/dataset-review-l0rluy/CA_Merged_Data_FEB_3.csv")

# Coerce variable types; County.x is the county identifier in this merged dataset
df <- df |>
  mutate(
    Election_Year                 = as.integer(Election_Year),
    Decarceratory                 = as.integer(Decarceratory),
    Contested                     = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County                        = as.factor(County.x),
    Quarter                       = as.factor(Quarter),
    time                          = as.numeric(X)  # numeric index for county time trends
  )

# --- Subsamples ---
# All elections: full sample
all_elections    <- df
# Contested: only elections where incumbent faced opposition
contested        <- df |> filter(Contested == 1)
# Incumbent sought: only elections where incumbent ran for reelection (cleanest test of personal electoral incentive)
incumbent_sought <- df |> filter(Did_Incumbent_Seek_Reelection == 1)
# Incumbent sought + contested: sharpest test — incumbent personally on ballot AND facing a challenger
incumbent_contested <- df |> filter(Did_Incumbent_Seek_Reelection == 1, Contested == 1)

# =============================================================================
# PRIMARY OUTCOME: Percentage_Prison
# Key finding: negative interaction = decarceratory DAs become LESS punitive
# near elections, contrary to the convergence hypothesis
# =============================================================================

spec       <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter
spec_trend <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter + County[time]

m1 <- feols(spec, data = all_elections,      cluster = ~County.x)
m2 <- feols(spec, data = contested,          cluster = ~County.x)
m3 <- feols(spec, data = incumbent_sought,   cluster = ~County.x)
m4 <- feols(spec, data = incumbent_contested, cluster = ~County.x)

m1_trend <- feols(spec_trend, data = all_elections,      cluster = ~County.x)
m2_trend <- feols(spec_trend, data = contested,          cluster = ~County.x)
m3_trend <- feols(spec_trend, data = incumbent_sought,   cluster = ~County.x)
m4_trend <- feols(spec_trend, data = incumbent_contested, cluster = ~County.x)

cat("\n=== PRIMARY OUTCOME: Percentage_Prison ===\n")
cat("Baseline:\n")
etable(m1, m2, m3, m4,
       headers = c("All Elections", "Contested", "Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

cat("\nWith County Time Trends:\n")
etable(m1_trend, m2_trend, m3_trend, m4_trend,
       headers = c("All Elections", "Contested", "Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

# --- Export regression tables ---
etable(m1, m2, m3, m4,
       headers   = c("All Elections", "Contested", "Incumbent Sought", "Incumbent + Contested"),
       keep      = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
       depvar    = TRUE,
       file      = "table1_prison_baseline.tex")

etable(m1_trend, m2_trend, m3_trend, m4_trend,
       headers   = c("All Elections", "Contested", "Incumbent Sought", "Incumbent + Contested"),
       keep      = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
       depvar    = TRUE,
       file      = "table1_prison_trends.tex")

cat("Tables exported: table1_prison_baseline.tex, table1_prison_trends.tex\n")

# =============================================================================
# ADDITIONAL OUTCOMES
# Prison and probation results mirror each other: decarceratory DAs shift
# sentences from prison toward probation near elections — leaning into their
# platform rather than converging toward punitiveness.
# Straight sentences show weak/inconsistent effects; split sentences show none.
# =============================================================================

outcomes <- c("Percentage_Probation", "Percentage_Straight", "Percentage_Split")

for (outcome in outcomes) {
  spec_o       <- as.formula(paste(outcome, "~ Election_Year * Decarceratory | County + Quarter"))
  spec_o_trend <- as.formula(paste(outcome, "~ Election_Year * Decarceratory | County + Quarter + County[time]"))

  ma   <- feols(spec_o,       data = all_elections,    cluster = ~County.x)
  mb   <- feols(spec_o,       data = contested,        cluster = ~County.x)
  mc   <- feols(spec_o,       data = incumbent_sought, cluster = ~County.x)
  ma_t <- feols(spec_o_trend, data = all_elections,    cluster = ~County.x)
  mb_t <- feols(spec_o_trend, data = contested,        cluster = ~County.x)
  mc_t <- feols(spec_o_trend, data = incumbent_sought, cluster = ~County.x)

  cat("\n=== OUTCOME:", outcome, "===\n")
  cat("Baseline:\n")
  etable(ma, mb, mc,
         headers = c("All Elections", "Contested", "Incumbent Sought"),
         keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

  cat("\nWith County Time Trends:\n")
  etable(ma_t, mb_t, mc_t,
         headers = c("All Elections", "Contested", "Incumbent Sought"),
         keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

  slug <- tolower(sub("Percentage_", "", outcome))
  etable(ma, mb, mc,
         headers = c("All Elections", "Contested", "Incumbent Sought"),
         keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
         depvar  = TRUE,
         file    = paste0("table2_", slug, "_baseline.tex"))
  etable(ma_t, mb_t, mc_t,
         headers = c("All Elections", "Contested", "Incumbent Sought"),
         keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
         depvar  = TRUE,
         file    = paste0("table2_", slug, "_trends.tex"))
  cat("Tables exported for", outcome, "\n")
}

# =============================================================================
# SENSITIVITY CHECK: LA 2020 COVID overlap
# LA had an election in 2020 Q3/Q4, coinciding with COVID court disruptions.
# Excluding those two observations does not meaningfully change results.
# =============================================================================

cat("\n=== SENSITIVITY: Excluding LA 2020 Election Quarters (COVID check) ===\n")
no_la_covid <- all_elections |>
  filter(!(County.x == "Los Angeles" & Quarter %in% c(" 2020 Q3 Court", " 2020 Q4 Court")))

m1_nola <- feols(Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter,
                 data = no_la_covid, cluster = ~County.x)

etable(m1, m1_nola,
       headers = c("Full Sample", "Excl. LA 2020"),
       keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

etable(m1, m1_nola,
       headers = c("Full Sample", "Excl. LA 2020"),
       keep    = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
       depvar  = TRUE,
       file    = "table3_covid_sensitivity.tex")
cat("Table exported: table3_covid_sensitivity.tex\n")

# =============================================================================
# PLACEBO TEST: Fake election years (t-2 shift)
# Assigns "election year" status to quarters two years before the actual
# election year. If the regression finds a similar interaction in fake election
# years, the result may reflect pre-existing trends rather than electoral behavior.
# A null placebo result strengthens the causal interpretation.
# =============================================================================

cat("\n=== PLACEBO TEST: Fake Election Years (shifted 2 years earlier) ===\n")

# Get the real election years per county
real_elections <- all_elections |>
  filter(Election_Year == 1) |>
  mutate(
    year = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter))
  ) |>
  select(County.x, year) |>
  distinct() |>
  mutate(placebo_year = year - 2)  # shift back 2 years

# Build placebo Election_Year flag
placebo_df <- all_elections |>
  mutate(year = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter)),
         qnum = as.integer(sub(".*Q(\\d).*", "\\1", Quarter))) |>
  left_join(real_elections |> select(County.x, placebo_year), by = "County.x") |>
  mutate(
    Placebo_Election = as.integer(!is.na(placebo_year) & year == placebo_year & qnum %in% c(3, 4))
  ) |>
  select(-year, -qnum, -placebo_year)

m_placebo <- feols(Percentage_Prison ~ Placebo_Election * Decarceratory | County + Quarter,
                   data = placebo_df, cluster = ~County.x)

cat("Placebo (fake election years, t-2):\n")
etable(m1, m_placebo,
       headers = c("Real Election Years", "Placebo (t-2)"),
       keep    = c("Election_Year", "Placebo_Election", "Decarceratory",
                   "Election_Year:Decarceratory", "Placebo_Election:Decarceratory"))

etable(m1, m_placebo,
       headers = c("Real Election Years", "Placebo (t-2)"),
       keep    = c("Election_Year", "Placebo_Election", "Decarceratory",
                   "Election_Year:Decarceratory", "Placebo_Election:Decarceratory"),
       depvar  = TRUE,
       file    = "table4_placebo.tex")
cat("Table exported: table4_placebo.tex\n")
cat("Interpretation: If placebo interaction is near zero and insignificant,\n")
cat("the real election effect is not driven by pre-existing trends.\n")
