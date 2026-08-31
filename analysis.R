library(fixest)
library(dplyr)

# Load data
df <- read.csv("https://raw.githubusercontent.com/C-L-Ferguson/California-Bigelow/claude/dataset-review-l0rluy/CA_Merged_Data_2024.csv")

# Coerce variable types; County.x is the county identifier in this merged dataset
df <- df |>
  mutate(
    Election_Year                 = as.integer(Election_Year),
    Decarceratory                 = as.integer(Decarceratory),
    Contested                     = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County                        = as.factor(County.x),
    Quarter                       = as.factor(Quarter),
    time                          = as.numeric(X),
    Recall                        = as.integer(ifelse(is.na(Recall.) | Recall. == "", 0, as.integer(Recall.)))
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

# --- Full-year coding: Election_Year_Full = 1 for ALL four quarters of election year ---
# Expands treatment window beyond Q3/Q4; tests whether effect is present throughout election year
df <- df |>
  mutate(
    year_str = sub(".*(\\d{4}).*", "\\1", Quarter),
    Election_Year_Full = as.integer(
      paste0(County.x, "_", year_str) %in%
        (df |>
           filter(Election_Year == 1) |>
           mutate(year_str = sub(".*(\\d{4}).*", "\\1", Quarter)) |>
           mutate(key = paste0(County.x, "_", year_str)) |>
           pull(key) |>
           unique()
        )
    )
  )

incumbent_sought_full    <- df |> filter(Did_Incumbent_Seek_Reelection == 1)
incumbent_contested_full <- df |> filter(Did_Incumbent_Seek_Reelection == 1, Contested == 1)

# =============================================================================
# PRIMARY OUTCOME: Percentage_Prison
# Primary spec uses full-year coding (all 4 quarters of election year = 1).
# Effect runs throughout the election year, not just Q3/Q4.
# Key finding: -8.033** (Incumbent + Contested), -3.497* (Incumbent Sought)
# =============================================================================

# Primary models: full-year coding
m3 <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
            data = incumbent_sought_full,    cluster = ~County.x)
m4 <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
            data = incumbent_contested_full, cluster = ~County.x)

m3_trend <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter + County[time],
                  data = incumbent_sought_full,    cluster = ~County.x)
m4_trend <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter + County[time],
                  data = incumbent_contested_full, cluster = ~County.x)

cat("\n=== PRIMARY OUTCOME: Percentage_Prison (full-year coding) ===\n")
cat("Baseline:\n")
print(etable(m3, m4,
       headers = c("Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory")))

cat("\nWith County Time Trends:\n")
print(etable(m3_trend, m4_trend,
       headers = c("Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory")))

etable(m3, m4,
       headers   = c("Incumbent Sought", "Incumbent + Contested"),
       keep      = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"),
       depvar    = TRUE,
       file      = "table1_prison_baseline.tex")

etable(m3_trend, m4_trend,
       headers   = c("Incumbent Sought", "Incumbent + Contested"),
       keep      = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"),
       depvar    = TRUE,
       file      = "table1_prison_trends.tex")

cat("Tables exported: table1_prison_baseline.tex, table1_prison_trends.tex\n")

# =============================================================================
# ROBUSTNESS: Q3/Q4-only coding (original specification)
# Confirms effect is present even with narrower treatment window.
# =============================================================================

cat("\n=== ROBUSTNESS: Q3/Q4-only coding vs. full-year primary spec ===\n")

m3_q34 <- feols(Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter,
                data = incumbent_sought,    cluster = ~County.x)
m4_q34 <- feols(Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter,
                data = incumbent_contested, cluster = ~County.x)

print(etable(m4, m4_q34,
       headers = c("Full Year (Primary)", "Q3/Q4 Only (Robustness)"),
       keep    = c("Election_Year_Full", "Election_Year", "Decarceratory",
                   "Election_Year_Full:Decarceratory", "Election_Year:Decarceratory")))

# =============================================================================
# ADDITIONAL OUTCOMES
# Prison and probation results mirror each other: decarceratory DAs shift
# sentences from prison toward probation near elections — leaning into their
# platform rather than converging toward punitiveness.
# Straight sentences show weak/inconsistent effects; split sentences show none.
# =============================================================================

outcomes <- c("Percentage_Probation", "Percentage_Straight", "Percentage_Split")

for (outcome in outcomes) {
  spec_o       <- as.formula(paste(outcome, "~ Election_Year_Full * Decarceratory | County + Quarter"))
  spec_o_trend <- as.formula(paste(outcome, "~ Election_Year_Full * Decarceratory | County + Quarter + County[time]"))

  mc   <- feols(spec_o,       data = incumbent_sought_full,    cluster = ~County.x)
  md   <- feols(spec_o,       data = incumbent_contested_full, cluster = ~County.x)
  mc_t <- feols(spec_o_trend, data = incumbent_sought_full,    cluster = ~County.x)
  md_t <- feols(spec_o_trend, data = incumbent_contested_full, cluster = ~County.x)

  cat("\n=== OUTCOME:", outcome, "===\n")
  cat("Baseline:\n")
  etable(mc, md,
         headers = c("Incumbent Sought", "Incumbent + Contested"),
         keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"))

  cat("\nWith County Time Trends:\n")
  etable(mc_t, md_t,
         headers = c("Incumbent Sought", "Incumbent + Contested"),
         keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"))

  slug <- tolower(sub("Percentage_", "", outcome))
  etable(mc, md,
         headers = c("Incumbent Sought", "Incumbent + Contested"),
         keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"),
         depvar  = TRUE,
         file    = paste0("table2_", slug, "_baseline.tex"))
  etable(mc_t, md_t,
         headers = c("Incumbent Sought", "Incumbent + Contested"),
         keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"),
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
no_la_covid <- incumbent_sought_full |>
  filter(!(County.x == "Los Angeles" & Quarter %in% c("2020 Q3 Court", "2020 Q4 Court")))

m3_nola <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
                 data = no_la_covid, cluster = ~County.x)

print(etable(m3, m3_nola,
       headers = c("Incumbent Sought", "Excl. LA 2020"),
       keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory")))

etable(m3, m3_nola,
       headers = c("Incumbent Sought", "Excl. LA 2020"),
       keep    = c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory"),
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

# Build placebo Election_Year_Full flag: all 4 quarters of placebo year
placebo_df <- df |>
  mutate(year = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter))) |>
  left_join(real_elections |> select(County.x, placebo_year), by = "County.x") |>
  mutate(
    Placebo_Election = as.integer(!is.na(placebo_year) & year == placebo_year)
  ) |>
  select(-year, -placebo_year) |>
  mutate(
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    Contested = as.integer(Contested),
    County    = as.factor(County.x),
    Quarter   = as.factor(Quarter)
  )

placebo_incumbent_sought    <- placebo_df |> filter(Did_Incumbent_Seek_Reelection == 1)
placebo_incumbent_contested <- placebo_df |> filter(Did_Incumbent_Seek_Reelection == 1, Contested == 1)

m_placebo   <- feols(Percentage_Prison ~ Placebo_Election * Decarceratory | County + Quarter,
                     data = placebo_incumbent_sought,    cluster = ~County.x)
m_placebo_c <- feols(Percentage_Prison ~ Placebo_Election * Decarceratory | County + Quarter,
                     data = placebo_incumbent_contested, cluster = ~County.x)

cat("Placebo (fake election years, t-2):\n")
print(etable(m3, m4, m_placebo, m_placebo_c,
       headers = c("Real (Sought)", "Real (Contested)", "Placebo (Sought)", "Placebo (Contested)"),
       keep    = c("Election_Year_Full", "Placebo_Election", "Decarceratory",
                   "Election_Year_Full:Decarceratory", "Placebo_Election:Decarceratory")))

etable(m4, m_placebo_c,
       headers = c("Real Election Years", "Placebo (t-2)"),
       keep    = c("Election_Year_Full", "Placebo_Election", "Decarceratory",
                   "Election_Year_Full:Decarceratory", "Placebo_Election:Decarceratory"),
       depvar  = TRUE,
       file    = "table4_placebo.tex")
cat("Table exported: table4_placebo.tex\n")
cat("Interpretation: If placebo interaction is near zero and insignificant,\n")
cat("the real election effect is not driven by pre-existing trends.\n")

# =============================================================================
# POST-ELECTION TEST: Do decarceratory DAs revert after the election?
# Flags Q1/Q2 of the year immediately after each election as Post_Election.
# If the interaction is positive (or null), prison rates bounce back —
# consistent with electoral strategy rather than genuine commitment.
# If negative, the effect persists — consistent with platform reinforcement.
# =============================================================================

cat("\n=== POST-ELECTION TEST: Reversion after election? ===\n")

# Get election years per county (Q3 of election year = rel_q 0)
election_years_by_county <- all_elections |>
  filter(Election_Year == 1) |>
  mutate(year = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter))) |>
  select(County.x, year) |>
  distinct() |>
  mutate(post_year = year + 1)

# Flag Q1/Q2 of the year after each election as Post_Election
post_df <- all_elections |>
  mutate(
    year = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter)),
    qnum = as.integer(sub(".*Q(\\d).*", "\\1", Quarter))
  ) |>
  left_join(election_years_by_county |> select(County.x, post_year),
            by = "County.x") |>
  mutate(
    Post_Election = as.integer(!is.na(post_year) & year == post_year & qnum %in% c(1, 2))
  ) |>
  select(-year, -qnum, -post_year) |>
  mutate(
    Election_Year = as.integer(Election_Year),
    Decarceratory = as.integer(Decarceratory),
    Contested     = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County        = as.factor(County.x),
    Quarter       = as.factor(Quarter)
  )

post_incumbent_sought    <- post_df |> filter(Did_Incumbent_Seek_Reelection == 1)
post_incumbent_contested <- post_df |> filter(Did_Incumbent_Seek_Reelection == 1, Contested == 1)

# Model A: Election_Year effect (replicates m3/m4 in this data frame)
# Model B: Post_Election effect
# Model C: Both simultaneously — compares in-election vs. post-election within same model

m_post_sought    <- feols(Percentage_Prison ~ Post_Election * Decarceratory | County + Quarter,
                          data = post_incumbent_sought,    cluster = ~County.x)
m_post_contested <- feols(Percentage_Prison ~ Post_Election * Decarceratory | County + Quarter,
                          data = post_incumbent_contested, cluster = ~County.x)

# Combined model: election AND post-election in same regression
m_combined_sought    <- feols(Percentage_Prison ~ Election_Year * Decarceratory +
                                                  Post_Election * Decarceratory | County + Quarter,
                              data = post_incumbent_sought,    cluster = ~County.x)
m_combined_contested <- feols(Percentage_Prison ~ Election_Year * Decarceratory +
                                                  Post_Election * Decarceratory | County + Quarter,
                              data = post_incumbent_contested, cluster = ~County.x)

cat("Post-election only (does the effect persist after the election?):\n")
print(etable(m_post_sought, m_post_contested,
       headers = c("Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Post_Election", "Decarceratory", "Post_Election:Decarceratory")))

cat("\nCombined: Election + Post-Election in same model:\n")
print(etable(m_combined_sought, m_combined_contested,
       headers = c("Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year", "Post_Election", "Decarceratory",
                   "Election_Year:Decarceratory", "Post_Election:Decarceratory")))

etable(m_combined_sought, m_combined_contested,
       headers = c("Incumbent Sought", "Incumbent + Contested"),
       keep    = c("Election_Year", "Post_Election", "Decarceratory",
                   "Election_Year:Decarceratory", "Post_Election:Decarceratory"),
       depvar  = TRUE,
       file    = "table5_post_election.tex")
cat("Table exported: table5_post_election.tex\n")
cat("Interpretation: Negative Election_Year interaction + null/positive Post_Election interaction\n")
cat("= effect is temporary (electoral strategy). Persistent negative = genuine commitment.\n")

# =============================================================================
# LEAVE-ONE-OUT CHECK: Is the result driven by a single county?
# Re-runs m4 (Incumbent + Contested) dropping one county at a time.
# If the interaction survives dropping any single county, the result is not
# a one-county story. If it collapses when dropping LA or SF, flag that.
# =============================================================================

cat("\n=== LEAVE-ONE-OUT CHECK: Primary result (Incumbent + Contested) ===\n")

# First: show exactly which county-quarters are identifying the key cell
cat("Key identifying observations (Decarceratory=1, Election_Year=1, Incumbent+Contested):\n")
print(
  incumbent_contested |>
    filter(Election_Year == 1, Decarceratory == 1) |>
    select(County.x, Quarter, Percentage_Prison) |>
    arrange(County.x, Quarter)
)

# Leave-one-out: drop each county and re-run m4
counties_in_sample <- unique(incumbent_contested$County.x)

loo_results <- lapply(counties_in_sample, function(co) {
  dat <- incumbent_contested |> filter(County.x != co)
  tryCatch({
    fit <- feols(Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter,
                 data = dat, cluster = ~County.x)
    data.frame(
      Dropped    = co,
      N          = nrow(dat),
      coef       = coef(fit)["Election_Year:Decarceratory"],
      se         = se(fit)["Election_Year:Decarceratory"],
      pval       = pvalue(fit)["Election_Year:Decarceratory"]
    )
  }, error = function(e) {
    data.frame(Dropped = co, N = nrow(dat), coef = NA, se = NA, pval = NA)
  })
})

loo_df <- do.call(rbind, loo_results) |>
  mutate(
    sig   = ifelse(!is.na(pval) & pval < 0.05, "*",
            ifelse(!is.na(pval) & pval < 0.10, ".", "")),
    coef  = round(coef, 3),
    se    = round(se,   3),
    pval  = round(pval, 3)
  ) |>
  arrange(coef)

cat("\nLeave-one-out results (sorted by coefficient):\n")
print(loo_df, row.names = FALSE)
cat("\nFull sample (m4): coef =", round(coef(m4)["Election_Year:Decarceratory"], 3),
    " se =", round(se(m4)["Election_Year:Decarceratory"], 3), "\n")
cat("If all leave-one-out coefficients are negative, the result is not county-specific.\n")
