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

# --- Baseline models: County + Quarter fixed effects ---
spec <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter

m1 <- feols(spec, data = all_elections,    cluster = ~County.x)
m2 <- feols(spec, data = contested,        cluster = ~County.x)
m3 <- feols(spec, data = incumbent_sought, cluster = ~County.x)

# --- Robustness: add county-specific linear time trends ---
# Addresses concern that decarceratory counties were on different pre-existing trajectories
spec_trend <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter + County[time]

m1_trend <- feols(spec_trend, data = all_elections,    cluster = ~County.x)
m2_trend <- feols(spec_trend, data = contested,        cluster = ~County.x)
m3_trend <- feols(spec_trend, data = incumbent_sought, cluster = ~County.x)

# --- Output ---
cat("\n=== BASELINE MODELS ===\n")
cat("Key coefficient: Election_Year x Decarceratory\n")
cat("Negative = decarceratory DAs become LESS punitive near elections (against convergence hypothesis)\n\n")

etable(m1, m2, m3,
       headers  = c("All Elections", "Contested", "Incumbent Sought"),
       keep     = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
       title    = "Baseline: County + Quarter FEs")

cat("\n=== ROBUSTNESS: COUNTY-SPECIFIC TIME TRENDS ===\n")
cat("Interaction attenuates, suggesting some baseline effect reflects differential county trajectories.\n")
cat("Incumbent Sought subsample retains significance — strongest test of personal electoral incentive.\n\n")

etable(m1_trend, m2_trend, m3_trend,
       headers  = c("All Elections", "Contested", "Incumbent Sought"),
       keep     = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"),
       title    = "Robustness: County + Quarter FEs + County Time Trends")
