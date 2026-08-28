library(fixest)
library(dplyr)

df <- read.csv("CA_Merged_Data_FEB_3.csv")

# Ensure correct types
df <- df |>
  mutate(
    Election_Year       = as.integer(Election_Year),
    Decarceratory       = as.integer(Decarceratory),
    Contested           = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County              = as.factor(County.x),
    Quarter             = as.factor(Quarter)
  )

# Subsamples
all_elections    <- df
contested        <- df |> filter(Contested == 1)
incumbent_sought <- df |> filter(Did_Incumbent_Seek_Reelection == 1)

# Add numeric time trend variable
all_elections    <- all_elections    |> mutate(time = as.integer(factor(Quarter)))
contested        <- contested        |> mutate(time = as.integer(factor(Quarter)))
incumbent_sought <- incumbent_sought |> mutate(time = as.integer(factor(Quarter)))

# Baseline models: County + Quarter FEs
spec <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter

m1 <- feols(spec, data = all_elections,    cluster = ~County.x)
m2 <- feols(spec, data = contested,        cluster = ~County.x)
m3 <- feols(spec, data = incumbent_sought, cluster = ~County.x)

# Robustness: add county-specific linear time trends
spec_trend <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter + County[time]

m1_trend <- feols(spec_trend, data = all_elections,    cluster = ~County.x)
m2_trend <- feols(spec_trend, data = contested,        cluster = ~County.x)
m3_trend <- feols(spec_trend, data = incumbent_sought, cluster = ~County.x)

# Baseline results
cat("\n=== Baseline: All Elections ===\n");          print(summary(m1))
cat("\n=== Baseline: Contested Elections ===\n");    print(summary(m2))
cat("\n=== Baseline: Incumbent Sought Reelection ===\n"); print(summary(m3))

# Robustness results
cat("\n=== With County Trends: All Elections ===\n");          print(summary(m1_trend))
cat("\n=== With County Trends: Contested Elections ===\n");    print(summary(m2_trend))
cat("\n=== With County Trends: Incumbent Sought Reelection ===\n"); print(summary(m3_trend))

# Side-by-side: baseline vs. with trends
cat("\n=== Baseline Table ===\n")
etable(m1, m2, m3,
       headers = c("All", "Contested", "Incumbent Sought"),
       keep = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))

cat("\n=== Robustness Table (County Time Trends) ===\n")
etable(m1_trend, m2_trend, m3_trend,
       headers = c("All", "Contested", "Incumbent Sought"),
       keep = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))
