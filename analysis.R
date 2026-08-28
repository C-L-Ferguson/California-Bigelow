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

# Models: Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter
spec <- Percentage_Prison ~ Election_Year * Decarceratory | County + Quarter

m1 <- feols(spec, data = all_elections,    cluster = ~County.x)
m2 <- feols(spec, data = contested,        cluster = ~County.x)
m3 <- feols(spec, data = incumbent_sought, cluster = ~County.x)

# Results
cat("\n=== All Elections ===\n");          print(summary(m1))
cat("\n=== Contested Elections ===\n");    print(summary(m2))
cat("\n=== Incumbent Sought Reelection ===\n"); print(summary(m3))

# Side-by-side
cat("\n=== Combined Table ===\n")
etable(m1, m2, m3,
       headers = c("All", "Contested", "Incumbent Sought"),
       keep = c("Election_Year", "Decarceratory", "Election_Year:Decarceratory"))
