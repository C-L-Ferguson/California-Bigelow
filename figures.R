library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("https://raw.githubusercontent.com/C-L-Ferguson/California-Bigelow/claude/dataset-review-l0rluy/CA_Merged_Data_FEB_3.csv")

df <- df |>
  mutate(
    Decarceratory = as.integer(Decarceratory),
    Election_Year = as.integer(Election_Year),
    # Parse year and quarter number from Quarter column (e.g. " 2014 Q3 Court")
    year  = as.integer(regmatches(Quarter, regexpr("\\d{4}", Quarter))),
    qnum  = as.integer(regmatches(Quarter, regexpr("(?<=Q)\\d", Quarter, perl = TRUE))),
    # Numeric time index: quarters since 2013 Q1
    t_abs = (year - 2013) * 4 + qnum
  )

# =============================================================================
# FIGURE 1: Event Study
# For each county-election, compute quarters relative to election year Q3 (t=0).
# Average Percentage_Prison by relative quarter and DA type.
# =============================================================================

# Identify election events: county + year where Q3 is an election quarter
election_events <- df |>
  filter(Election_Year == 1, qnum == 3) |>
  select(County.x, year) |>
  distinct() |>
  rename(election_year = year)

# For each observation, find its nearest prior election event within the same county
# and compute relative quarter
event_study <- df |>
  inner_join(election_events, by = "County.x") |>
  mutate(
    # t=0 is election year Q3; each quarter is 1 unit
    rel_q = (year - election_year) * 4 + (qnum - 3)
  ) |>
  # Keep window: 6 quarters before through 5 quarters after election Q3
  filter(rel_q >= -6, rel_q <= 5) |>
  # For counties with multiple elections, keep only the closest event
  group_by(County.x, year, qnum) |>
  slice_min(abs(rel_q - 0), n = 1, with_ties = FALSE) |>
  ungroup()

# Average by relative quarter and DA type
es_avg <- event_study |>
  filter(!is.na(Percentage_Prison)) |>
  mutate(DA_type = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA")) |>
  group_by(rel_q, DA_type) |>
  summarise(
    mean_prison = mean(Percentage_Prison, na.rm = TRUE),
    se_prison   = sd(Percentage_Prison, na.rm = TRUE) / sqrt(n()),
    n           = n(),
    .groups     = "drop"
  )

# Quarter labels for x-axis
quarter_labels <- c("-6"="Q1\n-2yr", "-5"="Q2\n-2yr", "-4"="Q3\n-2yr", "-3"="Q4\n-2yr",
                    "-2"="Q1\n-1yr", "-1"="Q2\n-1yr",
                     "0"="Q3\nElec", "1"="Q4\nElec",
                     "2"="Q1\n+1yr", "3"="Q2\n+1yr", "4"="Q3\n+1yr", "5"="Q4\n+1yr")

# CVD-safe two-color palette (Wong 2011)
pal <- c("Decarceratory DA" = "#0072B2", "Non-Decarceratory DA" = "#E69F00")

fig1 <- ggplot(es_avg, aes(x = rel_q, y = mean_prison,
                            color = DA_type, shape = DA_type, group = DA_type)) +
  # Shade election quarters
  annotate("rect", xmin = -0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.6) +
  annotate("text", x = 0.5, y = Inf, label = "Election\nQuarters",
           vjust = 1.5, size = 3, color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept =  1.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  # Confidence bands
  geom_ribbon(aes(ymin = mean_prison - 1.96 * se_prison,
                  ymax = mean_prison + 1.96 * se_prison,
                  fill = DA_type), alpha = 0.15, color = NA) +
  # Lines and points
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values  = pal) +
  scale_shape_manual(values = c("Decarceratory DA" = 16, "Non-Decarceratory DA" = 17)) +
  scale_x_continuous(breaks = -6:5, labels = quarter_labels) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Prison Sentence Rate Relative to Election Quarter",
    subtitle = "Average % sentenced to prison by quarter relative to election year Q3 (t = 0)",
    x        = "Quarter Relative to Election",
    y        = "% Sentenced to Prison",
    color    = NULL, fill = NULL, shape = NULL,
    caption  = "Shaded region = election quarters (Q3–Q4). Bands = 95% CI. N = 8 decarceratory counties."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "top",
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption      = element_text(color = "grey50", size = 9),
    plot.title        = element_text(face = "bold")
  )

ggsave("figure1_event_study.pdf", fig1, width = 8, height = 5)
ggsave("figure1_event_study.png", fig1, width = 8, height = 5, dpi = 300)
cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2: County-Level Prison Rate Trends
# Show Alameda, San Francisco, Santa Clara — three prominent decarceratory
# counties — with election quarters highlighted.
# =============================================================================

focus_counties <- c("Alameda", "San Francisco", "Santa Clara")

county_data <- df |>
  filter(County.x %in% focus_counties, !is.na(Percentage_Prison)) |>
  mutate(
    date     = year + (qnum - 1) / 4,  # decimal year for x-axis
    County.x = factor(County.x, levels = focus_counties)
  )

# Election quarter shading rectangles per county
election_rects <- county_data |>
  filter(Election_Year == 1) |>
  group_by(County.x) |>
  summarise(
    xmin = min(year + (qnum - 1) / 4) - 0.05,
    xmax = max(year + (qnum - 1) / 4) + 0.3,
    .groups = "drop"
  )

fig2 <- ggplot(county_data, aes(x = date, y = Percentage_Prison)) +
  # Shade election quarters per facet
  geom_rect(data = election_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#0072B2", alpha = 0.12) +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_point(color = "#0072B2", size = 1.5) +
  facet_wrap(~County.x, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Prison Sentence Rate Over Time: Selected Decarceratory Counties",
    subtitle = "Shaded regions indicate election quarters (Q3–Q4 of election years)",
    x        = NULL,
    y        = "% Sentenced to Prison",
    caption  = "Alameda, San Francisco, and Santa Clara all had decarceratory DAs throughout most of this period."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

ggsave("figure2_county_trends.pdf", fig2, width = 8, height = 9)
ggsave("figure2_county_trends.png", fig2, width = 8, height = 9, dpi = 300)
cat("Figure 2 saved.\n")
