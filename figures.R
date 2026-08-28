library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("https://raw.githubusercontent.com/C-L-Ferguson/California-Bigelow/claude/dataset-review-l0rluy/CA_Merged_Data_FEB_3.csv")

df <- df |>
  mutate(
    Decarceratory = as.integer(Decarceratory),
    Election_Year = as.integer(Election_Year),
    year  = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter)),
    qnum  = as.integer(sub(".*Q(\\d).*", "\\1", Quarter)),
    t_abs = (year - 2013) * 4 + qnum
  )

# CVD-safe two-color palette (Wong 2011)
pal <- c("Decarceratory DA" = "#0072B2", "Non-Decarceratory DA" = "#E69F00")

# =============================================================================
# FIGURE 1a: Event Study (original — noisy, for reference)
# =============================================================================

election_events <- df |>
  filter(Election_Year == 1, qnum == 3) |>
  select(County.x, year) |>
  distinct() |>
  rename(election_year = year)

event_study <- df |>
  inner_join(election_events, by = "County.x") |>
  mutate(rel_q = (year - election_year) * 4 + (qnum - 3)) |>
  filter(rel_q >= -6, rel_q <= 5) |>
  group_by(County.x, year, qnum) |>
  slice_min(abs(rel_q - 0), n = 1, with_ties = FALSE) |>
  ungroup()

es_avg <- event_study |>
  filter(!is.na(Percentage_Prison)) |>
  mutate(DA_type = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA")) |>
  group_by(rel_q, DA_type) |>
  summarise(
    mean_prison = mean(Percentage_Prison, na.rm = TRUE),
    se_prison   = sd(Percentage_Prison, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

quarter_labels <- c("-6"="Q1\n-2yr", "-5"="Q2\n-2yr", "-4"="Q3\n-2yr", "-3"="Q4\n-2yr",
                    "-2"="Q1\n-1yr", "-1"="Q2\n-1yr",
                     "0"="Q3\nElec",  "1"="Q4\nElec",
                     "2"="Q1\n+1yr",  "3"="Q2\n+1yr", "4"="Q3\n+1yr", "5"="Q4\n+1yr")

fig1a <- ggplot(es_avg, aes(x = rel_q, y = mean_prison,
                             color = DA_type, shape = DA_type, group = DA_type)) +
  annotate("rect", xmin = -0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.6) +
  annotate("text", x = 0.5, y = Inf, label = "Election\nQuarters",
           vjust = 1.5, size = 3, color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept =  1.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = mean_prison - 1.96 * se_prison,
                  ymax = mean_prison + 1.96 * se_prison,
                  fill = DA_type), alpha = 0.15, color = NA) +
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
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold")
  )

ggsave("figure1a_event_study.pdf", fig1a, width = 8, height = 5)
ggsave("figure1a_event_study.png", fig1a, width = 8, height = 5, dpi = 300)
cat("Figure 1a (event study) saved.\n")

# =============================================================================
# FIGURE 1b: Before/After Bar Chart (simple, law-review-friendly)
# Average prison rate for each DA type in election vs. non-election quarters.
# =============================================================================

bar_data <- df |>
  filter(!is.na(Percentage_Prison)) |>
  mutate(
    DA_type   = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA"),
    Period    = ifelse(Election_Year == 1, "Election Quarters\n(Q3–Q4)", "Non-Election\nQuarters")
  ) |>
  group_by(DA_type, Period) |>
  summarise(
    mean_prison = mean(Percentage_Prison, na.rm = TRUE),
    se_prison   = sd(Percentage_Prison, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(Period = factor(Period, levels = c("Non-Election\nQuarters", "Election Quarters\n(Q3–Q4)")))

fig1b <- ggplot(bar_data, aes(x = Period, y = mean_prison, fill = DA_type)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = paste0(round(mean_prison, 1), "%"),
                y = mean_prison + 0.5),
            position = position_dodge(width = 0.6), size = 3.5, color = "grey30") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Prison Sentence Rate: Election vs. Non-Election Quarters",
    subtitle = "Average % sentenced to prison by DA type and electoral period",
    x        = NULL,
    y        = "% Sentenced to Prison",
    fill     = NULL,
    caption  = "Error bars = 95% CI."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold")
  )

ggsave("figure1b_bar_chart.pdf", fig1b, width = 7, height = 5)
ggsave("figure1b_bar_chart.png", fig1b, width = 7, height = 5, dpi = 300)
cat("Figure 1b (bar chart) saved.\n")

# =============================================================================
# FIGURE 2a: County Trends — Raw (original)
# =============================================================================

focus_counties <- c("Alameda", "San Francisco", "Santa Clara")

county_data <- df |>
  filter(County.x %in% focus_counties, !is.na(Percentage_Prison)) |>
  mutate(
    date     = year + (qnum - 1) / 4,
    County.x = factor(County.x, levels = focus_counties)
  )

# One shading rectangle per county per election year (Q3–Q4 only)
election_rects <- df |>
  filter(County.x %in% focus_counties, Election_Year == 1) |>
  mutate(date = year + (qnum - 1) / 4) |>
  group_by(County.x, year) |>
  summarise(xmin = min(date) - 0.05, xmax = max(date) + 0.3, .groups = "drop") |>
  mutate(County.x = factor(County.x, levels = focus_counties))

fig2a <- ggplot(county_data, aes(x = date, y = Percentage_Prison)) +
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
    x        = NULL, y = "% Sentenced to Prison",
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

ggsave("figure2a_county_trends_raw.pdf",  fig2a, width = 8, height = 9)
ggsave("figure2a_county_trends_raw.png",  fig2a, width = 8, height = 9, dpi = 300)
cat("Figure 2a (county trends, raw) saved.\n")

# =============================================================================
# FIGURE 2b: County Trends — Smoothed (4-quarter rolling average)
# Reduces quarter-to-quarter noise; makes within-county trend easier to read.
# =============================================================================

county_data_smooth <- county_data |>
  arrange(County.x, date) |>
  group_by(County.x) |>
  mutate(prison_smooth = zoo::rollmean(Percentage_Prison, k = 4, fill = NA, align = "center")) |>
  ungroup()

fig2b <- ggplot(county_data_smooth, aes(x = date)) +
  geom_rect(data = election_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#0072B2", alpha = 0.12) +
  # Raw data as faint background
  geom_line(aes(y = Percentage_Prison), color = "#0072B2", linewidth = 0.4, alpha = 0.3) +
  # Smoothed line on top
  geom_line(aes(y = prison_smooth), color = "#0072B2", linewidth = 1.2) +
  facet_wrap(~County.x, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Prison Sentence Rate Over Time: Selected Decarceratory Counties",
    subtitle = "Bold line = 4-quarter rolling average. Faint line = raw quarterly data. Shaded = election quarters.",
    x        = NULL, y = "% Sentenced to Prison",
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

ggsave("figure2b_county_trends_smooth.pdf", fig2b, width = 8, height = 9)
ggsave("figure2b_county_trends_smooth.png", fig2b, width = 8, height = 9, dpi = 300)
cat("Figure 2b (county trends, smoothed) saved.\n")

# =============================================================================
# FIGURE 3a: Event Study — Probation
# =============================================================================

es_avg_prob <- event_study |>
  filter(!is.na(Percentage_Probation)) |>
  mutate(DA_type = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA")) |>
  group_by(rel_q, DA_type) |>
  summarise(
    mean_prob = mean(Percentage_Probation, na.rm = TRUE),
    se_prob   = sd(Percentage_Probation, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig3a <- ggplot(es_avg_prob, aes(x = rel_q, y = mean_prob,
                                  color = DA_type, shape = DA_type, group = DA_type)) +
  annotate("rect", xmin = -0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.6) +
  annotate("text", x = 0.5, y = Inf, label = "Election\nQuarters",
           vjust = 1.5, size = 3, color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept =  1.5, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = mean_prob - 1.96 * se_prob,
                  ymax = mean_prob + 1.96 * se_prob,
                  fill = DA_type), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values  = pal) +
  scale_shape_manual(values = c("Decarceratory DA" = 16, "Non-Decarceratory DA" = 17)) +
  scale_x_continuous(breaks = -6:5, labels = quarter_labels) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Probation Sentence Rate Relative to Election Quarter",
    subtitle = "Average % sentenced to probation by quarter relative to election year Q3 (t = 0)",
    x        = "Quarter Relative to Election",
    y        = "% Sentenced to Probation",
    color    = NULL, fill = NULL, shape = NULL,
    caption  = "Shaded region = election quarters (Q3–Q4). Bands = 95% CI. N = 8 decarceratory counties."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold")
  )

ggsave("figure3a_probation_event_study.pdf", fig3a, width = 8, height = 5)
ggsave("figure3a_probation_event_study.png", fig3a, width = 8, height = 5, dpi = 300)
cat("Figure 3a (probation event study) saved.\n")

# =============================================================================
# FIGURE 3b: Bar Chart — Probation
# =============================================================================

bar_data_prob <- df |>
  filter(!is.na(Percentage_Probation)) |>
  mutate(
    DA_type = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA"),
    Period  = ifelse(Election_Year == 1, "Election Quarters\n(Q3–Q4)", "Non-Election\nQuarters")
  ) |>
  group_by(DA_type, Period) |>
  summarise(
    mean_prob = mean(Percentage_Probation, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(Period = factor(Period, levels = c("Non-Election\nQuarters", "Election Quarters\n(Q3–Q4)")))

fig3b <- ggplot(bar_data_prob, aes(x = Period, y = mean_prob, fill = DA_type)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = paste0(round(mean_prob, 1), "%"),
                y = mean_prob + 0.5),
            position = position_dodge(width = 0.6), size = 3.5, color = "grey30") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Probation Sentence Rate: Election vs. Non-Election Quarters",
    subtitle = "Average % sentenced to probation by DA type and electoral period",
    x        = NULL,
    y        = "% Sentenced to Probation",
    fill     = NULL,
    caption  = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold")
  )

ggsave("figure3b_probation_bar_chart.pdf", fig3b, width = 7, height = 5)
ggsave("figure3b_probation_bar_chart.png", fig3b, width = 7, height = 5, dpi = 300)
cat("Figure 3b (probation bar chart) saved.\n")

# =============================================================================
# FIGURE 3c: County Trends — Probation (Smoothed)
# =============================================================================

county_data_prob <- df |>
  filter(County.x %in% focus_counties, !is.na(Percentage_Probation)) |>
  mutate(
    date     = year + (qnum - 1) / 4,
    County.x = factor(County.x, levels = focus_counties)
  ) |>
  arrange(County.x, date) |>
  group_by(County.x) |>
  mutate(prob_smooth = zoo::rollmean(Percentage_Probation, k = 4, fill = NA, align = "center")) |>
  ungroup()

fig3c <- ggplot(county_data_prob, aes(x = date)) +
  geom_rect(data = election_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#0072B2", alpha = 0.12) +
  geom_line(aes(y = Percentage_Probation), color = "#E69F00", linewidth = 0.4, alpha = 0.3) +
  geom_line(aes(y = prob_smooth), color = "#E69F00", linewidth = 1.2) +
  facet_wrap(~County.x, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Probation Sentence Rate Over Time: Selected Decarceratory Counties",
    subtitle = "Bold line = 4-quarter rolling average. Faint line = raw quarterly data. Shaded = election quarters.",
    x        = NULL, y = "% Sentenced to Probation",
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

ggsave("figure3c_probation_county_trends.pdf", fig3c, width = 8, height = 9)
ggsave("figure3c_probation_county_trends.png", fig3c, width = 8, height = 9, dpi = 300)
cat("Figure 3c (probation county trends) saved.\n")
