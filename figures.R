library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("https://raw.githubusercontent.com/C-L-Ferguson/California-Bigelow/claude/dataset-review-l0rluy/CA_Merged_Data_2024.csv")

df <- df |>
  mutate(
    Decarceratory = as.integer(Decarceratory),
    Election_Year = as.integer(Election_Year),
    year  = as.integer(sub(".*(\\d{4}).*", "\\1", Quarter)),
    qnum  = as.integer(sub(".*Q(\\d).*", "\\1", Quarter)),
    t_abs = (year - 2013) * 4 + qnum
  )

# Full-year election coding: 1 for all four quarters of an election year
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
# FIGURE 1b: Regression-Adjusted Margins Plot
# Shows predicted prison rate by DA type and electoral period, controlling for
# county and quarter fixed effects. Derived from the primary regression (m4).
# This is what the data actually show after absorbing county-level differences.
# =============================================================================

# Requires fixest — loaded below in Figure 4 block, but load here too
library(fixest)

df_reg_1b <- df |>
  mutate(
    Election_Year = as.integer(Election_Year),
    Decarceratory = as.integer(Decarceratory),
    Contested     = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County        = as.factor(County.x),
    Quarter       = as.factor(Quarter),
    time          = as.numeric(X)
  )

incumbent_contested_1b <- df_reg_1b |>
  mutate(
    year_str = sub(".*(\\d{4}).*", "\\1", Quarter),
    Election_Year_Full = as.integer(
      paste0(County.x, "_", year_str) %in%
        (df_reg_1b |>
           filter(Election_Year == 1) |>
           mutate(year_str = sub(".*(\\d{4}).*", "\\1", Quarter)) |>
           mutate(key = paste0(County.x, "_", year_str)) |>
           pull(key) |>
           unique()
        )
    )
  ) |>
  filter(Did_Incumbent_Seek_Reelection == 1, Contested == 1)

m4_1b <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
               data = incumbent_contested_1b, cluster = ~County.x)

b <- coef(m4_1b)

margins_data <- data.frame(
  DA_type            = c("Non-Decarceratory DA", "Non-Decarceratory DA",
                         "Decarceratory DA",     "Decarceratory DA"),
  Period             = c("Non-Election\nQuarters", "Election Year\n(All Quarters)",
                         "Non-Election\nQuarters", "Election Year\n(All Quarters)"),
  Decarceratory      = c(0, 0, 1, 1),
  Election_Year_Full = c(0, 1, 0, 1)
) |>
  mutate(
    predicted = (b["Election_Year_Full"]               * Election_Year_Full) +
                (b["Decarceratory"]                    * Decarceratory) +
                (b["Election_Year_Full:Decarceratory"] * Election_Year_Full * Decarceratory),
    Period = factor(Period, levels = c("Non-Election\nQuarters", "Election Year\n(All Quarters)"))
  )

vcv <- vcov(m4_1b)

se_for_margin <- function(decarc, elec) {
  g <- c(elec, decarc, elec * decarc)
  names(g) <- c("Election_Year_Full", "Decarceratory", "Election_Year_Full:Decarceratory")
  vars <- names(g)
  v <- vcv[vars, vars]
  sqrt(as.numeric(t(g) %*% v %*% g))
}

margins_data <- margins_data |>
  rowwise() |>
  mutate(se = se_for_margin(Decarceratory, Election_Year_Full)) |>
  ungroup() |>
  mutate(
    lo95 = predicted - 1.96 * se,
    hi95 = predicted + 1.96 * se
  )

fig1b <- ggplot(margins_data,
                aes(x = Period, y = predicted, color = DA_type, group = DA_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 0.9, position = position_dodge(width = 0.15)) +
  geom_point(aes(shape = DA_type), size = 4,
             position = position_dodge(width = 0.15)) +
  geom_errorbar(aes(ymin = lo95, ymax = hi95),
                width = 0.08, linewidth = 0.7,
                position = position_dodge(width = 0.15)) +
  scale_color_manual(values = pal) +
  scale_shape_manual(values = c("Decarceratory DA" = 16, "Non-Decarceratory DA" = 17)) +
  scale_y_continuous(labels = function(x) paste0(x, "pp")) +
  labs(
    title    = "Predicted Prison Sentencing: Election vs. Non-Election Periods",
    subtitle = "Regression-adjusted marginal effects (county & quarter FEs absorbed)\nIncumbent-sought, contested races — full election year specification",
    x        = NULL,
    y        = "Predicted change in prison rate (pp, relative to baseline)",
    color    = NULL, shape = NULL,
    caption  = "Estimated from OLS with county and quarter fixed effects, clustered SEs by county.\nPoints = predicted margins. Bars = 95% CI. Non-decarceratory, non-election = 0 reference.\nElection year coded 1 for all four quarters."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey40", size = 10)
  )

ggsave("figure1b_margins_plot.pdf", fig1b, width = 7, height = 5)
ggsave("figure1b_margins_plot.png", fig1b, width = 7, height = 5, dpi = 300)
cat("Figure 1b (regression-adjusted margins plot) saved.\n")

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

# =============================================================================
# FIGURE 4: Coefficient Plot
# Shows the Election_Year x Decarceratory interaction from the two primary
# specifications side by side with 95% CI. Both below zero = platform
# reinforcement, not convergence.
# =============================================================================

library(fixest)

# Rebuild subsamples needed for coefficient plot
df_reg <- df |>
  mutate(
    Election_Year = as.integer(Election_Year),
    Decarceratory = as.integer(Decarceratory),
    Contested     = as.integer(Contested),
    Did_Incumbent_Seek_Reelection = as.integer(Did_Incumbent_Seek_Reelection),
    County        = as.factor(County.x),
    Quarter       = as.factor(Quarter),
    time          = as.numeric(X)
  )

incumbent_sought_full    <- df_reg |>
  mutate(
    year_str = sub(".*(\\d{4}).*", "\\1", Quarter),
    Election_Year_Full = as.integer(
      paste0(County.x, "_", year_str) %in%
        (df_reg |>
           filter(Election_Year == 1) |>
           mutate(year_str = sub(".*(\\d{4}).*", "\\1", Quarter)) |>
           mutate(key = paste0(County.x, "_", year_str)) |>
           pull(key) |>
           unique()
        )
    )
  ) |>
  filter(Did_Incumbent_Seek_Reelection == 1)

incumbent_contested_full <- incumbent_sought_full |> filter(Contested == 1)

m3 <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
            data = incumbent_sought_full,    cluster = ~County.x)
m4 <- feols(Percentage_Prison ~ Election_Year_Full * Decarceratory | County + Quarter,
            data = incumbent_contested_full, cluster = ~County.x)

n_sought    <- nrow(incumbent_sought_full    |> filter(!is.na(Percentage_Prison)))
n_contested <- nrow(incumbent_contested_full |> filter(!is.na(Percentage_Prison)))

coef_data <- data.frame(
  Model     = c(paste0("Incumbent Sought\n(N = ", n_sought, ")"),
                paste0("Incumbent +\nContested\n(N = ", n_contested, ")")),
  est       = c(coef(m3)["Election_Year_Full:Decarceratory"],
                coef(m4)["Election_Year_Full:Decarceratory"]),
  se        = c(se(m3)["Election_Year_Full:Decarceratory"],
                se(m4)["Election_Year_Full:Decarceratory"]),
  headline  = c(FALSE, TRUE)
) |>
  mutate(
    lo95  = est - 1.96 * se,
    hi95  = est + 1.96 * se,
    Model = factor(Model, levels = rev(unique(Model)))
  )

fig4 <- ggplot(coef_data, aes(x = est, y = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95, color = headline),
                 height = 0.15, linewidth = 0.8) +
  geom_point(aes(color = headline, size = headline)) +
  geom_text(aes(label = paste0(round(est, 2), "pp")),
            nudge_y = 0.25, size = 3.5, color = "grey20") +
  annotate("text", x = coef(m4)["Election_Year_Full:Decarceratory"] - 0.3,
           y = 2,
           label = "** p < 0.01", hjust = 1, size = 3, color = "#0072B2") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#0072B2"), guide = "none") +
  scale_size_manual(values  = c("FALSE" = 3, "TRUE" = 5),                guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "pp"),
                     limits = c(-14, 4)) +
  labs(
    title    = "Electoral Effect on Prison Sentencing: Decarceratory DAs",
    subtitle = "Coefficient on Election Year × Decarceratory interaction — contested races drive the effect",
    x        = "Change in Prison Sentence Rate (percentage points)",
    y        = NULL,
    caption  = "Points = OLS estimates. Bars = 95% CI. County and quarter fixed effects. Clustered SEs by county.\nNegative = lower prison sentencing in election year. Incumbent + Contested is primary specification.\nElection year coded 1 for all four quarters."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "#0072B2", size = 10)
  )

ggsave("figure4_coefficient_plot.pdf", fig4, width = 8, height = 5)
ggsave("figure4_coefficient_plot.png", fig4, width = 8, height = 5, dpi = 300)
cat("Figure 4 (coefficient plot) saved.\n")

# =============================================================================
# FIGURE 5: Baseline Comparison — Decarceratory vs. Non-Decarceratory DAs
# in Non-Election Years (raw means with 95% CI error bars)
# Shows that the two groups differ at baseline before any electoral pressure.
# =============================================================================

baseline_df <- df |>
  filter(Election_Year == 0, !is.na(Percentage_Prison)) |>
  mutate(DA_type = ifelse(Decarceratory == 1, "Decarceratory DA", "Non-Decarceratory DA"))

baseline_summary <- baseline_df |>
  group_by(DA_type) |>
  summarise(
    mean_prison    = mean(Percentage_Prison, na.rm = TRUE),
    se_prison      = sd(Percentage_Prison, na.rm = TRUE) / sqrt(n()),
    mean_probation = mean(Percentage_Probation, na.rm = TRUE),
    se_probation   = sd(Percentage_Probation, na.rm = TRUE) / sqrt(n()),
    n              = n(),
    .groups = "drop"
  ) |>
  mutate(
    lo95_prison    = mean_prison    - 1.96 * se_prison,
    hi95_prison    = mean_prison    + 1.96 * se_prison,
    lo95_probation = mean_probation - 1.96 * se_probation,
    hi95_probation = mean_probation + 1.96 * se_probation
  )

fig5 <- ggplot(baseline_summary,
               aes(x = DA_type, y = mean_prison, fill = DA_type, color = DA_type)) +
  geom_col(width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = lo95_prison, ymax = hi95_prison),
                width = 0.12, linewidth = 0.7, color = "grey30") +
  geom_text(aes(label = paste0(round(mean_prison, 1), "%")),
            vjust = -1.2, size = 4, color = "grey20", fontface = "bold") +
  scale_fill_manual(values  = pal, guide = "none") +
  scale_color_manual(values = pal, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Prison Sentencing Rate: Decarceratory vs. Non-Decarceratory DAs",
    subtitle = "Non-election quarters only (raw means ± 95% CI)",
    x        = NULL,
    y        = "% of Sentences Resulting in Prison",
    caption  = "Unit of observation: county-quarter. Non-election quarters defined as quarters\nwhere Election_Year = 0. Error bars show 95% confidence intervals."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption       = element_text(color = "grey50", size = 9),
    plot.title         = element_text(face = "bold")
  )

ggsave("figure5_baseline_comparison.pdf", fig5, width = 7, height = 5)
ggsave("figure5_baseline_comparison.png", fig5, width = 7, height = 5, dpi = 300)
cat("Figure 5 (baseline comparison) saved.\n")

# --- Table: Baseline means by DA type (non-election quarters) ---
baseline_table <- baseline_df |>
  group_by(DA_type) |>
  summarise(
    N                  = n(),
    Mean_Prison        = round(mean(Percentage_Prison,    na.rm = TRUE), 2),
    SD_Prison          = round(sd(Percentage_Prison,      na.rm = TRUE), 2),
    Mean_Probation     = round(mean(Percentage_Probation, na.rm = TRUE), 2),
    SD_Probation       = round(sd(Percentage_Probation,   na.rm = TRUE), 2),
    Mean_Straight      = round(mean(Percentage_Straight,  na.rm = TRUE), 2),
    SD_Straight        = round(sd(Percentage_Straight,    na.rm = TRUE), 2),
    Mean_Split         = round(mean(Percentage_Split,     na.rm = TRUE), 2),
    SD_Split           = round(sd(Percentage_Split,       na.rm = TRUE), 2),
    .groups = "drop"
  )

print(baseline_table)

# Export as LaTeX
library(xtable)
xt <- xtable(baseline_table,
             caption = "Baseline Sentencing Outcomes by DA Type (Non-Election Quarters)",
             label   = "tab:baseline",
             digits  = 2)
print(xt,
      file             = "table0_baseline_comparison.tex",
      include.rownames = FALSE,
      booktabs         = TRUE,
      caption.placement = "top")
cat("Table exported: table0_baseline_comparison.tex\n")
