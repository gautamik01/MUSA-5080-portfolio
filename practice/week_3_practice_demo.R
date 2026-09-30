library(tidyverse)
library(tidycensus)

county_data <- get_acs(
  geography = "county",
  variables = "B07012_002",
  state = "New York",
  year = 2023,
  survey = "acs5"
)

county_data <- county_data %>%
  mutate(moe_pct = moe / estimate * 100)

county_data <- county_data %>%
  mutate(reliability = case_when(
    moe_pct < 5  ~ "High confidence",
    moe_pct < 10 ~ "Moderate",
    TRUE         ~ "Low confidence"
  ))

ggplot(county_data)+
  aes(x=estimate, y = moe_pct, color = "blue")+
  geom_point()

ggplot(county_data)+
  aes(x=estimate, y = moe_pct)+
  geom_point(color = "blue")

# error bar charts. let's build
county_data %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe))

# let's make it less horrible
# reduce to 15 most unreliable.

county_data %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe))

## Flip to make names more readable:
county_data %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = NAME, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

## Sort bars by bar length rather than alphabetically:
county_data %>%
  arrange(desc(moe_pct)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

## Remove "County, New York"

county_data %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, " County, New York")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe)) +
  coord_flip()

#change to narrower width of the error bars

county_data %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, " County, New York")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip()

# tidy up with a nice theme:
county_data %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, " County, New York")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip() +
  theme_minimal()

# fix labels and numbers:

county_data %>%
  arrange(desc(moe_pct)) %>%
  mutate(NAME = str_remove(NAME, " County, New York")) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(NAME, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.3) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Counties with the largest margins of error",
    subtitle = "People below 100% of the poverty level, ACS 2023 5-year estimates",
    x = NULL,
    y = "Population below poverty level"
  )

## Derived Uncertainty

# you want the total number of people below poverty across several couties. You need the MOE for that combined number. you don't just add the MOEs together!

region <- county_data %>%
  filter(NAME %in% c("Hamilton County, New York",
                     "Essex County, New York",
                     "Greene County, New York"))

region_total    <- sum(region$estimate)
region_total_moe <- moe_sum(region$moe, region$estimate)

region_total      # 9,510
region_total_moe  # ~890, not 141 + 493 + 720 = 1,354 (overstates the moe)


county_data %>%
  group_by(reliability) %>%
  summarize(
    total     = sum(estimate),
    total_moe = moe_sum(moe, estimate)
  )

#what proportion of a county's population
poverty_share <- get_acs(
  geography = "county",
  variables = c(below_poverty = "B07012_002",
                total         = "B07012_001"),
  state     = "NY",
  year      = 2023,
  survey    = "acs5",
  output    = "wide"
) %>%
  mutate(
    share     = below_povertyE / totalE,
    share_moe = moe_prop(below_povertyE, totalE,
                         below_povertyM, totalM)
  )

## Change over time

get_poverty <- function(yr) {
  get_acs(
    geography = "county",
    variables = "B07012_002",
    state     = "NY",
    year      = yr,
    survey    = "acs5"
  ) %>%
    mutate(period = paste0(yr - 4, "-", yr))
}

# Reformat to wide to put the years side by side
poverty_change <- bind_rows(
  get_poverty(2018),
  get_poverty(2023)
)

poverty_wide <- poverty_change %>%
  select(GEOID, NAME, period, estimate, moe) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(estimate, moe)
  )

# Compute change

poverty_tested <- poverty_wide %>%
  mutate(
    change   = `estimate_2019-2023` - `estimate_2014-2018`,
    se_1     = `moe_2014-2018` / 1.645,
    se_2     = `moe_2019-2023` / 1.645,
    se_diff  = sqrt(se_1^2 + se_2^2),
    moe_diff = se_diff * 1.645,
    z        = change / se_diff,
    significant = abs(z) > 1.645
  )

poverty_tested %>%
  count(significant)

# Visualize with error bars
poverty_tested %>%
  filter(significant) %>%
  mutate(NAME = str_remove(NAME, " County, New York")) %>%
  ggplot(aes(x = reorder(NAME, change), y = change)) +
  geom_col(aes(fill = change > 0)) +
  geom_errorbar(aes(ymin = change - moe_diff,
                    ymax = change + moe_diff), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Statistically significant change in poverty count",
       subtitle = "2014–2018 vs 2019–2023, ACS 5-year estimates",
       x = NULL, y = "Change in people below poverty level")

## Create a fancy reliability table like in the paper:

library(gt)

reliability_table <- county_data %>%
  mutate(
    cv = (moe / 1.645) / estimate * 100,
    dot = case_when(
      cv < 12  ~ "🟢",
      cv <= 40 ~ "🟡",
      TRUE     ~ "🔴"
    ),
    flag = case_when(
      cv < 12  ~ "Reliable",
      cv <= 40 ~ "Somewhat reliable",
      TRUE     ~ "Unreliable"
    ),
    NAME = str_remove(NAME, " County, New York")
  ) %>%
  arrange(desc(cv)) %>%
  select(dot, NAME, estimate, moe, cv, flag)

reliability_table %>%
  gt() %>%
  fmt_number(columns = c(estimate, moe), decimals = 0) %>%
  fmt_number(columns = cv, decimals = 1) %>%
  cols_label(
    dot      = "",
    NAME     = "County",
    estimate = "Estimate",
    moe      = "MOE",
    cv       = "CV (%)",
    flag     = "Reliability"
  ) %>%
  cols_align(align = "center", columns = dot) %>%
  tab_header(
    title    = "Population below 100% of the poverty level",
    subtitle = "New York counties, ACS 2023 5-year estimates"
  ) %>%
  tab_source_note(
    "Reliability thresholds follow Jurjevich et al. (2018): CV < 12% reliable, 12–40% somewhat reliable, > 40% unreliable."
  )
