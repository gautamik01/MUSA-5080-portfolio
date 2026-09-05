library(tidyverse)
library(tidycensus)

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

dim(pa_income) # Pennsylvania has 67 counties and 5 variable data is logged
glimpse(pa_income) # shows every column, rotated sideways with its data type
head(pa_income) # shows the first 6 rows, in the normal spreadsheet-style layout

pa_income$GEOID # Will display values of the mentioned dataset.

as.numeric("01001") #It did not write 0 since its a numeric number

filter(pa_income, estimate > 60000)

# Self practice
# Counties where margin of error is bigger than 3000
filter(pa_income, moe > 3000)

# Counties where the estimate is under 50000)
filter(pa_income, estimate < 50000)

select(pa_income, NAME, estimate, moe) # It will select the given columns

# mutate() - Makes a new column
mutate(pa_income, moe_pct = moe/ estimate * 100) # Created an additional column of moe percentage, but only appears in console


pa_income <- mutate(pa_income, moe_pct = moe/estimate * 100)
pa_income

# arrange() - Sorts

arrange(pa_income, moe_pct) # It defaults to ascending
arrange(pa_income, desc(moe_pct)) # Arranges in descending order

# PIPE %>% - and then do this
# step1 <- filter(pa_income, moe_pct > 5)
# step2 <- arrange(step1, desc(moe_pct))
# step3 <- select(step2, NAME, estimate, moe, moe_pct)

pa_income%>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct) # Shorten and take the result of the first step



worst <- pa_income %>%  # assigned this result to worst resut <- pa_income
  filter(moe_pct > 8) %>%
  arrange(desc(estimate)) %>%
  select(NAME, moe_pct)

# group_by() + summarize() - Many rows become FEW

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(), avg_income = mean(estimate))

# case_when() - sorting into categories

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence", # 26 counties
    moe_pct < 6 ~ "Moderate",        # 34 counties
    TRUE        ~ "Low confidence"   # 7 counties
  ))

count (pa_income, reliability)

# two Variables and a shape problem
pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two #Ans : Because every county will give you 2 values. Its population and its income. so you have 134 rows and not 67.

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
#Ans : E = estimate and M = Margin or error.

pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
#Ans : the moe for counties with less population is higher than those with higher population, except Montour County.



# Trying out for another state

pa_wide2 <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "CA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide2 %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
#Ans : Similar trend for California is seen. The moe for counties with less population is higher than those with
# higher population, except for Mono County.








