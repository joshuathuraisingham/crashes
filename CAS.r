library(dplyr)
library(tidyr)

## load dataset
crashes <- read.csv("Crash_Analysis_System_(CAS)_data.csv")

## explore data
dim(crashes)
head(crashes)
names(crashes)


## response variable - crashSeverity
crashes %>% group_by(crashSeverity) %>% summarise(total_count=n())

# group fatal crash + serious crash = major crash, minor crash + non-injury crash = minor crash
crashes$outcome <- ifelse((crashes$crashSeverity == "Fatal Crash" | crashes$crashSeverity == "Serious Crash"), 
                          "Major Crash", "Minor Crash")
crashes %>% group_by(outcome) %>% summarise(total_count=n())


## investigate time period
min(crashes$crashYear)  
max(crashes$crashYear)  
print(crashes %>% group_by(crashYear) %>% summarise(total_count=n()), n=30)

# take 10 year sample (2013 - 2022), not enough data in 2023 - might skew data as 
#  minor crashes take more time to register in system than major crashes

crashes_2 <- crashes %>% filter(crashYear %in% (2013:2022))

## investigate region
crashes_2 %>% group_by(region) %>% summarise(total_count=n())

# remove rows where region is blank
crashes_3 <- crashes_2 %>% filter(region != "")


major_crashes_by_region <- crashes_3 %>% group_by(region, outcome) %>% summarise(total_count=n()) %>% 
  pivot_wider(names_from = "outcome", values_from="total_count") %>% mutate("%-major-crashes" = `Major Crash`/(`Major Crash`+`Minor Crash`))


## gather population data from latest year (2022)
# https://nzdotstat.stats.govt.nz/wbos/Index.aspx?DataSetCode=TABLECODE7979
population <- read.csv("population.csv")

region_crash_data <- merge(x=major_crashes_by_region, y=population, by.x = "region", by.y = "ï..region") %>%
  mutate("%-major-crashes-pop" = `Major Crash`/`population`) %>% 
  mutate("%-total-crashes-pop" = (`Major Crash`+`Minor Crash`)/`population`)


write.csv(region_crash_data, "region_crash_data.csv")



## investigate trend of % of major crashes to total (West Coast vs Rest of NZ)
crashes_3$region <- ifelse(crashes_3$region == "West Coast Region", 
                          "West Coast Region", "Rest of NZ")
crash_trend <- crashes_3 %>% group_by(region, crashYear, outcome) %>% summarise(total_count=n()) %>%
  pivot_wider(names_from = "outcome", values_from="total_count") %>% mutate("%-major-crashes" = `Major Crash`/(`Major Crash`+`Minor Crash`))

write.csv(crash_trend, "crash_trend.csv")


## investigate % of major crashes to total by holiday (West Coast vs Rest of NZ)
holiday_crashes <- crashes_3 %>% group_by(region, holiday, outcome) %>% summarise(total_count=n()) %>%
  pivot_wider(names_from = "outcome", values_from="total_count") %>% mutate("%-major-crashes" = `Major Crash`/(`Major Crash`+`Minor Crash`))

holiday_crashes$holiday <- ifelse(holiday_crashes$holiday == "",
                                  "No Holiday", holiday_crashes$holiday) 

write.csv(holiday_crashes, "holiday_crashes.csv")


## investigate % of major crashes to total by motorcycle crashes (West Coast vs Rest of NZ)
motorcycle_crashes <- crashes_3 %>% filter(motorcycle >= 1) %>% 
  group_by(region, outcome) %>% summarise(total_count=n()) %>%
  pivot_wider(names_from = "outcome", values_from="total_count") %>% 
  mutate("%-major-crashes" = `Major Crash`/(`Major Crash`+`Minor Crash`)) %>%
  select(-c(`Major Crash`, `Minor Crash`)) %>%
  pivot_wider(names_from = "region", values_from="%-major-crashes")


write.csv(motorcycle_crashes, "motorcycle_crashes.csv")


## investigate % of major crashes to total crashes in the dark by street light (West Coast vs Rest of NZ)
crashes_3 %>% filter(light == "Dark") %>% group_by(streetLight) %>%
  summarise(total_count=n(), .groups = 'drop')

crashes_3 %>% group_by(light) %>% summarise(total_count=n(), .groups = 'drop')

crashes_4 <- crashes_3 %>% filter(streetLight != "Null") 
crashes_4$streetLight <- ifelse((crashes_4$streetLight == "None" | crashes_4$streetLight == "Off"),
                                "No", "Yes")
street_light_crashes <- crashes_4 %>% filter(light == "Dark")  %>% group_by(region, streetLight, outcome) %>% summarise(total_count=n(), .groups = 'drop') %>%
  pivot_wider(names_from = "streetLight", values_from="total_count") %>% 
  mutate("%-crashes" = `No`/(`Yes`+`No`))

write.csv(street_light_crashes, "street_light_crashes.csv")

## investigate % of major crashes to total crashes in the water by guard rail (West Coast vs Rest of NZ)
crashes_3 %>% filter(waterRiver >= 1) %>% group_by(guardRail) %>%
  summarise(total_count=n(), .groups = 'drop')

crashes_3 %>% group_by(guardRail) %>% summarise(total_count=n(), .groups = 'drop')

crashes_3$guardRail <- ifelse((crashes_3$guardRail >= 1),
                                "Yes", "No")

nz_water <- crashes_3 %>% filter(waterRiver >= 1) %>% group_by(guardRail, crashLocation1) %>% summarise(total_count=n(), .groups = 'drop') %>%
  pivot_wider(names_from = "guardRail", values_from="total_count") %>% replace(is.na(.), 0) %>%
  mutate("%-crashes" = `No`/(`Yes`+`No`)) %>% mutate("total-crashes" = `Yes`+`No`) %>% filter(`total-crashes` >= 20)

sh6_water <- crashes_3 %>% filter(waterRiver >= 1) %>% filter(crashLocation1 == "SH 6") %>%
  group_by(guardRail, region, outcome) %>% summarise(total_count=n(), .groups = 'drop') %>%
  pivot_wider(names_from = "guardRail", values_from="total_count") %>% replace(is.na(.), 0) %>%
  mutate("%-crashes" = `No`/(`Yes`+`No`)) %>% mutate("total-crashes" = `Yes`+`No`)

write.csv(nz_water, "nz_water.csv")
write.csv(sh6_water, "sh6_water.csv")


## investigate funding data (NZ vs West Coast Region)
# https://www.nzta.govt.nz/planning-and-investment/learning-and-resources/transport-data/funding-and-transport-dashboard-and-open-data
funding <- read.csv("funding.csv") %>% mutate(`%-nz` = `West.Coast.Region`/`Total.NZ`) %>% 
  select('ï..', "%-nz") %>%
  pivot_wider(names_from = "ï..", values_from="%-nz")
write.csv(funding, "wc-funding_proportions.csv")
#################################################################################################
