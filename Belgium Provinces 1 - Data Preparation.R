# All data used here, retrieved from the National Bank of Belgium's Online Statistics
# (http://stat.nbb.be/?lang=en#), contains annual data from 2017 for each of the eleven
# Belgian provinces. All variables are measured in millions of euros except for
# population, measured in individuals.

# Optional: Freshening up RStudio 
rm(list = ls(all = TRUE))                        # Remove all previous objects
gc()                                             # Garbage collection
assign("last.warning", NULL, envir = baseenv())  # Reset list of past warnings

# Loading the tidyverse. We will mainly be using the included dplyr package.

library(tidyverse)

# Section 1. Importing, tidying, and merging our data.

# a.  Importing our population dataset, renaming and selecting only province names and the population value)
pop <- read_csv("[PATH]/REGPOP_31122020033943445.csv")
pop_fil <- pop %>% 
          rename(Province = `Territorial unit (Region/Province/District)`, Population = Value) %>%
          select(Province, Population)

# b.  Importing our imports/exports/net exports dataset and selecting our provinces, our 9 items (under Item),
# and their respective values.
exports <- read_csv("[PATH]/REGTRD_31122020040249769.csv")
exports_fil <- exports %>%
              rename(Province = `Territorial unit (Region/Province/District)`) %>%
              select(Province, Item, Value)
# We want our 9 items to each have their own column rather than be set as values under the column "Item."
# We also want our columns to not contain certain characters like spaces while we are still coding.
exports_p <- exports_fil %>% pivot_wider(names_from = Item, 
                                     values_from = Value)
names(exports_p)<-str_replace_all(names(exports_p), c(" " = "_" , "," = "" ))


# c.  Importing our household income accounts dataset. We have four different measurement units
#     so we filter only for measurements in Current prices in millions of euro
#     and then select our provinces, 34 items (under Item), and their respective values.
household <-  read_csv("[PATH]/REGHHINC_31122020040937767.csv")
household_fil <- household %>%
                filter(Unit == "Current prices in millions of euro") %>%
                rename(Province = `Territorial unit (Region/Province/District)`) %>%
                select(Province, Item, Value)
# Again, we want to convert our "Item" column into a series of columns and remove special characters.
household_pp <- household_fil %>% pivot_wider(names_from = Item, 
                                              values_from = Value)
names(household_pp) <-str_replace_all(names(household_pp), c(" " = "_", "\\." = "_", "," = "", "\\(" = "", "\\)" = "", "\\'" = ""))

# Finally, we have some nest listed-columns due to duplicate variable names. Cross-referencing with the NBB's site, we see that:
# i.    For Resources, the left value is for -Allocation of primary income account- and
#       the right value is for -Secondary distribution of income account-.
# ii.   Property incomes: left = primary_resources, right = primary_uses
# iii.  Uses: left = primary, right = uses
# iv.   Balance of primary incomes: left = primary_uses, right = secondary_resources
# v.    Employers' imputed social contriubtions: left = secondary_resources, right = primary_resources
# vi.   Other social insurance benefits: left = secondary_resources, right = secondary_uses
# vii.  Other current transfers: left = secondary_resources, right = secondary_uses
household_p <- household_pp %>%
  unnest_wider(Resources, names_repair = "unique") %>%
  unnest_wider(Property_income_D_4, names_repair = "unique") %>%
  unnest_wider(Uses, names_repair = "unique") %>%
  unnest_wider(Balance_of_primary_incomes_B_5n, names_repair = "unique") %>%
  unnest_wider(Employers_imputed_social_contributions_D_612, names_repair = "unique") %>%
  unnest_wider(Other_social_insurance_benefits_D_622, names_repair = "unique") %>%
  unnest_wider(Other_current_transfers_D_7, names_repair = "unique") %>%
  rename_at(vars(contains("...")), ~ c("Resources_primary", "Resources_secondary",
                                        "Property_income_primary_resources", "Property_income_primary_uses",
                                       "Uses_primary", "Uses_secondary",
                                       "Balance_of_primary_incomes_primary_uses", "Balance_of_primary_incomes_secondary_resources",
                                       "Employers_imputed_social_contributions_secondary_resources", "Employers_imputed_social_contributions_primary_resources",
                                       "Other_social_insurance_benefits_secondary_resources", "Other_social_insurance_benefits_secondary_uses",
                                       "Other_current_transfers_secondary_resources", "Other_current_transfers_secondary_uses")) 




# d.  Importing our final consumption of NPISH's and government dataset.
#     Again, we have multiple measurement units but we only want the same one used above.
#     Once again we pivot our Item column.
nonprofit <- read_csv("[PATH]/REGP3S13S15_31122020035624938.csv")
nonprofit_fil <- nonprofit %>%
                filter(Unit == "Current prices in millions of euro") %>%
                rename(Province = `Territorial unit (Region/Province/District)`) %>%
                select(Province, Item, Value)
nonprofit_pp <- nonprofit_fil %>% pivot_wider(names_from = Item,
                                             values_from = Value)
names(nonprofit_pp) <-str_replace_all(names(nonprofit_pp), c(" " = "_", "\\." = "_", "," = "", "\\(" = "", "\\)" = "", "\\'" = "", "\\-" = "", "\\/" = "_"))
# More concatenated values. Cross-referencing with the NBB's site, we see that:
# Final consumption expenditure's left value is under -General government- while the 
# right value is under -Non profit institutions serving households-.
# This pattern is the same for: Individual consumption expenditure, Social transfers in kind,
# Social transfers in kind non market production, and Social transfers in kind purchased market production.
nonprofit_pnas <- nonprofit_pp %>%
  unnest_wider(Final_consumption_expenditure_P_3, names_repair = "unique") %>%
  unnest_wider(Individual_consumption_expenditure_P_31, names_repair = "unique") %>%
  unnest_wider(Social_transfers_in_kind_D_63, names_repair = "unique") %>%
  unnest_wider(Social_transfers_in_kind__nonmarket_production_D_631, names_repair = "unique") %>%
  unnest_wider(Social_transfers_in_kind__purchased_market_production_D_632, names_repair = "unique") %>%
  rename_at(vars(contains("...")), ~ c("Final_consumption_expenditure_government", 
                                       "Final_consumption_expenditure_NPISH",
                "Individual_consumption_expenditure_government", 
                "Individual_consumption_expenditure_NPISH",
                "Social_transfers_in_kind_government", 
                "Social_transfers_in_kind_NPISH",
                "Social_transfers_in_kind__nonmarket_production_government", 
                "Social_transfers_in_kind__nonmarket_production_NPISH",
                "Social_transfers_in_kind__purchased_market_production_government", 
                "Social_transfers_in_kind__purchased_market_production_NPISH"))

# Removing missing values: upon further inspection, our resulting dataset contains 
# three columns with missing values (all equal to zero), rendering them useless.
# This will be as simple as deselecting them.

nonprofit_p <- nonprofit_pnas %>% 
  select(-Social_transfers_in_kind__purchased_market_production_NPISH, 
         -Interventions_in_the_daily_price_of_hospitals, -Interventions_of_the_INIG)


# e. Importing our consumption of fixed capital dataset.
fixedcap <- read_csv("[PATH]/REGK1_31122020035037708.csv")
fixedcap_fil <- fixedcap %>%
  filter(Unit == "Current prices in millions of euro") %>%
  rename(Province = `Territorial unit (Region/Province/District)`) %>%
  select(Province, Item, Value)

fixedcap_p <- fixedcap_fil %>% pivot_wider(names_from = Item, 
                                         values_from = Value)
# As with earlier sets, we will remove spaces to keep our variable names consistent and readable.
names(fixedcap_p) <-str_replace_all(names(fixedcap_p), c(" " = "_"))


# f. Importing our Overview by institutional sector dataset.
instit <- read_csv("[PATH]/REGACSEC_31122020041521926.csv")

# Unlike our previous datasets, this one includes a column -Institutional sector- including an overall
# Total economy variable as well as variables within it e.g., -Households- and Financial corporations.
# We want to combine our sought variables like GDP with their respective sectoral label.
# We'll be sure to remove special characters first so the space in Institutional sector will
# not interfere with tidyr's unite() function.
names(instit) <-str_replace_all(names(instit), c(" " = "_", "\\." = "_", "," = "", "\\(" = "", "\\)" = "", "\\'" = "", "\\-" = "", "\\/" = ""))
instit_u <- instit %>% unite(Item_u, Item, Institutional_sector, remove = FALSE)
instit_fil <- instit_u %>%
  filter(Price_type__Unit == "Current prices in millions of euro") %>%
  rename(Province = Territorial_unit_RegionProvinceDistrict) %>%
  select(Province, Item_u, Value)
instit_p <- instit_fil %>% pivot_wider(names_from = Item_u,
                                              values_from = Value)
names(instit_p) <-str_replace_all(names(instit_p), c(" " = "_", "\\." = "_", "," = "", "\\(" = "", "\\)" = "", "\\'" = "", "\\-" = "", "\\/" = ""))

# -------------------------------------------------------------------------------------------------------

# Joining our tables:
# With all of our tables prepared, each of them sharing the column Province and each of them
# measured in Current prices in millions of euro (with the exception of Population, measured
# in individual persons), we can easily join our tables.

bprovinces_2017 <- pop_fil %>%
  inner_join(exports_p, by = "Province") %>%
  inner_join(household_p, by = "Province") %>%
  inner_join(nonprofit_p, by = "Province") %>%
  inner_join(fixedcap_p, by = "Province") %>%
  inner_join(instit_p, by = "Province")

# We'll save it just in case. First we have to flatten our table, meaning converting any list-columns into numeric, which
# will be useful not just for saving our new csv but also for keeping our data clean for future use.
# First we check which variables are lists:
sapply(bprovinces_2017, class)
# 26 columns are lists, so we will use lapply() across all columns save for our first one, Provinces, which contains character values.
i <- c(2:78)
# bprovinces_2017[ , i] <- lapply(provinces_2017[ , i], function(x) as.numeric(as.character(x)))
bprovinces_2017[ , i] <- lapply(bprovinces_2017[ , i], function(x) as.numeric(x))
write_csv(bprovinces_2017,"[PATH]\\bprovinces_2017.csv")
