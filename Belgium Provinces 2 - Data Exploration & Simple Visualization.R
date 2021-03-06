
library(plyr)
library(tidyverse)
library(gapminder)
library(reshape2)
library(FactoMineR)
library(factoextra)


# SECTION 2 - Some brief exploration.
# Let's take a look at our variable names.
colnames(bprovinces_2017)

# FIGURE 1: 
bprovinces_2017 %>% ggplot(aes(Wages_and_salaries_D_11,Province)) +
  geom_col() +
  labs(x = "Wages and salaries in millions of euros")

# Making a bar chart with our columns on wages and salaries, total GDP, and taxes on income weath,
# for which we select these variables and then change into into long format.
b_wtgdp <- bprovinces_2017 %>% select(Province, Wages_and_salaries_D_11, 
                                      Current_taxes_on_income_wealth_etc__D_5,
                                      Gross_domestic_product_GDP_Total_economy)
b_wtgdplong <- b_wtgdp %>% gather(key = Stat, value = Value, Wages_and_salaries_D_11:Gross_domestic_product_GDP_Total_economy)
# FIGURE 2:
ggplot(b_wtgdplong, aes(Value, Province, fill = Stat)) + 
  geom_col(position = "dodge") + 
  labs(x = "Millions of euros", title = "Belgium, 2017", subtitle = "Regional Wages, Total GDP, and Taxes on Income Wealth", caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#)") + 
  scale_fill_discrete(name = "", labels = c("Income wealth taxes", "Total GDP", "Wages and Salaries"), guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position = c(0.7, 0.7), legend.direction = "vertical")
  
  
# Making a bar chart for other selected variables. Here we will simplify the process a bit by subsetting our columns by
# their position instead of typing out their full names.
# FIGURE 3:
bb2pos <- c(1,62,63,68,69,74,75)
bb2 <- bprovinces_2017 %>% select(bb2pos)
bb2long <- bb2 %>% gather(key = Stat, value = Value, Gross_value_added_at_basic_prices_B_1g_Nonfinancial_corporations:Gross_fixed_capital_formation_Financial_corporations)
ggplot(bb2long, aes(Value, Province, fill = Stat)) +
  geom_col(position = "dodge") + 
  labs(x = "Millions of euros", title = "Belgium, 2017", subtitle = "Nonfinancial vs Financial corporations", caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#)") +
  scale_fill_discrete(name = "", guide = guide_legend(reverse = TRUE)) +
  theme_light() +
  theme(legend.position = c(0.72, 0.7), legend.direction = "vertical", legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "red", size = 7))
