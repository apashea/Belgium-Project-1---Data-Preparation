library(tidyverse)

# Scatterplot of population vs. cluster, colored by cluster
# FIGURE 1:
ggplot(bprovinces_2017c, aes(cluster, Population, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Population")

# Scatterplot of Government final consumption expenditure vs. cluster, colored by cluster
# FIGURE 2:
ggplot(bprovinces_2017c, aes(cluster, Final_consumption_expenditure_government, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Government final consumption expenditure")

# Scatterplot of Disposable income versus Imports of Goods & Services, colored by cluster
# # FIGURE 3:
ggplot(bprovinces_2017c, aes(Import_of_goods_and_services, Disposable_income_B_6n, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Import of goods and services in millions of euros", 
       y = "Disposable income (B.6n) in millions of euros", 
       title = "Belgium, 2017: Clustering Imports Consumption", 
       subtitle = "Total disposable income vs. Import of goods and services in each province colored by cluster", caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#)")

# Scatterplot of social insurance scheme service charge, colored by cluster
# FIGURE 4:
ggplot(bprovinces_2017c, aes(cluster, Social_insurance_scheme_service_charge_D61SC, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Social insurance scheme service charge")

# Scatterplot of housholds' actual social contributions vs. cluster, colored by clustered
# FIGURE 5:
ggplot(bprovinces_2017c, aes(cluster, Households_actual_social_contributions_D_613, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Households' actual social contributions (D.613)")

# Scatterplot of gross value added by financial corporations, colored by clustered.
# We see that Brussels is much more 'financialized' than the other provinces, the rest
# being significantly more similar.
# FIGURE 6:
ggplot(bprovinces_2017c, aes(cluster, Gross_value_added_at_basic_prices_B_1g_Financial_corporations, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = Province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Gross value added by financial corporations (B.1g)")
