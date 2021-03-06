

library(plyr)
library(tidyverse)
library(gapminder)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)

# -----------------------------------------
# Section 4. 
# Mapping: XXXXXXXXXXXXXXX consider moving this section to BEFORE PCA/k-means (including renaming etc, though watch for PCA errors b/c need an all-numeric matrix for it)

library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
# Fonts (which can be used in the above scatterplots as well, of course.)
library(extrafont)
# Note that importing our fonts into our directory will take a few minutes but is only necessary once..
# font_import()
loadfonts(device = "win")
windowsFonts()

# Loading in our map 
# source: https://hub.arcgis.com/datasets/6fa897b56c3048009ee9a575bca54a27
# 'BELGIUM - Provinces' (bottom-right option, download zipped shapefile)
# Setting options so our numbers don't display as scientific notation.
options(scipen = 999)
bmap <- st_read("C:/Users/andre/Desktop/R/belgium_data/maps/BELGIUM_-_Provinces.shp", stringsAsFactors = FALSE)
str(bmap)

# Let's simplify our provinces' names from the original data's names, then add a column using the naming scheme from
# our shapefile under the "NAME_2" column (French or Dutch depending on region).
provnames <- as.character(c("Brussels", "Antwerp", "Limburg", "East Flanders", "Flemish Brabant", "West Flanders", "Walloon Brabant", "Hainaut", "Liège", "Luxemberg", "Namur"))
brenamed <- bprovinces_2017c
brenamed$Province <- provnames
shpnames <- as.character(c("Bruxelles", "Antwerpen", "Limburg", "Oost-Vlaanderen", "Vlaams Brabant", "West-Vlaanderen", "Brabant Wallon", "Hainaut", "Liège", "Luxembourg", "Namur"))
brenamed1 <- brenamed %>% mutate(NAME_2 = shpnames)
b_data_map <- inner_join(bmap, brenamed1)

# A simple population density map.
# FIGURE 1:
ggplot(b_data_map) +
  geom_sf(aes(fill = Population)) +
  scale_fill_gradient(low = "#38EECD", high = "#17594D") +
  labs(title = "Belgium, 2017", subtitle = "Provincial population density", 
       caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#")

# Mapping provinces by 'financialization' of the economy, i.e. the percentage of GDP produced by financial operations.
# We will add the names of each province to our map as well.
financializ <- (b_data_map$Gross_value_added_at_basic_prices_B_1g_Financial_corporations / b_data_map$Gross_domestic_product_GDP_Total_economy)
# FIGURE 2:
ggplot(b_data_map) +
  geom_sf(aes(fill = (financializ))) +
  scale_fill_gradient(low = "#FFFFFF", high = "#055500", name = FALSE, labels = percent) +
  labs(title = "Belgium, 2017: Provincial Financialization", subtitle = "Gross value added by financial corporations as percent of GDP", 
       caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#) \nBelgian map data courtesy of  Esri BeLux Data - ArcGIS") +
  theme(legend.position = c(0.15, 0.2), legend.direction = "vertical", legend.title = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),) +
  geom_sf_label_repel(aes(label = Province),
                      force = 0.1, nudge_y = 0.09, nudge_x = 0.00001, 
                      seed = 10, box.padding = 1)

# Mapping provinces by government expenditure on collective consumption, i.e. total ('final')
# expenditure on consumption minus individual consumption expenditure. 
# This would be written to a Francophone audience.
# FIGURE 3:
ggplot(b_data_map) +
  geom_sf(aes(fill = Collective_consumption_expenditure_P_32)) +
  scale_fill_gradient(low = "#FFFFFF", high = "#08A300", name = "Millions d'euros") +
  labs(title = "Les provinces de Belgique, 2017", 
       subtitle = "Les dépenses collectives de consommation des administrations publiques", 
       caption = "Source: Banque Nationale de Belgique Statistiques en ligne (http://stat.nbb.be/?lang=fr#)") +
  geom_sf_label_repel(aes(label = FR_Name),
                      force = 0.02, nudge_y = 0.09, 
                      nudge_x = 0.00001, 
                      seed = 10, box.padding = 1) +
    theme(plot.title = element_text(family = "Source Serif Pro Black"),
          text = element_text(family = "Source Serif Pro"),
          legend.position = c(0.15, 0.2), 
          legend.direction = "vertical", 
          legend.title = element_text(family = "Source Serif Pro"),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank())
  
# Now let's map by cluster.
# FIGURE 4:
ggplot(b_data_map) +
  geom_sf(aes(fill = cluster)) +
  scale_colour_manual(values = b_data_map$cluster, name = "Cluster") +
  labs(title = "Belgium, 2017", 
       subtitle = "Colored by assigned cluster based on similarity of economic indicators", 
       caption = "Source: National Bank of Belgium Online Statistics (http://stat.nbb.be/?lang=en#)") +
  geom_sf_label_repel(aes(label = Province),
                      force = 0.02, nudge_y = 0.09, 
                      nudge_x = 0.00001, 
                      seed = 10, box.padding = 1) +
  theme(text = element_text(family = "Source Serif Pro"),
        plot.title = element_text(family = "Source Serif Pro Black"),
        plot.caption = element_text(family = "Source Serif Pro Black"),
        legend.position = c(0.15, 0.2), 
        legend.direction = "vertical", 
        legend.title = element_text(family = "Source Serif Pro Black"), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank())
