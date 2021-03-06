library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggrepel)
# The ggbiplot package is great for plotting PCA based analyses but is notorious for installation errors.
# (re)install rlang as well as fs, then install devtools which allows use of the install_github() function.
#install.packages("rlang")
#install.packages("fs")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)



# Section 3.


# Matrix for Principal Components Analysis.
bprovinces_matrix <- bprovinces_2017 %>%
  select_if(is.numeric) %>%
  as.matrix

# Plot the original variables and the first 2 components and print the plot object.
# Given our large number of variables, this visualization will not be particularly useful
# in this project but would be for others with fewer variables.
# We will see a better alternative shortly.
# FIGURE 1:
print(pca_var_plot <- fviz_pca_var(bprovinces_pca))

# We can review the correlations of our variables - dimensions, with the values 
# furthest from zero (toward 1 or -1) more strongly determining our clusters.
# Along the way we can inspect for missing values to be sure we can continue our PCA 
# without issue (and we see that we have none).
bprovinces_pca$var$cor

# Further, we can visualize which of our variables contribute the most to Dim1, 
# which captures 74% of the variance in our data.
# Let's see the top 15.
# FIGURE 2:
fviz_contrib(bprovinces_pca, choice = "var", axes = 1, top = 15, xtickslab.rt	= 90)
# And again for those contributing to Dim2, which captures 15.5% of our variance,
# significantly less than Dim1.
# FIGURE 3:
fviz_contrib(bprovinces_pca, choice = "var", axes = 2, top = 15, xtickslab.rt	= 90)

# How much of the variance is preserved by our first two components? 89.51616%.
(variance_first_two_pca <- bprovinces_pca$eig[1,2] + bprovinces_pca$eig[2,2])
# Let's visualize how much all of our computed dimensions explain the variance
# using a Scree plot.
# We see that Dim1 explains about 74%, Dim2 15.5%, Dim3 6.1%, and so on.
# FIGURE 4:
fviz_screeplot(bprovinces_pca, addlabels = TRUE, ylim = c(0, 80))

# Now we plot our individuals (our provinces) along these components.
# FIGURE 5:
fviz_pca_ind(bprovinces_pca, title = "Provinces - PCA")

# Set seed to 555 for reproducibility
set.seed(555)

# Create an intermediate data frame with pca_1 and pca_2
bprovinces_comps <- tibble(pca_1 = bprovinces_pca$ind$coord[,1],  
                          pca_2 = bprovinces_pca$ind$coord[,2])

# Cluster the observations using the first 2 components and print its contents
bprovinces_km <- kmeans(bprovinces_comps, centers = 4, nstart = 20, iter.max = 50)

# Convert assigned clusters to factor
clusters_as_factor <- factor(bprovinces_km$cluster)

# Plot individuals colored by cluster.
# FIGURE 6:
fviz_pca_ind(bprovinces_pca, 
             title = "Clustered Provinces - PCA", 
             habillage = clusters_as_factor) 

# Add cluster column to bprovinces
bprovinces_2017c <- bprovinces_2017 %>%
  mutate(cluster=clusters_as_factor)

# See each province and its respective cluster.
# FIGURE 7:
temppos <- c(1,79)
print(bprovinces_2017c[temppos])