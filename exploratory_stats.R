#==============================================
#   This Document was used for most of the
#        exploratory statistic work
#           Robert Ilyes - Geog 481
#==============================================

# Import Libraries
library(dplyr)
library(ggplot2)

# Import Merged Tree Dataset
trees = read.csv("C://Users/robik/Downloads/FinalMerge0716.csv")
summary(trees)

# Split into individual and Forest Stand data
indi = trees %>% filter(standID == 0)
forest = trees %>% filter(standID != 0)

# Create Height Bins
trees2 = trees
trees2$heightbins = floor(trees2$zmax/5)
trees2$agebins = floor(trees2$AGE_zmax/5)

# Calculate average carbon by bin
carbon_heights = trees2 %>% group_by(heightbins) %>% summarise(average = mean(CARBON_FIN))

# Get the top 20 species
top_species <- indi %>%
  group_by(NewSpecies) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(20, wt = count) %>%
  select(NewSpecies)

# Pie Graph
filtered_tree_data <- indi %>%
  filter(NewSpecies %in% top_species$NewSpecies)
specie_counts = trees %>% group_by(NewSpecies) %>% summarise(average = mean(Carbon))
specie_counts$pcts = round((specie_counts$count / sum(specie_counts$count)) *100)
lbls = paste0(specie_counts$NewSpecies, " ", (specie_counts$pcts * 100), "%")
pie(x= specie_counts$count, labels=lbls, main="Species Distribution of Kitchener")

# Get average age and heights for species
averages <- trees %>%
  group_by(NewSpecies) %>%
  summarise(
    average_height = mean(zmax, na.rm = TRUE),
    average_age = mean(AGE_zmax, na.rm = TRUE),
    count = n()
  )

# Get average and sum carbon per species
result <- trees %>%
  group_by(NewSpecies) %>%
  summarise(
    average_carbon = mean(CARBON_FIN, na.rm = TRUE),
    total_carbon = sum(CARBON_FIN, na.rm = TRUE),
    count = n()
  )
# ====================================================
# CBM-CFS3 vs i-Tree Work
itree = indi %>% filter(zmax <= 50, AGE_zmean_ <= 80)
cbm = forest %>% filter(zmax <= 50, AGE_zmean <= 80)
trees2 = trees2 %>% filter(zmax <= 50, AGE_zmean <= 80)

# Get general Summary Statistics
summary(itree$CARBON_FIN)
summary(cbm$CARBON_FIN)


# Get the top 20 species
top_species <- trees %>%
  group_by(NewSpecies) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5, wt = count) %>%
  select(NewSpecies)

# Pie Graph
filtered_cbm <- cbm %>%
  filter(NewSpecies %in% top_species$NewSpecies)
filtered_itree <- itree %>%
  filter(NewSpecies %in% top_species$NewSpecies)

# Just to view average values
average_carbon <- trees %>%
  group_by(NewSpecies, Type) %>%
  summarize(average_carbon = mean(CARBON_FIN, na.rm = TRUE),
            average_height = mean(HEIGHT_FIN, na.rm = TRUE),
            .groups = 'drop')
print(average_carbon)

# Try normalizing carbon based on the common height field
filtered_itree$normalizedCarbon = filtered_itree$CARBON_FIN / filtered_itree$HEIGHT_FIN
filtered_cbm$normalizedCarbon = filtered_cbm$CARBON_FIN / filtered_cbm$HEIGHT_FIN
trees2$normalizedCarbon = trees2$CARBON_FIN / trees2$HEIGHT_FIN

# Define a function to cap outliers
cap_outliers <- function(x, lower_quantile = 0.05, upper_quantile = 0.98) {
  quantiles <- quantile(x, probs = c(lower_quantile, upper_quantile))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

# Apply the function to cap outliers in the carbon_storage column
filtered_itree2 <- filtered_itree %>%
  group_by(NewSpecies, Type) %>%
  mutate(carbon_storage_capped = cap_outliers(CARBON_FIN))

# Create a boxplot to compare carbon storage between models for each species
ggplot(filtered_itree2, aes(x = NewSpecies, y = normalizedCarbon)) +
  geom_boxplot() +
  ylim(0, 150) +
  labs(title = "Carbon Storage by Species in i-Tree",
       x = "Species",
       y = "Carbon Storage per Tree Height (Kg / M)") +
  theme_minimal() +
  scale_fill_manual(values = c("i-tree" = "blue", "CBM-CFS3" = "red"))

ggplot(filtered_cbm, aes(x = NewSpecies, y = normalizedCarbon)) +
  geom_boxplot() +
  labs(title = "Carbon Storage by Species in CBM-CFS3",
       x = "Species",
       y = "Carbon Storage per Tree Height (Kg / M)") +
  theme_minimal() +
  scale_fill_manual(values = c("i-tree" = "blue", "CBM-CFS3" = "red"))

# Both models Together

trees2 <- trees2 %>%
  mutate(Type = recode(Type, "forest" = "CBM-CFS3", "individual" = "i-Tree"))
ggplot(trees2, aes(x = Type, y = CARBON_FIN)) +
  geom_boxplot() +
  ylim(0, 5000) +
  labs(title = "Carbon Storage by Models",
       x = "Model",
       y = "Carbon Storage per Tree (Kg)") +
  theme_minimal()

ggplot(trees2, aes(x = Type, y = normalizedCarbon)) +
  geom_boxplot() +
  ylim(0, 5000) +
  labs(title = "Carbon Storage by Species in CBM-CFS3",
       x = "Tree Type",
       y = "Carbon Storage per Tree Kg") +
  theme_minimal()


# Create an interaction plot to see the effect of species and model on carbon storage
interaction_plot <- ggplot(trees, aes(x = NewSpecies, y = CARBON_FIN, color = Type)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Plot: Species vs Carbon Storage by Model",
       x = "Species",
       y = "Carbon Storage (kg)") +
  theme_minimal() +
  scale_color_manual(values = c("forest" = "blue", "individual" = "red"))

# Display the interaction plot
print(interaction_plot)

# Plot CBM Results by Species
ggplot(cbm, aes(x=cbm$AGE_zmax, y=cbm$CARBON_FIN, col=cbm$NewSpecies))+geom_point()

# ====================================================
# Paired T-test Work (mostly unfinised)

# Get species common in both i-Tree and CBM-CFS3 datasets
common_species <- trees %>%
  group_by(NewSpecies) %>%
  filter(n_distinct(Type) == 2) %>%
  pull(NewSpecies) %>%
  unique()

filtered_df <- trees %>% filter(NewSpecies %in% common_species)

# perform_t_test(data): Performs paired t-test on two datasets
perform_t_test <- function(data) {
  model1_data <- data %>% filter(Type == "forest") %>% pull(CARBON_FIN)
  model2_data <- data %>% filter(Type == "individual") %>% pull(CARBON_FIN)
  t_test_result <- t.test(model1_data, model2_data, paired = TRUE)
  return(tidy(t_test_result))
}

# Group by species and perform paired t-test
results <- filtered_df %>%
  group_by(NewSpecies) %>%
  do(perform_t_test(.))

# Iterate through common species and return p-value
for (i in 1:length(common_species)){
  new = filtered_df %>% filter(NewSpecies == common_species[i])
  tres = perform_t_test(new)
  result <- rbind(result, tres)
}

# t-test of trees with the same height
itree_10 = itree %>% filter(heightbins == 2)
cbm_10 = cbm %>% filter(heightbins == 2)
# still vastly different results
t.test(itree_10$CARBON_FIN, cbm_10$CARBON_FIN, paired=FALSE)
