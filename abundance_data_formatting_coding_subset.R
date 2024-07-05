library(arrow)   # For reading and writing Parquet files
library(dplyr)   # For data manipulation
library(rlang)   # For working with symbols and unquoting

##################### Settings

# Specify the path to the Parquet file
file <- "/Users/simplexdna/Desktop/final_table_tax.parquet"

# Define a vector of taxonomic ranks
ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")

# Define the rank to group by
grouping_rank <- "class"

#################### Reading in df

# Read the Parquet file into a data frame
df <- read_parquet(file)

# Subset the first 10,000 rows of the data frame
df_sub <- df[1:10000, ]

################### Standardizing taxonomy
# TO DO: polish standardization of taxonomy

# Standardize species rank by replacing:
  # Starts with: bacterium_ OR uncultured_
  # Ends with _bacterium OR _metagenome
  # Contains sp. OR _bacterium_
ends_with_bacterium <- grepl("_bacterium$", df_sub$species)
df_sub$species[ends_with_bacterium] <- NA
ends_with_metagenome <- grepl("_metagenome$", df_sub$species)
df_sub$species[ends_with_metagenome] <- NA
starts_with_uncultured <- grepl("^uncultured_", df_sub$species)
df_sub$species[starts_with_uncultured] <- NA
starts_with_bacterium <- grepl("^bacterium_", df_sub$species)
df_sub$species[starts_with_bacterium] <- NA
contains_sp <- grepl("_sp\\.", df_sub$species)
df_sub$species[contains_sp] <- NA
contains_bacterium <- grepl("_bacterium_", df_sub$species)
df_sub$species[contains_bacterium] <- NA

######################## Aggregation + statistics

# Aggregate the data by the grouping rank, summing all other columns
df_sub_rankToKeep <- df_sub %>% select(-all_of(ranks[ranks != grouping_rank]))
grouping_rank_sym <- sym(grouping_rank)
aggregated_df <- df_sub_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df[, !names(aggregated_df) %in% grouping_rank])
aggregated_df$total_count_absolute <- row_sums
aggregated_df$total_count_percentages <- row_sums / sum(row_sums) * 100

########################## Eukaryote zoom in
# Create eukaryote df
df_eukaryota <- df_sub %>% filter(domain == "Eukaryota")

# Aggregate the data by the grouping rank, summing all other columns
df_eukaryota_rankToKeep <- df_eukaryota %>% select(-all_of(ranks[ranks != grouping_rank]))
grouping_rank_sym <- sym(grouping_rank)
aggregated_df_eukaryotes <- df_eukaryota_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df_eukaryotes[, !names(aggregated_df_eukaryotes) %in% grouping_rank])
aggregated_df_eukaryotes$total_count_absolute <- row_sums
aggregated_df_eukaryotes$total_count_percentages <- row_sums / sum(row_sums) * 100

########################## Chordata zoom in
# Create chordata df
df_chordata <- df_eukaryota %>% filter(phylum == "Chordata")

# Aggregate the data by the grouping rank, summing all other columns
df_chordata_rankToKeep <- df_chordata %>% select(-all_of(ranks[ranks != grouping_rank]))
grouping_rank_sym <- sym(grouping_rank)
aggregated_df_chordata <- df_chordata_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df_chordata[, !names(aggregated_df_chordata) %in% grouping_rank])
aggregated_df_chordata$total_count_absolute <- row_sums
aggregated_df_chordata$total_count_percentages <- row_sums / sum(row_sums) * 100
