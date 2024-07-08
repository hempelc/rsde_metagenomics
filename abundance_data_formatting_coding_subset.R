library(arrow)   # For reading and writing Parquet files
library(dplyr)   # For data manipulation
library(rlang)   # For working with symbols and unquoting

##################### Settings

# Specify the path to the Parquet file
file <- "/Users/simplexdna/Desktop/final_table_tax.parquet"
# Define a vector of taxonomic ranks
ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")

# Define the ranks to group by for the different levels we analyse (all, eukaryota, chordata)
grouping_rank_all <- "phylum"
grouping_rank_eukaryota <- "class"
grouping_rank_chordata <- "order"

# Define name of the output directory in which all formatted tables are saved
outdir <- "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results"

#################### Reading in df

# Read the Parquet file into a data frame
df <- read_parquet(file)

# Subset the first 10,000 rows of the data frame
df_sub <- df[1:10000, ]

#################### Create output dir
if (!dir.exists(outdir)) {
  dir.create(outdir)
}

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
df_sub_rankToKeep <- df_sub %>% select(-all_of(ranks[ranks != grouping_rank_all]))
grouping_rank_sym <- sym(grouping_rank_all)
aggregated_df_all <- df_sub_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df_all[, !names(aggregated_df_all) %in% grouping_rank_all])
aggregated_df_all$total_count_absolute <- row_sums
aggregated_df_all$total_count_percentages <- row_sums / sum(row_sums) * 100

# Cut down the df to relevant columns and save to outdir
aggregated_df_all_overview <- aggregated_df_all %>% select(grouping_rank_all, total_count_percentages)
write.csv(aggregated_df_all_overview, file.path(outdir, paste0("aggregated_df_all_", grouping_rank_all, ".csv")), row.names = FALSE)

########################## Eukaryote zoom in
# Create eukaryote df
df_eukaryota <- df_sub %>% filter(domain == "Eukaryota")

# Aggregate the data by the grouping rank, summing all other columns
df_eukaryota_rankToKeep <- df_eukaryota %>% select(-all_of(ranks[ranks != grouping_rank_eukaryota]))
grouping_rank_sym <- sym(grouping_rank_eukaryota)
aggregated_df_eukaryotes <- df_eukaryota_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df_eukaryotes[, !names(aggregated_df_eukaryotes) %in% grouping_rank_eukaryota])
aggregated_df_eukaryotes$total_count_absolute <- row_sums
aggregated_df_eukaryotes$total_count_percentages <- row_sums / sum(row_sums) * 100

# Cut down the df to relevant columns and save to outdir
aggregated_df_eukaryota_overview <- aggregated_df_eukaryotes %>% select(grouping_rank_eukaryota, total_count_percentages)
write.csv(aggregated_df_eukaryota_overview, file.path(outdir, paste0("aggregated_df_eukaryota_", grouping_rank_eukaryota, ".csv")), row.names = FALSE)

########################## Chordata zoom in
# Create chordata df
df_chordata <- df_eukaryota %>% filter(phylum == "Chordata")

# Aggregate the data by the grouping rank, summing all other columns
df_chordata_rankToKeep <- df_chordata %>% select(-all_of(ranks[ranks != grouping_rank_chordata]))
grouping_rank_sym <- sym(grouping_rank_chordata)
aggregated_df_chordata <- df_chordata_rankToKeep %>%
  group_by(!!grouping_rank_sym) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Calculate the row sums of the aggregated data frame
row_sums <- rowSums(aggregated_df_chordata[, !names(aggregated_df_chordata) %in% grouping_rank_chordata])
aggregated_df_chordata$total_count_absolute <- row_sums
aggregated_df_chordata$total_count_percentages <- row_sums / sum(row_sums) * 100

# Cut down the df to relevant columns and save to outdir
aggregated_df_chordata_overview <- aggregated_df_chordata %>% select(grouping_rank_chordata, total_count_percentages)
write.csv(aggregated_df_chordata_overview, file.path(outdir, paste0("aggregated_df_chordata_", grouping_rank_chordata, ".csv")), row.names = FALSE)

######################### Krona

# Group by the vector of column names and summarize all other columns
krona_df <- df_sub %>%
  group_by(across(all_of(ranks))) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')
row_sums <- rowSums(krona_df[, !names(krona_df) %in% ranks])
krona_df$total_count_absolute <- row_sums
krona_df <- krona_df %>% select(-starts_with("RSDE"))
krona_df <- krona_df[, c(ncol(krona_df), seq_along(names(krona_df)[-ncol(krona_df)]))]
write.table(krona_df, file.path(outdir, "final_table_tax_krona.tsv"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)