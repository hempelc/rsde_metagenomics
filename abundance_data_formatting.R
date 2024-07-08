library(arrow)   # For reading and writing Parquet files
library(dplyr)   # For data manipulation
library(rlang)   # For working with symbols and unquoting

##################### Settings

# Specify the path to the Parquet file
file <- "/Users/simplexdna/Desktop/final_table_tax.parquet"
# Define a vector of taxonomic ranks
ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")

# Define the ranks to group by for the different levels we analyse (all, eukaryota, chordata)
grouping_rank_all <- "species"
grouping_rank_eukaryota <- "class"
grouping_rank_chordata <- "order"

# Define name of the output directory in which all formatted tables are saved
outdir <- "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results"

#################### Reading in df

# Read the Parquet file into a data frame
df <- read_parquet(file)

#################### Create output dir
if (!dir.exists(outdir)) {
  dir.create(outdir)
}

################### Standardizing taxonomy
# TO DO: polish standardization of taxonomy

# Standardize species rank by replacing:
# Starts with: bacterium_ OR uncultured_ OR miscellaneous_ OR cyanobiont_ OR unidentified_ OR archaeon_ OR haloarchaeon_
# Ends with _bacterium OR _metagenome OR _archaeon OR _environmental_sample OR _cyanobacterium
# Contains sp. OR _bacterium_ OR _archaeon_ OR [ OR symbiont OR _clone_ OR _proteobacterium_ OR _bacteria_ OR _bacteirum_ OR _cyanobacterium_ OR _oral_ OR actinobacterium_ OR _epibiont_ OR ' OR _thaumarchaeote_ OR _euryarchaeote_
# Equal to: metagenome OR bacterium OR Plasmid_Ti OR Sym_plasmid OR synthetic_construct OR unidentified OR archaeon OR candidate_division_TM7_genomosp._GTL1 OR Candidatus_Erysipelatoclostridium_merdavium
ends_with_bacterium <- grepl("_bacterium$", df$species)
df$species[ends_with_bacterium] <- NA
ends_with_metagenome <- grepl("_metagenome$", df$species)
df$species[ends_with_metagenome] <- NA
ends_with_archaeon <- grepl("_archaeon$", df$species)
df$species[ends_with_archaeon] <- NA
ends_with_environmental_sample <- grepl("_environmental_sample$", df$species)
df$species[ends_with_environmental_sample] <- NA
ends_with_cyanobacterium <- grepl("_cyanobacterium$", df$species)
df$species[ends_with_cyanobacterium] <- NA

starts_with_uncultured <- grepl("^uncultured_", df$species)
df$species[starts_with_uncultured] <- NA
starts_with_bacterium <- grepl("^bacterium_", df$species)
df$species[starts_with_bacterium] <- NA
starts_with_miscellaneous <- grepl("^miscellaneous_", df$species)
df$species[starts_with_miscellaneous] <- NA
starts_with_cyanobiont <- grepl("^cyanobiont_", df$species)
df$species[starts_with_cyanobiont] <- NA
starts_with_unidentified <- grepl("^unidentified_", df$species)
df$species[starts_with_unidentified] <- NA
starts_with_archaeon <- grepl("^archaeon_", df$species)
df$species[starts_with_archaeon] <- NA
starts_with_haloarchaeon <- grepl("^haloarchaeon_", df$species)
df$species[starts_with_haloarchaeon] <- NA

contains_sp <- grepl("_sp\\.", df$species)
df$species[contains_sp] <- NA
contains_bacterium <- grepl("_bacterium_", df$species)
df$species[contains_bacterium] <- NA
contains_archaeon <- grepl("_archaeon_", df$species)
df$species[contains_archaeon] <- NA
contains_squarebracket <- grepl("\\[", df$species)
df$species[contains_squarebracket] <- NA
contains_symbiont <- grepl("symbiont", df$species)
df$species[contains_symbiont] <- NA
contains_clone <- grepl("_clone_", df$species)
df$species[contains_clone] <- NA
contains_proteobacterium <- grepl("_proteobacterium_", df$species)
df$species[contains_proteobacterium] <- NA
contains_bacteria <- grepl("_bacteria_", df$species)
df$species[contains_bacteria] <- NA
contains_bacteirum <- grepl("_bacteirum_", df$species)
df$species[contains_bacteirum] <- NA
contains_cyanobacterium <- grepl("cyanobacterium_", df$species)
df$species[contains_cyanobacterium] <- NA
contains_oral <- grepl("_oral_", df$species)
df$species[contains_oral] <- NA
contains_actinobacterium <- grepl("actinobacterium_", df$species)
df$species[contains_actinobacterium] <- NA
contains_epibiont <- grepl("_epibiont_", df$species)
df$species[contains_epibiont] <- NA
contains_euryarchaeote <- grepl("_euryarchaeote_", df$species)
df$species[contains_euryarchaeote] <- NA
contains_thaumarchaeote <- grepl("_thaumarchaeote_", df$species)
df$species[contains_thaumarchaeote] <- NA
contains_quote <- grepl("//'", df$species)
df$species[contains_quote] <- NA

names_to_replace <- c("metagenome", "bacterium", "Plasmid_Ti", "Sym_plasmid", "synthetic_construct", "unidentified", "archaeon", "candidate_division_TM7_genomosp._GTL1", "Candidatus_Erysipelatoclostridium_merdavium")
df <- df %>%
  mutate(species = case_when(
    species %in% names_to_replace ~ NA_character_,
    TRUE ~ species
  ))

######################## Aggregation + statistics

# Aggregate the data by the grouping rank, summing all other columns
df_rankToKeep <- df %>% select(-all_of(ranks[ranks != grouping_rank_all]))
grouping_rank_sym <- sym(grouping_rank_all)
aggregated_df_all <- df_rankToKeep %>%
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
df_eukaryota <- df %>% filter(domain == "Eukaryota")

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
krona_df <- df %>%
  group_by(across(all_of(ranks))) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')
row_sums <- rowSums(krona_df[, !names(krona_df) %in% ranks])
krona_df$total_count_absolute <- row_sums
krona_df <- krona_df %>% select(-starts_with("RSDE"))
krona_df <- krona_df[, c(ncol(krona_df), seq_along(names(krona_df)[-ncol(krona_df)]))]
write.table(krona_df, file.path(outdir, "final_table_tax_krona.tsv"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

######################## Summarize number of taxa per rank and domain
df_summary <- df %>%
  filter(!is.na(domain)) %>%
  group_by(domain) %>%
  summarize(
    phyla = n_distinct(phylum),
    classes = n_distinct(class),
    orders = n_distinct(order),
    families = n_distinct(family),
    genera = n_distinct(genus),
    species = n_distinct(species)
  )
rank_sums <- colSums(select(df_summary, -domain), na.rm = TRUE)
df_summary <- rbind(df_summary, c("Total", rank_sums))
write.csv(df_summary, file.path(outdir, "rank_taxa_number_summary.csv"), row.names = FALSE)
