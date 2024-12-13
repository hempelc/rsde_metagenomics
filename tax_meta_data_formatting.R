library(arrow)   # For reading and writing Parquet files
library(dplyr)   # For data manipulation
library(rlang)   # For working with symbols and unquoting
library(tibble)

##################### Settings

# Specify the path to the Parquet file

file <- "/Users/simplexdna/Desktop/final_table_tax.parquet"
# Define a vector of taxonomic ranks
ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")

# Define name of the output directory in which all formatted tables are saved
outdir <- "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results"

################### Define taxonomic grouping
holozoa_phyla <- c("Porifera", "Cnidaria", "Ctenophora","Placozoa","Xenacoelomorpha",
                   "Echinodermata","Chordata","Hemichordata","Nematoda","Nematomorpha",
                   "Arthropoda","Onychophora","Tardigrada","Priapulida","Kinorhyncha",
                   "Loricifera","Platyhelminthes","Nemertea","Annelida","Mollusca",
                   "Brachiopoda","Bryozoa","Acanthocephala","Gastrotricha","Entoprocta",
                   "Cycliophora","Phoronida","Rotifera","Chaetognatha","Gnathostomulida",
                   "Dicyemida","Orthonectida")
holomycota_phyla <- c("Ascomycota", "Basidiomycota", "Blastocladiomycota", "Chytridiomycota",
                      "Cryptomycota", "Microsporidia", "Mucoromycota", "Olpidiomycota", "Zoopagomycota")
alveolata_phyla <- c("Apicomplexa", "Ciliophora", "Perkinsozoa")
stramenopiles_phyla <- c("Bacillariophyta", "Oomycota")
rhizaria_phyla <- c("Cercozoa", "Endomyxa", "Foraminifera")
archaeplastida_phyla <- c("Chlorophyta", "Prasinodermophyta", "Rhodophyta", "Streptophyta")
amoebozoa_phyla <- c("Discosea", "Evosea", "Tubulinea")
discoba_phyla <- c("Euglenozoa", "Heterolobosea")
metamonada_phyla <- c("Fornicata", "Parabasalia", "Preaxostyla")
haptista_phyla <- c("Haptophyta")

# Make a named list
supergroups <- list(
  holozoa = holozoa_phyla,
  holomycota = holomycota_phyla,
  alveolata = alveolata_phyla,
  stramenopiles = stramenopiles_phyla,
  rhizaria = rhizaria_phyla,
  archaeplastida = archaeplastida_phyla,
  amoebozoa = amoebozoa_phyla,
  discoba = discoba_phyla,
  metamonada = metamonada_phyla,
  haptista = haptista_phyla
)

#################### Reading in df

# Read the Parquet file into a data frame
keep <- c("RSDE_150",
  "RSDE_42",
  "RSDE_61",
  "RSDE_140",
  "RSDE_92",
  "RSDE_243",
  "RSDE_124",
  "RSDE_194",
  "RSDE_280",
  "RSDE_86",
  "RSDE_76",
  "RSDE_162",
  "RSDE_178",
  "RSDE_226",
  "RSDE_244",
  "RSDE_170",
  "RSDE_109",
  "RSDE_29",
  "RSDE_122",
  "RSDE_145",
  "RSDE_230",
  "RSDE_249",
  "RSDE_233",
  "RSDE_207",
  "RSDE_251",
  "RSDE_218",
  "RSDE_255",
  "RSDE_38",
  "RSDE_3",
  "RSDE_65",
  "RSDE_134",
  "RSDE_272",
  "RSDE_102",
  "RSDE_208",
  "RSDE_224",
  "RSDE_60",
  "RSDE_143",
  "RSDE_253",
  "RSDE_222",
  "RSDE_285",
  "RSDE_37",
  "RSDE_200",
  "RSDE_215",
  "RSDE_80",
  "RSDE_117",
  "RSDE_149",
  "RSDE_89",
  "RSDE_260",
  "RSDE_41",
  "RSDE_79",
  "RSDE_90",
  "RSDE_64",
  "RSDE_101",
  "RSDE_154",
  "RSDE_148",
  "RSDE_206",
  "RSDE_111",
  "RSDE_156",
  "RSDE_18",
  "RSDE_264",
  "RSDE_212",
  "RSDE_119",
  "RSDE_19",
  "RSDE_98",
  "RSDE_220",
  "RSDE_126",
  "RSDE_44",
  "RSDE_229",
  "RSDE_258",
  "RSDE_118",
  "RSDE_152",
  "RSDE_191",
  "RSDE_121",
  "RSDE_159",
  "RSDE_217",
  "RSDE_30",
  "RSDE_142",
  "RSDE_268",
  "RSDE_254",
  "RSDE_181",
  "RSDE_52",
  "RSDE_231",
  "RSDE_132",
  "RSDE_97",
  "RSDE_184",
  "RSDE_27",
  "RSDE_283",
  "RSDE_190",
  "RSDE_164",
  "RSDE_54",
  "RSDE_235",
  "RSDE_266",
  "RSDE_189",
  "RSDE_2",
  "RSDE_123",
  "RSDE_57",
  "RSDE_9",
  "RSDE_277",
  "RSDE_32",
  "RSDE_165",
  "RSDE_26",
  "RSDE_49",
  "RSDE_278",
  "RSDE_186",
  "RSDE_85",
  "RSDE_171",
  "RSDE_130",
  "RSDE_70",
  "RSDE_209",
  "RSDE_241",
  "RSDE_113",
  "RSDE_106",
  "RSDE_180",
  "RSDE_139",
  "RSDE_112",
  "RSDE_16",
  "RSDE_247",
  "RSDE_175",
  "RSDE_223",
  "RSDE_182",
  "RSDE_12",
  "RSDE_11",
  "RSDE_246",
  "RSDE_56",
  "RSDE_237",
  "RSDE_68",
  "RSDE_71",
  "RSDE_47",
  "RSDE_279",
  "RSDE_17",
  "RSDE_271",
  "RSDE_14",
  "RSDE_160",
  "RSDE_107",
  "RSDE_213",
  "RSDE_176",
  "RSDE_155",
  "RSDE_273",
  "RSDE_261",
  "RSDE_28",
  "RSDE_1",
  "RSDE_288",
  "RSDE_127",
  "RSDE_263",
  "RSDE_135",
  "RSDE_275",
  "RSDE_270",
  "RSDE_84",
  "RSDE_131",
  "RSDE_100",
  "RSDE_77",
  "RSDE_114",
  "RSDE_10",
  "RSDE_74",
  "RSDE_24",
  "RSDE_196",
  "RSDE_91",
  "RSDE_93",
  "RSDE_95",
  "RSDE_214",
  "RSDE_195",
  "RSDE_151",
  "RSDE_104",
  "RSDE_105",
  "RSDE_210",
  "RSDE_265",
  "RSDE_248",
  "RSDE_72",
  "RSDE_146",
  "RSDE_267",
  "RSDE_144",
  "RSDE_187",
  "RSDE_78",
  "RSDE_172",
  "RSDE_198",
  "RSDE_259",
  "RSDE_250",
  "RSDE_166",
  "RSDE_66",
  "RSDE_82",
  "RSDE_173",
  "RSDE_211",
  "RSDE_8",
  "RSDE_169",
  "RSDE_33",
  "RSDE_234",
  "RSDE_34",
  "RSDE_43",
  "RSDE_274",
  "RSDE_115",
  "RSDE_185",
  "RSDE_83",
  "RSDE_40",
  "RSDE_69",
  "RSDE_128",
  "RSDE_58",
  "RSDE_21",
  "RSDE_174",
  "RSDE_167",
  "RSDE_168",
  "RSDE_289",
  "RSDE_239",
  "RSDE_125",
  "RSDE_204",
  "RSDE_177",
  "RSDE_238",
  "RSDE_157",
  "RSDE_219",
  "RSDE_227",
  "RSDE_141",
  "RSDE_192",
  "RSDE_59",
  "RSDE_53",
  "RSDE_225",
  "RSDE_197",
  "RSDE_88",
  "RSDE_55",
  "RSDE_110",
  "RSDE_20",
  "RSDE_63",
  "RSDE_287",
  "RSDE_67",
  "RSDE_201",
  "RSDE_282",
  "RSDE_22",
  "RSDE_6",
  "RSDE_39",
  "RSDE_108",
  "RSDE_87",
  "RSDE_158",
  "RSDE_199",
  "RSDE_193",
  "RSDE_188",
  "RSDE_7",
  "RSDE_245",
  "RSDE_256",
  "RSDE_216",
  "RSDE_103",
  "RSDE_232",
  "RSDE_281",
  "RSDE_262",
  "RSDE_228",
  "RSDE_116",
  "RSDE_284",
  "RSDE_133",
  "RSDE_276",
  "RSDE_183",
  "RSDE_161",
  "RSDE_286",
  "RSDE_236",
  "RSDE_120",
  "RSDE_257",
  "RSDE_35",
  "RSDE_153",
  "RSDE_269",
  "RSDE_179",
  "RSDE_202",
  "RSDE_15",
  "RSDE_81",
  "RSDE_94",
  "RSDE_138",
  "RSDE_4",
  "RSDE_73",
  "RSDE_242",
  "RSDE_48",
  "RSDE_50",
  "RSDE_46",
  "RSDE_205",
  "RSDE_96",
  "RSDE_137",
  "RSDE_136",
  "RSDE_36",
  "RSDE_163",
  "RSDE_99",
  "RSDE_13",
  "RSDE_23",
  "RSDE_203",
  "RSDE_5",
  "RSDE_45",
  "RSDE_129",
  "RSDE_252",
  "RSDE_31",
  "RSDE_75",
  "RSDE_62",
  "RSDE_221",
  "RSDE_240",
  "domain",
  "phylum",
  "class",
  "order",
  "family",
  "genus",
  "species")
df <- read_parquet(file, col_select=keep)

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

names_to_replace <- c("metagenome", "bacterium", "Plasmid_Ti", "Sym_plasmid",
                      "synthetic_construct", "unidentified", "archaeon",
                      "candidate_division_TM7_genomosp._GTL1",
                      "Candidatus_Erysipelatoclostridium_merdavium")
df <- df %>%
  mutate(species = case_when(
    species %in% names_to_replace ~ NA_character_,
    TRUE ~ species
  ))


###################### Aggregate by depth zones
# Read the metadata CSV file
metadata_df <- read.csv("/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/RSDE Metagenomics Metadata - edited.csv")
metadata_df <- metadata_df[c("Sample_ID", "Habitat")]
# Set "Sample_ID" as the row names and transpose the DataFrame
metadata_df <- metadata_df %>%
  column_to_rownames("Sample_ID") %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

# Define depth category function
depth_category <- function(depth) {
  if (depth < 20) {
    return("coastal")
  } else if (depth >= 20 && depth <= 200) {
    return("epi")
  } else if (depth > 200 && depth <= 1000) {
    return("meso")
  } else {
    return("bathy")
  }
}

# Define region category function
region_category <- function(latitude) {
  if (latitude < 17.5) {
    return("SRS")
  } else if (latitude >= 17.5 && latitude <= 22) {
    return("CSRS")
  } else if (latitude > 22 && latitude <= 25.5) {
    return("CNRS")
  } else if (latitude > 25.5 && latitude <= 27.8) {
    return("NRS")
  } else {
    return("AQB")
  }
}

# Apply the category functions to the appropriate rows
metadata_df["depth_category", ] <- sapply(as.numeric(metadata_df["Depth", ]), depth_category)
metadata_df["region_category", ] <- sapply(as.numeric(metadata_df["Latitude", ]), region_category)

# Change sample order to that of tax_df
rsde_columns <- grep("^RSDE_", colnames(tax_meta_df), value = TRUE)
metadata_df <- metadata_df[, rsde_columns, drop = FALSE]


### Make tax df smaller
# Group by the vector of column names and summarize all other columns
tax_df <- df %>%
  group_by(across(all_of(ranks))) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')


# Create a new DataFrame with NA values for the new columns
new_cols_df <- data.frame(matrix(NA, nrow = nrow(metadata_df), ncol = length(ranks)))
colnames(new_cols_df) <- ranks

# Add the new columns at the start using cbind
metadata_df2 <- cbind(new_cols_df, metadata_df)


tax_meta_df <- rbind(tax_meta_df, metadata_df2)

write.csv(tax_meta_df, "/Users/simplexdna/Desktop/final_table_tax2.csv")
tax_meta_df <- read.csv("/Users/simplexdna/Desktop/final_table_tax.csv", row.names = 1)

chord_orders <- c(
  "Albuliformes",
  "Anguilliformes",
  "Atheriniformes",
  "Batrachoidiformes",
  "Beloniformes",
  "Blenniiformes",
  "Carangiformes",
  "Clupeiformes",
  "Gadiformes",
  "Gobiiformes",
  "Perciformes",
  "Pleuronectiformes",
  "Scombriformes",
  "Spariformes",
  "Syngnathiformes",
  "Tetraodontiformes",
  "Elopiformes",
  "Phlebobranchia",
  "Stolidobranchia",
  "Amphioxiformes",
  "Carcharhiniformes",
  "Chimaeriformes",
  "Orectolobiformes",
  "Rhinopristiformes",
  "Sirenia"
)

filtered_df <- tax_meta_df %>%
  filter(order %in% chord_orders | row_number() > (n() - 5))

write.csv(filtered_df, "/Users/simplexdna/Desktop/chords_with_all_meta.csv")


### Aggregate samples

# Get the unique depth categories (excluding NA)
depth_categories <- unique(tax_meta_df["depth_category", ][!is.na(tax_meta_df["depth_category", ])])

# Initialize an empty data frame to store results
result_non_euk_depth <- data.frame(
  depth_category = depth_categories,
  Bacteria = numeric(length(depth_categories)),
  Archaea = numeric(length(depth_categories)),
  Viruses = numeric(length(depth_categories)),
  stringsAsFactors = FALSE
)

# Loop through each depth category
for (depth_cat in depth_categories) {
  # Select taxonomic columns + columns with the current depth category
  selected_columns <- c(c(1:7), which(tax_meta_df["depth_category", ] == depth_cat))
  sub_df <- tax_meta_df[, selected_columns, drop = FALSE]
  
  # Convert non-taxonomic columns to numeric
  sub_df[, 8:ncol(sub_df)] <- lapply(sub_df[, 8:ncol(sub_df)], as.numeric)
  
  # For each domain, filter rows and perform operations
  for (domain in c("Bacteria", "Archaea", "Viruses")) {
    domain_df <- sub_df[sub_df$domain == domain, , drop = FALSE]
    
    # Sum all non-taxonomic columns
    domain_df$sum <- rowSums(domain_df[8:ncol(domain_df)], na.rm = TRUE)
    
    # Keep only rows with sum > 0
    domain_df <- domain_df[domain_df$sum > 0, ]
    
    # Count unique entries in the "species" column
    unique_species <- length(unique(domain_df$species[!is.na(domain_df$species)]))

    # Store the count in the result_non_euk_depth data frame
    result_non_euk_depth[result_non_euk_depth$depth_category == depth_cat, domain] <- unique_species
  }
}

#### REGION
# Get the unique depth categories (excluding NA)
region_categories <- unique(tax_meta_df["region_category", ][!is.na(tax_meta_df["region_category", ])])

# Initialize an empty data frame to store results
result_non_euk_region <- data.frame(
  region_category = region_categories,
  Bacteria = numeric(length(region_categories)),
  Archaea = numeric(length(region_categories)),
  Viruses = numeric(length(region_categories)),
  stringsAsFactors = FALSE
)

# Loop through each depth category
for (region_cat in region_categories) {
  # Select taxonomic columns + columns with the current depth category
  selected_columns <- c(c(1:7), which(tax_meta_df["region_category", ] == region_cat))
  sub_df <- tax_meta_df[, selected_columns, drop = FALSE]
  
  # Convert non-taxonomic columns to numeric
  sub_df[, 8:ncol(sub_df)] <- lapply(sub_df[, 8:ncol(sub_df)], as.numeric)
  
  # For each domain, filter rows and perform operations
  for (domain in c("Bacteria", "Archaea", "Viruses")) {
    domain_df <- sub_df[sub_df$domain == domain, , drop = FALSE]
    
    # Sum all non-taxonomic columns
    domain_df$sum <- rowSums(domain_df[8:ncol(domain_df)], na.rm = TRUE)
    
    # Keep only rows with sum > 0
    domain_df <- domain_df[domain_df$sum > 0, ]
    
    # Count unique entries in the "species" column
    unique_species <- length(unique(domain_df$species[!is.na(domain_df$species)]))
    
    # Store the count in the result_non_euk_region data frame
    result_non_euk_region[result_non_euk_region$region_category == region_cat, domain] <- unique_species
  }
}


# Add supergroups per phylum
## Function to get supergroup for a phylum
get_supergroup <- function(phylum) {
  for (supergroup in names(supergroups)) {
    if (phylum %in% supergroups[[supergroup]]) {
      return(supergroup)
    }
  }
  return(NA) # If not found, return NA
}
tax_meta_df$row_names <- rownames(tax_meta_df)
tax_meta_df <- as.data.frame(tax_meta_df)
## Apply the function to create a new column in the dataframe
tax_meta_df$supergroup <- sapply(tax_meta_df$phylum, get_supergroup)
rownames(tax_meta_df) <- tax_meta_df$row_names
tax_meta_df$row_names <- NA
# Move the last column to the first position
tax_meta_df <- tax_meta_df[, c(ncol(tax_meta_df), 1:(ncol(tax_meta_df) - 1))]


# Get the unique depth categories (excluding NA)
depth_categories <- unique(tax_meta_df["depth_category", ][!is.na(tax_meta_df["depth_category", ])])

# Create the data frame
result_euk_depth <- data.frame(
  depth_category = depth_categories,
  matrix(ncol = length(supergroups), nrow = length(depth_categories)),
  stringsAsFactors = FALSE
)

# Assign the supergroup names as column names
colnames(result_euk_depth) <- c("depth_category", names(supergroups))

# Loop through each depth category
for (depth_cat in depth_categories) {
  # Select taxonomic columns + columns with the current depth category
  selected_columns <- c(c(1:8), which(tax_meta_df["depth_category", ] == depth_cat))
  sub_df <- tax_meta_df[, selected_columns, drop = FALSE]
  
  # Convert non-taxonomic columns to numeric
  sub_df[, 9:ncol(sub_df)] <- lapply(sub_df[, 9:ncol(sub_df)], as.numeric)
  
  # For each domain, filter rows and perform operations
  for (supergroup in names(supergroups)) {
    supergroup_df <- sub_df[sub_df$supergroup == supergroup, , drop = FALSE]
    
    # Sum all non-taxonomic columns
    supergroup_df$sum <- rowSums(supergroup_df[9:ncol(supergroup_df)], na.rm = TRUE)
    
    # Keep only rows with sum > 0
    supergroup_df <- supergroup_df[supergroup_df$sum > 0, ]
    
    # Count unique entries in the "species" column
    unique_species <- length(unique(supergroup_df$species[!is.na(supergroup_df$species)]))
    
    # Store the count in the result_non_euk_depth data frame
    result_euk_depth[result_non_euk_depth$depth_category == depth_cat, supergroup] <- unique_species
  }
}

#### REGION
# Get the unique depth categories (excluding NA)
region_categories <- unique(tax_meta_df["region_category", ][!is.na(tax_meta_df["region_category", ])])

# Create the data frame
result_euk_region <- data.frame(
  region_category = region_categories,
  matrix(ncol = length(supergroups), nrow = length(region_categories)),
  stringsAsFactors = FALSE
)

# Assign the supergroup names as column names
colnames(result_euk_region) <- c("region_category", names(supergroups))

# Loop through each depth category
for (region_cat in region_categories) {
  # Select taxonomic columns + columns with the current depth category
  selected_columns <- c(c(1:8), which(tax_meta_df["region_category", ] == region_cat))
  sub_df <- tax_meta_df[, selected_columns, drop = FALSE]
  
  # Convert non-taxonomic columns to numeric
  sub_df[, 9:ncol(sub_df)] <- lapply(sub_df[, 9:ncol(sub_df)], as.numeric)
  
  # For each domain, filter rows and perform operations
  for (supergroup in names(supergroups)) {
    supergroup_df <- sub_df[sub_df$supergroup == supergroup, , drop = FALSE]
    
    # Sum all non-taxonomic columns
    supergroup_df$sum <- rowSums(supergroup_df[9:ncol(supergroup_df)], na.rm = TRUE)
    
    # Keep only rows with sum > 0
    supergroup_df <- supergroup_df[supergroup_df$sum > 0, ]
    
    # Count unique entries in the "species" column
    unique_species <- length(unique(supergroup_df$species[!is.na(supergroup_df$species)]))
    
    # Store the count in the result_non_euk_region data frame
    result_euk_region[result_non_euk_region$region_category == region_cat, supergroup] <- unique_species
  }
}

write.csv(result_euk_region, paste0(outdir, "/result_euk_region.csv"))
write.csv(result_non_euk_region, paste0(outdir, "/result_non_euk_region.csv"))
write.csv(result_euk_depth, paste0(outdir, "/result_euk_depth.csv"))
write.csv(result_non_euk_depth, paste0(outdir, "/result_non_euk_depth.csv"))
