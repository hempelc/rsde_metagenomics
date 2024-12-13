library(plyr)
library(dplyr)
library(tidyverse)
library(vegan)
library(zCompositions)
library(metagMisc)
library(data.table)
library(htmlwidgets)
library(iNEXT)
library(microViz)
library(ComplexHeatmap)
library(readxl)
library(GGally)
library(plotly)
library(fastDummies)
library(phyloseq)


# Define file
df_file <- "/Users/simplexdna/Desktop/final_table_tax.csv"

# Make folder for plots
plot_outdir <- "/Users/simplexdna/Desktop/metagenomics_pca"
dir.create(plot_outdir, showWarnings = FALSE)

# Read in file
df <- read.csv(df_file, row.names=1)

#############################  Metadata processing
# Extract metadata
metadata <- as.data.frame(t(df[(nrow(df) - 5):nrow(df), -(1:7)]))

# Create the new column "Depth (RV)" based on the "habitat" column
metadata$"Depth_RV" <- ifelse(
  metadata$Habitat == "sea", 
  ">40m OceanX", 
  "<20m Al Azizi"
)

# Adjust column classes
metadata$Latitude <- as.numeric(metadata$Latitude)
metadata$Longitude <- as.numeric(metadata$Longitude)
metadata$Depth <- as.numeric(metadata$Depth)
metadata$'region_category' <- factor(metadata$'region_category', levels=c("AQB", "NRS", "CNRS", "CSRS", "SRS"))
metadata$'depth_category' <- factor(metadata$'depth_category', levels=c("coastal", "epi", "meso", "bathy"))
metadata$'Habitat' <- factor(metadata$'Habitat', levels=c("seagrass_meadow", "coral_reef", "mangrove", "sea"))

# Create dummy variables
metadata_dummy <- dummy_cols(metadata[, sapply(metadata, is.factor)], remove_first_dummy = FALSE, remove_selected_columns = TRUE)
metadata <- cbind(metadata, metadata_dummy)


# - Check if metadata is correlated
heatmap(abs(cor(select_if(metadata, is.numeric))), 
        # Compute pearson correlation (note they are absolute values)
        col = rev(heat.colors(6)), 
        Colv = NA, Rowv = NA)
legend("topleft", 
       title = "Absolute Pearson R",
       legend =  round(seq(0,1, length.out = 6),1),
       y.intersp = 0.7, bty = "n",
       fill = rev(heat.colors(6)))

################################ Make tax df
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
                      "Candidatus_Erysipelatoclostridium_merdavium",
                      "Not available", "Not_available", "Taxonomy unreliable",
                      "No match in database", "Unreliable taxonomy")
df <- df %>%
  mutate(species = case_when(
    species %in% names_to_replace ~ NA_character_,
    TRUE ~ species
  ))

ranks <- c("domain", "phylum", "class", "order", "family", "genus", "species")
tax_df <- df[ranks]
# Remove env rows and NA row
tax_df <- tax_df[1:(nrow(tax_df) - 7), ]

######################### OTU sample processing
otu_df <- df[, !names(df) %in% ranks]
# Remove env rows and NA row
otu_df <- otu_df[1:(nrow(otu_df) - 7), ]
otu_df[] <- lapply(otu_df, as.numeric)

#################### Make phyloseq objects
OTU <- otu_table(otu_df, taxa_are_rows = TRUE)

# Tax table: has to be a matrix for phyloseq to be happy
TAX <- tax_table(as.matrix(tax_df))

# Metadata: row names have to align with sample names in the OTU table
META <- sample_data(metadata)

# Make the phyloseq object
physeq = phyloseq(OTU, TAX, META)


############################ Phyloseq object processing
# Threshold prevalence >2
tresh_prev = 2
physeq <- filter_taxa(physeq, function(x) sum(x > 0) > tresh_prev, prune = T)
physeq <- tax_fix(physeq, unknowns = c("Incertae_Sedis", "Incertae_Sedis class", "Gammaproteobacteria_Incertae_Sedis", "Alphaproteobacteria_Incertae_Sedis", "Unknown_Family"))

############# RDAs
## Explore
physeq %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

pca_RV <- physeq %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Depth_RV", fill = "Depth_RV",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Depth_RV)
  )
svglite::svglite(file.path(plot_outdir, "pca_RV.svg"), width = 4.5, height = 3)
pca_RV
dev.off()

physeq_alazizi <- ps_filter(physeq, depth_category == "coastal")
physeq_oceanx <- ps_filter(physeq, depth_category != "coastal")

physeq_alazizi %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

physeq_oceanx %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

pca_oceanx_depth <- physeq_oceanx %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "depth_category", fill = "depth_category",
    shape = "circle", alpha = 0.5,
    size = 2,
    plot_taxa = 1:5
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = depth_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = depth_category, y = depth_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = depth_category, x = depth_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_oceanx_depth.svg"), width = 6, height = 4)
pca_oceanx_depth
dev.off()

pca_oceanx_region_supp <- physeq_oceanx %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2,
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_oceanx_region_supp.svg"), width = 6, height = 4)
pca_oceanx_region_supp
dev.off()

pca_alazizi_habiat_supp <- physeq_alazizi %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2,
    plot_taxa = 1:5
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  ) +
  ggside::geom_xsideboxplot(aes(fill = Habitat, y = Habitat), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_alazizi_habitat_supp.svg"), width = 6, height = 4)
pca_alazizi_habiat_supp
dev.off()

pca_alazizi_region_supp <- physeq_alazizi %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2,
    plot_taxa = 1:5
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_alazizi_region_supp.svg"), width = 6, height = 4)
pca_alazizi_region_supp
dev.off()


###### Al azizi eukaryotic
physeq_alazizi_eukaryota <- subset_taxa(physeq_alazizi, domain=="Eukaryota")
physeq_alazizi_non_eukaryota <- subset_taxa(physeq_alazizi, domain!="Eukaryota")

physeq_alazizi_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

physeq_alazizi_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    constraints = c("Habitat_seagrass_meadow", "Habitat_coral_reef", "Habitat_mangrove"),
    method = "RDA"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  ) +
  ggside::geom_xsideboxplot(aes(fill = Habitat, y = Habitat), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

pca_alazizi_eukaryotes_region_habitat <- physeq_alazizi_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    plot_taxa = 1:4,
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE)
svglite::svglite(file.path(plot_outdir, "pca_alazizi_eukaryotes_region_habitat.svg"), width = 7, height = 5)
pca_alazizi_eukaryotes_region_habitat
dev.off()

physeq_alazizi_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
    ggside::theme_ggside_void()

physeq_alazizi_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  ) +
  ggside::geom_xsideboxplot(aes(fill = Habitat, y = Habitat), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

###### Al azizi non-eukaryotic
physeq_alazizi_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    constraints = c("Habitat_seagrass_meadow", "Habitat_coral_reef", "Habitat_mangrove"),
    method = "RDA"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  )

pca_alazizi_non_eukaryotes_region_supp <- physeq_alazizi_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_alazizi_non_eukaryotes_region_supp.svg"), width = 7, height = 5)
pca_alazizi_non_eukaryotes_region_supp
dev.off()

pca_alazizi_non_eukaryotes_habitat_supp <- physeq_alazizi_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  ) +
  ggside::geom_xsideboxplot(aes(fill = Habitat, y = Habitat), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()
svglite::svglite(file.path(plot_outdir, "pca_alazizi_non_eukaryotes_habitat_supp.svg"), width = 7, height = 5)
pca_alazizi_non_eukaryotes_habitat_supp
dev.off()

##### AL azizi chordata
physeq_alazizi_chordata <- subset_taxa(physeq_alazizi, phylum=="Chordata")

physeq_alazizi_chordata %>%
  tax_transform(rank = "class", trans = "clr") %>%
  ord_explore()

physeq_alazizi_chordata %>%
  tax_transform(rank = "class", trans = "clr") %>%
  ord_calc(
    constraints = c("Habitat_seagrass_meadow", "Habitat_coral_reef", "Habitat_mangrove"),
    method = "RDA"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  )

physeq_alazizi_chordata %>%
  tax_transform(rank = "class", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

physeq_alazizi_chordata %>%
  tax_transform(rank = "class", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "Habitat", fill = "Habitat",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = Habitat)
  ) +
  ggside::geom_xsideboxplot(aes(fill = Habitat, y = Habitat), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = Habitat, x = Habitat), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

# No pattern for chords

#################
###### OceanX eukaryotic
physeq_oceanx_eukaryota <- subset_taxa(physeq_oceanx, domain=="Eukaryota")
physeq_oceanx_non_eukaryota <- subset_taxa(physeq_oceanx, domain!="Eukaryota")

physeq_oceanx_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

physeq_oceanx_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "depth_category", fill = "depth_category",
    shape = "circle", alpha = 0.5,
    size = 2,
    plot_taxa = 1:5
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = depth_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = depth_category, y = depth_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = depth_category, x = depth_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

physeq_oceanx_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2,
    plot_taxa = 1:5
  ) +
  ggplot2::stat_ellipse(
    ggplot2::aes(colour = region_category)
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE) +
  ggside::theme_ggside_void()

physeq_oceanx_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_explore()

physeq_oceanx_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    plot_taxa = 1:4,
    colour = "depth_category", fill = "depth_category",
    shape = "circle", alpha = 0.5,
    size = 2
  )

physeq_oceanx_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    plot_taxa = 1:4,
    colour = "depth_category", fill = "depth_category",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggside::geom_xsideboxplot(aes(fill = depth_category, y = depth_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = depth_category, x = depth_category), orientation = "x", varwidth=FALSE, show.legend=FALSE)

physeq_oceanx_non_eukaryota %>%
  tax_transform(rank = "phylum", trans = "clr") %>%
  ord_calc(
    method = "auto"
  ) %>% 
  ord_plot(
    axes = c(1, 2),
    plot_taxa = 1:4,
    colour = "region_category", fill = "region_category",
    shape = "circle", alpha = 0.5,
    size = 2
  ) +
  ggside::geom_xsideboxplot(aes(fill = region_category, y = region_category), orientation = "y", varwidth=FALSE, show.legend=FALSE) +
  ggside::geom_ysideboxplot(aes(fill = region_category, x = region_category), orientation = "x", varwidth=FALSE, show.legend=FALSE)

physeq_oceanx_chordata <- subset_taxa(physeq_oceanx, phylum=="Chordata")

physeq_oceanx_chordata %>%
  tax_transform(rank = "class", trans = "clr") %>%
  ord_explore()

# No pattern for chords