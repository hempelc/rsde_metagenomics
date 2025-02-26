library(ggplot2)
library(dplyr)
library(scales)
library(forcats)

# Define name of the input directory in which all formatted tables are saved
indir <- "data_formatting_results"
# Define name of the output directory in which graphs are saved
outdir <- "data_visualizations"

#################### Create output dir
if (!dir.exists(outdir)) {
  dir.create(outdir)
}

#################### Radial chart
df_eukaryota_summary <- read.csv(file.path(indir, "eukaryota_phyla_family_number.csv"))

# Group order
## Convert the supergroup column to a factor with the specified levels
supergroup_order <- c("stramenopiles", "alveolata", "rhizaria", "haptista", "archaeplastida", "discoba", "metamonada", "amoebozoa", "holomycota", "holozoa")
df_eukaryota_summary$supergroup <- factor(df_eukaryota_summary$supergroup, levels = supergroup_order)
## Sort the dataframe
df_eukaryota_summary <- df_eukaryota_summary %>%
  arrange(supergroup, desc(families))
## Preserve the order of phylum as it appears in the dataframe
df_eukaryota_summary$phylum <- fct_inorder(df_eukaryota_summary$phylum)

# Define breaks for logarithmic scale including intermediate ticks
main_breaks <- 10^(-2:2)
intermediate_breaks <- c(30)
all_breaks <- sort(c(main_breaks, intermediate_breaks))

# Create the curved bar chart
ggplot(df_eukaryota_summary, aes(x = phylum, y = families, fill = supergroup)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = phylum, y = families), angle = 90, hjust = -0.1, size = 3) +
  coord_radial(start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.6, rotate.angle = TRUE) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10, sigma = 1),
    breaks = all_breaks,
    labels = label_number(accuracy = 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( linewidth = 0.03, color="black" )
    #panel.grid = element_blank()
  ) +
  labs(x = NULL, 
       y = "Number of Families")

ggsave(file=file.path(outdir, "eukaryota_phyla_family_number.svg"), width=10, height=5, dpi=300)

######################## Pie charts
assigned_df <- read.csv("data_formatting_results/assigned_df.csv")
ggplot(assigned_df, aes(x="", y=total_count_percentages, fill= assignment_status) )+ 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0,direction = -1) + theme_void() +
  labs(x = NULL, y = NULL, fill = NULL) + scale_fill_brewer(palette="Blues", direction=-1) +
  geom_text(aes(label = paste0(round(total_count_percentages, 1), "%")), size=3, position=position_stack(vjust=0.5)) 
ggsave(file=file.path(outdir, "assigned_pie.svg"), width=10, height=5, dpi=300)

domain_df <- read.csv("data_formatting_results/domain_df.csv")
ggplot(domain_df, aes(x="", y=total_count_percentages, fill= domain) )+ 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0,direction = -1) + theme_void() +
  labs(x = NULL, y = NULL, fill = NULL) + scale_fill_brewer(palette="Blues", direction=-1) +
  geom_text(aes(label = percent(total_count_percentages, 0.1) ), size=3, position=position_stack(vjust=0.5)) 
ggsave(file=file.path(outdir, "domain_pie.svg"), width=10, height=5, dpi=300)

holozoa_df <- read.csv("data_formatting_results/non_holozoa_vs_holozoa_df_df.csv")
ggplot(holozoa_df, aes(x="", y=total_count_percentages, fill= holozoan_status) )+ 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0,direction = -1) + theme_void() +
  labs(x = NULL, y = NULL, fill = NULL) + scale_fill_brewer(palette="Blues", direction=-1) +
  geom_text(aes(label = paste0(round(total_count_percentages, 1), "%")), size=3, position=position_stack(vjust=0.5)) 
ggsave(file=file.path(outdir, "holozoa_pie.svg"), width=10, height=5, dpi=300)
