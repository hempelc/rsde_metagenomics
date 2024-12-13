library(ggplot2)
library(dplyr)
library(scales)
library(forcats)

# Define name of the output directory in which graphs are saved
outdir <- "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_visualizations"

#################### Create output dir
if (!dir.exists(outdir)) {
  dir.create(outdir)
}

#################### Radial chart
df_chordata_summary <- read.csv("/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results/chordata_order_species_number_manual_edit.csv")

# Group order
## Convert the supergroup column to a factor with the specified levels
class_order <- c("Cephalochordata", "Ascidiacea", "Chondrichthyes", "Actinopterygii", "Mammalia")
df_chordata_summary$class <- factor(df_chordata_summary$class, levels = class_order)
## Sort the dataframe
df_chordata_summary <- df_chordata_summary %>%
  arrange(class, desc(num_species))
## Preserve the order of phylum as it appears in the dataframe
df_chordata_summary$order <- fct_inorder(df_chordata_summary$order)

# Define breaks for logarithmic scale including intermediate ticks
main_breaks <- 10^(-2:2)
intermediate_breaks <- c(30)
all_breaks <- sort(c(main_breaks, intermediate_breaks))

# Create the curved bar chart
ggplot(df_chordata_summary, aes(x = order, y = num_species, fill = class)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = order, y = num_species), angle = 90, hjust = -0.1, size = 3) +
  coord_radial(start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.6, rotate.angle = TRUE) +
#  scale_y_continuous(
#    trans = pseudo_log_trans(base = 10, sigma = 1),
#    breaks = all_breaks,
#    labels = label_number(accuracy = 1)
#  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( linewidth = 0.03, color="black" )
    #panel.grid = element_blank()
  ) +
  labs(x = NULL, 
       y = "Number of Species")

ggsave(file=file.path(outdir, "chordata_orders_species_number.svg"), width=10, height=5, dpi=300)
