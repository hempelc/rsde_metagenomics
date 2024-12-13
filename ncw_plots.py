import pandas as pd
import plotly.express as px
import os

outdir = "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_visualizations"

euk_depth_file = "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results/result_euk_depth.csv"
euk_region_file = "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results/result_euk_region.csv"
non_euk_depth_file = "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results/result_non_euk_depth.csv"
non_euk_region_file = "/Users/simplexdna/GDrive KAUST/Elisa & Chris/RSDE/Paper 1 - taxonomy paper/data_formatting_results/result_non_euk_region.csv"

euk_depth_df = pd.read_csv(euk_depth_file, index_col=0).melt(
    id_vars=["depth_category"], var_name="supergroup", value_name="num_species"
)
euk_region_df = pd.read_csv(euk_region_file, index_col=0).melt(
    id_vars=["region_category"], var_name="supergroup", value_name="num_species"
)
non_euk_depth_df = pd.read_csv(non_euk_depth_file, index_col=0).melt(
    id_vars=["depth_category"], var_name="domain", value_name="num_species"
)
non_euk_region_df = pd.read_csv(non_euk_region_file, index_col=0).melt(
    id_vars=["region_category"], var_name="domain", value_name="num_species"
)

# Depth plots
# euk_depth = px.scatter(euk_depth_df,  y="depth_category", x="supergroup", size="num_species",
# 	         color="depth_category", size_max=60, text="num_species")
# euk_depth.update_traces(textposition='top center')
custom_euk_order = [
    "holomycota",
    "holozoa",
    "archaeplastida",
    "stramenopiles",
    "alveolata",
    "amoebozoa",
    "discoba",
    "rhizaria",
    "haptista",
    "metamonada",
]
custom_domain_order = ["Bacteria", "Viruses", "Archaea"]
for depth_category in list(euk_depth_df["depth_category"].drop_duplicates()):
    euk_depth = px.bar(
        euk_depth_df[euk_depth_df["depth_category"] == depth_category],
        y="num_species",
        x="supergroup",
        text="num_species",
        category_orders={"supergroup": custom_euk_order},
    )
    euk_depth.update_yaxes(range=[0, 1122])
    euk_depth.write_image(os.path.join(outdir, f"depth_plots_euk_{depth_category}.svg"))

    non_euk_depth = px.bar(
        non_euk_depth_df[non_euk_depth_df["depth_category"] == depth_category],
        y="num_species",
        x="domain",
        text="num_species",
        category_orders={"domain": custom_domain_order},
    )
    non_euk_depth.update_yaxes(range=[0, 15343])
    non_euk_depth.write_image(
        os.path.join(outdir, f"depth_plots_non_euk_{depth_category}.svg")
    )


# Region plots
custom_euk_order = [
    "holomycota",
    "holozoa",
    "archaeplastida",
    "stramenopiles",
    "alveolata",
    "amoebozoa",
    "discoba",
    "rhizaria",
    "haptista",
    "metamonada",
]
custom_domain_order = ["Bacteria", "Viruses", "Archaea"]
for region_category in list(euk_region_df["region_category"].drop_duplicates()):
    euk_region = px.bar(
        euk_region_df[euk_region_df["region_category"] == region_category],
        y="num_species",
        x="supergroup",
        text="num_species",
        category_orders={"supergroup": custom_euk_order},
    )
    euk_region.update_yaxes(range=[0, 1104])
    euk_region.write_image(
        os.path.join(outdir, f"region_plots_euk_{region_category}.svg")
    )

    non_euk_region = px.bar(
        non_euk_region_df[non_euk_region_df["region_category"] == region_category],
        y="num_species",
        x="domain",
        text="num_species",
        category_orders={"domain": custom_domain_order},
    )
    non_euk_region.update_yaxes(range=[0, 3842])
    non_euk_region.write_image(
        os.path.join(outdir, f"region_plots_non_euk_{region_category}.svg")
    )
