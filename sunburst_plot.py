import plotly.express as px
import pandas as pd
import os

# Define name of the input directory in which all formatted tables are saved
indir = "data_formatting_results"
# Define name of the output directory in which graphs are saved
outdir = "data_visualizations"


df = pd.read_csv(os.path.join(indir, "sunburst_df.csv"))

df_arc = df[df["domain"] == "Archaea"]
df_vir = df[df["domain"] == "Viruses"]
df_bac = df[df["domain"] == "Bacteria"]

##### Archaea
top_archaea = [
    "Euryarchaeota",
    "Thermoproteota",
    "Candidatus_Thermoplasmatota",
    "Nitrososphaerota",
    "Candidatus_Undinarchaeota",
]
df_arc.loc[~df_arc["phylum"].isin(top_archaea), ["phylum", "class"]] = "Other"
df_arc.loc[
    (df_arc["phylum"] == "Euryarchaeota")
    & ~df_arc["class"].isin(["Halobacteria", "Methanomicrobia"]),
    "class",
] = "Other"

fig_arc = px.sunburst(
    df_arc,
    path=["domain", "phylum", "class"],
    values="order_count",
)
fig_arc.show()

fig_arc.update_traces(textfont_size=15)

fig_arc.write_image(os.path.join(outdir, "sunburst_domain_phylum_class_arc.svg"))
fig_arc.write_image(os.path.join(outdir, "sunburst_domain_phylum_class_arc.png"))

#### Viruses
fig_vir = px.sunburst(
    df_vir,
    path=["domain", "phylum", "class"],
    values="order_count",
)
fig_vir.show()

fig_vir.update_traces(textfont_size=15)

fig_vir.write_image(os.path.join(outdir, "sunburst_domain_phylum_class_vir.svg"))
fig_vir.write_image(os.path.join(outdir, "sunburst_domain_phylum_class_vir.png"))

#### Bacteria
top_ten_phyla_bac = [
    "Pseudomonadota",
    "Actinomycetota",
    "Cyanobacteriota",
    "Bacillota",
    "Thermodesulfobacteriota",
    "Chloroflexota",
    "Acidobacteriota",
    "Planctomycetota",
    "Bacteroidota",
    "Verrucomicrobiota",
]
df_bac.loc[~df_bac["phylum"].isin(top_ten_phyla_bac), ["phylum", "class"]] = "Other"
df_bac.loc[df_bac["order_count"] == 1, ["class"]] = "Other"


fig_bac = px.sunburst(
    df_bac,
    path=["domain", "phylum", "class"],
    values="order_count",
)
fig_bac.update_traces(textfont_size=15)

fig_bac.write_image(os.path.join(outdir, "sunburst_domain_phylum_bac.svg"))
fig_bac.write_image(os.path.join(outdir, "sunburst_domain_phylum_bac.png"))


# Barplot Bacteria
df_bac_top_ten = df_bac[df_bac["phylum"] != "Other"]

# Make phylum categorical to preserve order
df_bac_top_ten["phylum"] = pd.Categorical(
    df_bac_top_ten["phylum"], categories=top_ten_phyla_bac, ordered=True
)


# Group by 'phylum' and 'class', then sum 'order_count'
grouped_df = df_bac_top_ten.groupby(["phylum", "class"], as_index=False)[
    "order_count"
].sum()

# Sort by 'phylum' and 'order_count' in descending order
sorted_df = grouped_df.sort_values(
    by=["phylum", "order_count"], ascending=[True, False]
)
sorted_df = sorted_df[sorted_df["order_count"] != 0]

# Make phylum non-categorical for atachment of phylum to other
sorted_df["phylum"] = sorted_df["phylum"].astype(str)
# Make sure class others are attached to their phyla
sorted_df.loc[sorted_df["class"] == "Other", "class"] = (
    sorted_df["class"] + " - " + sorted_df["phylum"]
)

# Turn phylum back to categorical
sorted_df["phylum"] = pd.Categorical(
    sorted_df["phylum"], categories=top_ten_phyla_bac, ordered=True
)

# Ensure 'class' column is treated as categorical and its order is maintained
sorted_df["class"] = pd.Categorical(
    sorted_df["class"], categories=sorted_df["class"].unique(), ordered=True
)


# Plot with Plotly Express (horizontal bar plot)
barplot_bac = px.bar(
    sorted_df,
    x="order_count",
    y="class",
    color="phylum",
    orientation="h",
    title="Order Counts per Class by Phylum",
    labels={"class": "Class", "order_count": "Order Count"},
    category_orders={"class": sorted_df["class"].tolist()},
    height=800,
)

barplot_bac.show()

barplot_bac.write_image(
    os.path.join(outdir, "sunburst_domain_phylum_bac_class_bar.svg")
)
