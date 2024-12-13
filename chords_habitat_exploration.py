import pandas as pd
import plotly.express as px
import numpy as np
from collections import Counter

ranks = ["domain", "phylum", "class", "order", "family", "genus", "species"]

# Read the dataset
df = pd.read_csv("/Users/simplexdna/Desktop/chords_with_all_meta.csv", index_col=0)

# Step 1: Identify columns that start with "RSDE"
rsde_columns = [col for col in df.columns if col.startswith("RSDE")]

# Step 2: Extract the "Habitat" row and relevant RSDE columns
habitat_row = df.loc["Habitat", rsde_columns]

# Step 3: Drop the "Habitat" row from the DataFrame to focus on numeric data
numeric_data = df.drop(index="Habitat").iloc[:-4]

# Step 4: Transpose RSDE columns to align with Habitat for grouping
rsde_data = numeric_data[rsde_columns].transpose()
rsde_data["Habitat"] = habitat_row.values
rsde_data.loc[:, rsde_data.columns != "Habitat"] = rsde_data.loc[
    :, rsde_data.columns != "Habitat"
].astype(float)

# Step 5: Group by Habitat and sum the values
grouped = rsde_data.groupby("Habitat").sum().transpose()
# Convert DataFrame to binary
binary_grouped = (grouped > 0).astype(int)

final_df = pd.concat((df[ranks].iloc[:-5], binary_grouped), axis=1)

final_df["order-species"] = final_df["order"] + "-" + final_df["species"]
final_df["class-species"] = final_df["class"] + "-" + final_df["species"]

n_habitats = Counter(habitat_row.values)

# Set different columns as the index and select numeric columns for the heatmap
heatmap_data = final_df.set_index("order-species")[list(np.unique(habitat_row.values))]
heatmap_data = final_df.set_index("class-species")[list(np.unique(habitat_row.values))]


# Create heatmap
fig = px.imshow(
    heatmap_data,
    labels=dict(x="Habitat Type", y="Order-Species", color="Value"),
    x=heatmap_data.columns,
    y=heatmap_data.index,
    color_continuous_scale="Viridis",
)

# Update layout for better visualization
fig.update_layout(
    title="Heatmap of Species Across Habitats",
    xaxis_title="Habitat Type",
    yaxis_title="Order-Species",
    yaxis=dict(autorange="reversed"),  # Optional: Reverse y-axis for better readability
    width=500,
    height=1500,
)

# Class level
class_df = (
    final_df.set_index("class")[list(np.unique(habitat_row.values))]
    .groupby("class")
    .sum()
)
class_df["total"] = list(Counter(final_df["class"]).values())

for habitat in list(np.unique(habitat_row.values)):
    fig = px.bar(class_df[habitat], text=class_df[habitat].values)
    fig.update_yaxes(range=[0, 27])
    fig.write_image(f"/Users/simplexdna/Desktop/chords_{habitat}.svg")


# Venn diagram
from venny4py.venny4py import *

names = {
    "sea": "Open sea",
    "seagrass_meadow": "Seagrass meadow",
    "mangrove ": "Mangroves",
    "coral_reef": "Coral reef",
}

for tax_class in list(final_df["class"].drop_duplicates()):
    sets = {}
    subdf = final_df[final_df["class"] == tax_class]
    for habitat in list(habitat_row.drop_duplicates()):
        sets[names[habitat]] = set(subdf[subdf[habitat] == 1]["species"].dropna())
    venny4py(sets=sets, out=tax_class)

# dict of sets
sets = {
    "Set1": set(list("Harry Potter")),
    "Set2": set(list("Hermione Granger")),
    "Set3": set(list("Ron Weasley")),
    "Set4": set(list("Severus Snape")),
}
os.getcwd()
