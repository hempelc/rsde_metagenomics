import plotly.express as px
import pandas as pd

df = pd.read_csv("/Users/simplexdna/Desktop/test.csv")

fig = px.sunburst(
    df,
    path=["domain", "phylum", "class"],
    values="order_count",
)
fig.show()
