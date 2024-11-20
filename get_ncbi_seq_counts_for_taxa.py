from Bio import Entrez
import pandas as pd

# Replace with your email
Entrez.email = "christopher.hempel@kaust.edu.sa"


# Function to get the number of sequences for a specific taxid
def get_sequence_count(taxid):
    handle = Entrez.esearch(db="nucleotide", term=f"txid{taxid}[Organism]", retmax=0)
    record = Entrez.read(handle)
    handle.close()
    return int(record["Count"])


# Function to get the taxid for a specific taxon
def get_taxid(taxon):
    handle = Entrez.esearch(db="taxonomy", term=taxon)
    record = Entrez.read(handle)
    handle.close()
    return record["IdList"][0] if record["IdList"] else None


# List of taxa
taxa = [
    "Eukaryota",
    "Porifera",
    "Cnidaria",
    "Ctenophora",
    "Placozoa",
    "Xenacoelomorpha",
    "Echinodermata",
    "Chordata",
    "Hemichordata",
    "Nematoda",
    "Nematomorpha",
    "Arthropoda",
    "Onychophora",
    "Tardigrada",
    "Priapulida",
    "Kinorhyncha",
    "Loricifera",
    "Platyhelminthes",
    "Nemertea",
    "Annelida",
    "Mollusca",
    "Brachiopoda",
    "Bryozoa",
    "Acanthocephala",
    "Gastrotricha",
    "Entoprocta",
    "Cycliophora",
    "Phoronida",
    "Rotifera",
    "Chaetognatha",
    "Gnathostomulida",
    "Dicyemida",
    "Orthonectida",
]

# Create a list to store the results
data = []

for taxon in taxa:
    if taxid := get_taxid(taxon):
        count = get_sequence_count(taxid)
        data.append((taxon, taxid, count))

# Create a pandas DataFrame
df = pd.DataFrame(data, columns=["Taxon", "TaxID", "SequenceCount"])

# Display the DataFrame
print(df)
