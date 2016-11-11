# -*- coding: utf-8 -*-

"""
This script extracts a list of sovereign states and
dependent territories by continent from wikipedia.
"""

# Import the libraries.
import requests
from bs4 import BeautifulSoup
import codecs

url = ("https://en.wikipedia.org/wiki/List_of_sovereign" +
       "_states_and_dependent_territories_by_continent")
response = requests.get(url, params={"action": "render"}, timeout=10)
soup = BeautifulSoup(response.content, "lxml")
tables = soup.findAll("table", class_="wikitable sortable")

# Loop over the tables (one for each continent) and create a list of countries.
continents = {}
for table in tables:
    continent = table.find_previous_sibling("h2").find("span").contents[0]
    countries = []
    for row in table.findAll("tr"):
        try:
            countries.append(row.findAll("a")[1].contents[0])
        except IndexError:
            pass
    continents[continent] = countries

# Create a text file.
output = codecs.open("../R/summer_olympics/data/continents.txt", "w", "utf-8")
output.write("continent;country\n")

for key, value in sorted(continents.items()):
    for country in value:
        output.write(key + ";" + country)
        output.write("\n")

output.close()
