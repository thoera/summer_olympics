# -*- coding: utf-8 -*-

"""
This script extracts from wikipedia the medal tables of 
the Summer Olympics from 1896 to 2016.
"""

# Import the libraries.
import requests
from bs4 import BeautifulSoup
import pandas as pd

# Loop over the years.
for years in range(1896, 2020, 4):
    # Summer Olympics were 1916, 1940, 1944 were cancelled due to WW1 & WW2.
    if years in [1916, 1940, 1944]:
        pass
    else:
        url = ("https://en.wikipedia.org/wiki/" + str(years) + 
        "_Summer_Olympics_medal_table")
        print(url)
        print("---")
        response = requests.get(url, params={"action": "render"}, timeout=10)
        soup = BeautifulSoup(response.content, "lxml")
        
        # Try to find the "correct" table.
        try:
            # Generally it's the first one with a caption on the page.
             table = soup.find("caption").parent 
        except AttributeError:
            # But sometimes there is no caption at all.
            # In that case it can be the first or the second table of the page.
            table_test = soup.findAll("table")
            if table_test[0].find("a").text == str(years) + " Summer Olympics":
                table = table_test[1]
            else:
                table = table_test[0]
        
        medals_by_country = {}
        
        for row in table.findAll("tr"):
            try:
                # Find the names of the countries.
                country = row.find("a").contents[0]
                # Find the number of medals.
                medals = []
                for i in row.findAll("td")[-4:]:
                    medals.append(i.text)
                medals_by_country[country] = medals
            except AttributeError:
                pass
        
        header = [i.text for i in table.findAll("th", scope="col")][-5:]
        
        df = pd.DataFrame(medals_by_country).T.reset_index()
        df.columns = header
        
        # Save it.
        df.to_csv(("../R/summer_olympics/data/" + str(years) + 
        "_Summer_Olympics_medal_table.csv"), sep=",", 
        header=True, index=False, encoding="utf-8")
