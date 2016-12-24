#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
@author: James Wang
"""

import requests
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
import re
# from collections import defaultdict

# load data
with open("zip_codes.csv", "r") as f:
    zips = [i.splitlines() for i in f][0][1:]

zips_attributes = pd.read_csv("zip_codes_age_popdense.csv")
zips_attributes.columns = ["zip", "age", "popden"]

# scrape data
url_start = "https://www.incomebyzipcode.com/washington/"
zip_data = np.zeros((1, 5))

for i in zips:
    # get link
    url = url_start + i
    # make request
    r = requests.get(url)
    s = BeautifulSoup(r.text, "html5lib")
    # get data
    names_tags = s.find_all("div", {"class" : "data-block"})
    names = [[j.strip() for j in names_tags[k].text.splitlines() if j.strip()][0]
             for k in range(len(names_tags))]
    data_tags = s.find_all("td", {"class" : "hilite"})
    data = [re.findall(r"[\w,.]+", j)[0] for j in [j.text for j in data_tags]]
    data = [j.replace(",", "") for j in data]
    # add to dataframe
    colnames = ["zip"] + names
    colvalues = [i] + data[:4]
    zip_data = np.vstack((zip_data, colvalues))

# change data types
zip_data = zip_data.astype(float)
zip_data[:, 4] = zip_data[:, 4] / 100

# create dataframe
zip_codes_income = pd.DataFrame(zip_data[1:, :])
zip_codes_income.columns = colnames[:5]

# join dataframes
zip_codes_data = zips_attributes.join(zip_codes_income.set_index('zip'), on="zip", how="left")


# export as csv
zip_codes_data.to_csv("zip_codes_data.csv", index=False)
