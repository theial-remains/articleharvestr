import xml.etree.ElementTree as ET
import requests
import urllib.request as urllib2
import xmltodict
import json
import pandas as pd
from bs4 import BeautifulSoup
from lxml import etree
from datetime import datetime

def parse_datetime_string(dt_string):
    # Parse the string into a datetime object
    dt_object = datetime.fromisoformat(dt_string[:-1])  # Remove 'Z' for parsing

    # Extract year, month, day, and time
    year = dt_object.year
    month = dt_object.month
    day = dt_object.day
    time = dt_object.strftime("%H:%M:%S")  # Format time as HH:MM:SS
    
    return year, month, day, time

xml_dict = {}

url = "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
res = requests.get(url)
raw = xmltodict.parse(res.text)

data = [r["loc"] for r in raw["sitemapindex"]["sitemap"]]
arr = {}

for i in range(len(data)): 
    print("Starting lap ", i, " of ", len(data), " in first loop")
    url_to_be = data[i]
    res_to_be = requests.get(url_to_be)
    raw_to_be = xmltodict.parse(res_to_be.text)
    if type(raw_to_be['urlset']['url']) == dict:
        print("Starting lap 1 of 1 in if statement")
        loc = raw_to_be['urlset']['url']['loc']
        if year not in arr.keys():
            arr[year] = {month: {day: {time: [loc]}}}
        elif month not in arr[year].keys(): 
            arr[year][month] = {day: {time: [loc]}}
        elif day not in arr[year][month].keys():
            arr[year][month][day] = {time: [loc]}
        elif time not in arr[year][month][day].keys():
            arr[year][month][day][time] = [loc]
        elif time in arr[year][month][day].keys():
            arr[year][month][day][time].append(loc)
    elif type(raw_to_be['urlset']['url']) == list:
        for j in range(len(raw_to_be['urlset']['url'])):
            print("Starting lap ", j, " of ", len(raw_to_be['urlset']['url']), " in second loop")
            loc = raw_to_be['urlset']['url'][j]['loc']
            lastmod = raw_to_be['urlset']['url'][j]['lastmod']
            year, month, day, time = parse_datetime_string(lastmod)
            if year not in arr.keys():
                arr[year] = {month: {day: {time: [loc]}}}
            elif month not in arr[year].keys(): 
                arr[year][month] = {day: {time: [loc]}}
            elif day not in arr[year][month].keys():
                arr[year][month][day] = {time: [loc]}
            elif time not in arr[year][month][day].keys():
                arr[year][month][day][time] = [loc]
            elif time in arr[year][month][day].keys():
                arr[year][month][day][time].append(loc)
            
f = open('output.txt', 'w')
json.dump(arr, f)
f.close()
