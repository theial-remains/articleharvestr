from __future__ import print_function
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException, WebDriverException
from selenium.webdriver.support.select import Select
from mailmerge import MailMerge
from venmo_api import Client
import datetime
import os
import win32com.client as win32
import calendar
import pdfkit
import string
from langdetect import detect
import json
import urllib.request
import requests
import time as time_clock
from termilighter import log
from requests_html import HTMLSession


# Path to your WebDriver (change this to your actual path)
chrome_options = Options()
chrome_options.add_argument("--ignore-certificate-errors")
chrome_options.add_argument("--ignore-ssl-errors")
chrome_options.add_argument('--log-level=3')
chrome_options.add_argument("--disable-web-security")
chrome_options.add_argument("--allow-insecure-localhost")
driver = webdriver.Chrome(options=chrome_options)
driver.set_page_load_timeout(45)
MAX_RETRIES = 3

with open("output.txt", 'r') as fp:
    years = json.load(fp)
    
def download_pdf_with_requests(url, output_path):
    session = HTMLSession()
    response = session.get(url)
    response.html.render()
    pdfkit.from_string(response.html.html, output_path)

try: 
    with open('adapted_progress.json', 'r') as f: 
        progress = json.load(f)
        dic = progress.get("dic", {})
        unable_to_load_urls = progress.get("unable_to_load_urls", [])
        non_usable_urls = progress.get("non_usable_urls", [])
        urls_to_retest = progress.get("urls_to_retest", [])
except FileNotFoundError:
    dic = {}
    unable_to_load_urls = []
    non_usable_urls = []
    urls_to_retest = []
    
def save_progress():
    with open('adapted_progress.json', 'w') as f:
        json.dump({"dic": dic, "unable_to_load_urls": unable_to_load_urls, "non_usable_urls": non_usable_urls}, f)

try: 
    for i, year in enumerate(years.keys()):
        print("The year is ", year, ". The index is ", i, ".")
        for j, month in enumerate(years[year].keys()):
            print("The month number is", month, ". The index is", j, ".")
            for l, day in enumerate(years[year][month].keys()):
                print("The day of the month is", day, ". The index is", l, ".")
                for p, time in enumerate(years[year][month][day].keys()):
                    print("The time is", time, ". The index is", p, ".")
                    for k, article in enumerate(years[year][month][day][time]):
                        if dic.get(year, {}).get(month, {}).get(day, {}).get(time, {}).get(article) or (article in unable_to_load_urls) or (article in non_usable_urls):
                            print("Skipping already processed article:", article)
                            continue
                        
                        try: 
                            print("Processing article:", article)
                            dic.setdefault(year, {}).setdefault(month, {}).setdefault(day, {}).setdefault(time, {}).setdefault(article, {})
                            for attempt in range(MAX_RETRIES):
                                try:
                                    driver.get(article)
                                    
                                    paragraphs = driver.find_elements(By.TAG_NAME, 'p') 
                                    all_text = " ".join([paragraph.text for paragraph in paragraphs if paragraph.text.strip()])
                                    if all_text:
                                        language = detect(all_text)
                                        author_line = driver.find_elements(By.CLASS_NAME, "entry__byline__author")
                                        if language == "en" and author_line:
                                            headline = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "headline"))).text
                                            header = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "entry__header")))
                                            section = header.find_element(By.XPATH, './/span').text 
                                            tags = [tag.text for tag in header.find_elements(By.XPATH, './/a[@data-vars-subunit-name="tags"]')]
                                            summary = driver.find_element(By.CLASS_NAME, "dek").text
                                            authors = []
                                            for a_line in author_line:
                                                author_name = a_line.find_element(By.TAG_NAME, "span").text.split(",")[0].strip()
                                                authors.append(author_name)
                                            dic[year][month][day][time][article].update({
                                                "headline": headline,
                                                "section": section,
                                                "tags": tags,
                                                "summary": summary,
                                                "author": authors if len(authors) > 1 else authors[0],
                                                "text": all_text
                                            })
                                            for i in authors:
                                                path = "C:\\Users\\nerdb\\Documents\\STAT-EM\\HuffingtonPost\\" + i 
                                                os.makedirs(path, exist_ok=True)
                                                dateofCovid = datetime.datetime(2020, 1, 21, 0, 0, 0)
                                                hours, minutes, seconds = map(int, time.split(":"))
                                                dateofArticle = datetime.datetime(int(year), int(month), int(day), hours, minutes, seconds)
                                                if dateofArticle >= dateofCovid:
                                                    path1 = path + "\\Covid"
                                                    os.makedirs(path1, exist_ok=True)
                                                    response = urllib.request.urlopen(article)
                                                    filename = path1 + "\\" + headline + ".pdf"
                                                    if not os.path.exists(filename):
                                                        download_pdf_with_requests(article, filename)
                                                if dateofArticle < dateofCovid:
                                                    path1 = path + "\\PreCovid"
                                                    os.makedirs(path1, exist_ok=True)
                                                    response = urllib.request.urlopen(article)
                                                    filename = path1 + "\\" + headline + ".pdf"
                                                    if not os.path.exists(filename):
                                                        r = requests.get(article, stream=True)
                                                        with open(filename, 'wb') as fd: 
                                                            for chunk in r.iter_content(chunk_size):
                                        else:
                                            log.error(
                                                text=f"Cannot use article that have no named author or that is not in english. The language is {language}.", 
                                                prefix=True, 
                                                tag="Non-english text or text without named author"
                                            )
                                            non_usable_urls.append(article)
                                        save_progress()
                                        break
                                except WebDriverException as e: 
                                    log.error(
                                        text=f"Attempt {attempt + 1} failed: {e}", 
                                        prefix=True, 
                                        tag="Web Driver Exception Error!"
                                    )
                                    if attempt == MAX_RETRIES - 1: 
                                        unable_to_load_urls.append(article)
                                        save_progress()
                                    else: 
                                        time_clock.sleep(2 ** attempt)
                        except KeyboardInterrupt:
                            log.error(
                                text="A keyboard interruption occurred",
                                prefix=True,
                                tag="KeyboardInterrupt" 
                            )
                            save_progress()
                            driver.quit()
                            raise
                        except WebDriverException as e:
                            response = requests.get(article)
                            if response.status_code == 200:
                                log.error(
                                    text=f"WebDriverException occurred while trying to load {article}: {e}. The is still usable because response status code is {response.status_code}.",
                                    prefix=True,
                                    tag="Web Driver Exception"
                                )
                                if article not in urls_to_retest:
                                    urls_to_retest.append(article)
                            else:
                                log.error(
                                    text=f"WebDriverException occurred while trying to load {article}: {e}. This is non-usable because response status code is {response.status_code}",
                                    prefix=True,
                                    tag="Web Driver Exception"
                                )
                                unable_to_load_urls.append(article)
                            save_progress()
                        except Exception as e:
                            log.error(
                                text=f"Unexpected error with article: {str(e)}", 
                                prefix=True, 
                                tag="UnexpectedError"
                            )
                            unable_to_load_urls.append(article)
                            save_progress()
                        time_clock.sleep(3)
finally: 
    log.error(
        text="Exiting program... saving progress.",
        prefix=True, 
        tag="Finished!"
    )
    save_progress()
    driver.quit()
with open('adapted_final.json', 'w') as f:
    json.dump(dic, f)