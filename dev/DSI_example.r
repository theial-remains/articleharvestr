rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# step 1: write schemas to file
# write the classes of the elements
# sitemap html: fill in elements
# sitemap xml: all NULL by default, extract all loc elements sequentialy
gs_write_schema(
  website_url = "https://www.huffpost.com",
  sitemap_url = "https://www.huffpost.com/sitemaps/sitemap-v1.xml",
  author_element = "entry__byline__author",
  title_element = "headline",
  date_element = "timestamp",
  text_element = "primary-cli cli cli-text "
)

gs_check_schema("https://www.huffpost.com") # can use any url with bbc.com in it

schema <- gs_pull_schema("https://www.huffpost.com")
View(schema)

gs_remove_schema("https://www.huffpost.com", every = TRUE)


# step 2: get urls
gu_parse_sitemap_recursive(base_url = "https://www.huffpost.com/sitemaps/sitemap-v1.xml",
    levels = 1,
    start_date = "2020-01-01",
    end_date = "2020-01-03")

# step 3: write urls to csv


# step 4: scrape urls contents from csv


# step 5: save scraped information to csv


# step 6: sentiment analysis of saved information

