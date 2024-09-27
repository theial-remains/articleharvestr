setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")
rm(list = ls())

devtools::load_all()

# testing find_schema_elements
check_schema("https://www.example.com")

schema <- pull_schema("https://www.example2.com")
print(schema)

overwrite <- prompt_overwrite("https://www.example.com")
if (overwrite) {
  # Proceed with overwriting the schema
  write_schema("https://www.example.com", author_element, title_element, date_element, text_element)
} else {
  message("No changes made to the schema.")
}

write_schema(
  website_url = "https://www.example.com",
  author_element = ".author",
  title_element = ".title",
  date_element = ".pubdate",
  text_element = ".content"
)