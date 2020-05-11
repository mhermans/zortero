library(httr)
library(purrr)
library(dplyr)
library(jsonlite) 
library(tidyr)
library(countrycode)

api_base_url = 'https://api.zotero.org'
source(here::here('zotero_api_keys.r'))
#user_id = 
#user_api_key = 

# https://www.zotero.org/mhermans/collections/R7E2F4UJ/items/II2AUIT5/collection

r_url <- paste(api_base_url, 'users', user_id, 'collections', sep = '/')
r_url <- paste(api_base_url, 'users', user_id, 'collections/top', sep = '/')
r_url <- paste(api_base_url, 'users', user_id, 'collections/R7E2F4UJ', sep = '/')
r_url <- paste(api_base_url, 'users', user_id, 'collections/R7E2F4UJ/items', sep = '/')
r_url <- paste(api_base_url, 'users', user_id, 'collections/R7E2F4UJ/items/top', sep = '/')

r_url <- paste(api_base_url, 'users', user_id, 'items', sep = '/')
r_url <- paste(api_base_url, 'users', user_id, 'items/top', sep = '/')
#r_url <- paste(api_base_url, 'users', user_id, 'items/tags', sep = '/')

r_url <- paste(api_base_url, 'users', user_id, 'items/XHDK46DJ/children', sep = '/')

r <- GET(
  r_url, 
  add_headers(
    'Zotero-API-Version' = 3,
    'Zotero-API-Key' = user_api_key)
  #,
  # query = list(tag = "research_roundup", format = "bib", style = 'apa') # return only a formatted citation
  # query = list(tag = "research_roundup", format = "json", include='data,bib,citation', style = 'apa') # return only a formatted citation
)
r

# r <- GET(
#   r_url, 
#   add_headers(
#     'Zotero-API-Version' = 3,
#     'Zotero-API-Key' = user_api_key),
#   query = list(itemType = 'book'))
# r
# content(r, as = 'text', encoding = 'utf-8')


# Get pdf of article
https://www.zotero.org/mhermans/items/WHSILW8A/file
=> WHSILW8A key for child

# Get a note that is a child-item to an article



r_headers <- headers(r)

as.integer(r_headers$`total-results`)
as.integer(r_headers$`last-modified-version`)

r_headers$link






d <- fromJSON(content(r, as = 'text', encoding = 'utf-8'))
d %>% glimpse()

d$data %>%
  unnest()

d$data %>%
  unnest(cols = c(tags))


d$data %>%
  #select(key, tags) %>%
  unnest(cols = c(tags)) %>%
  filter(tag %in% codelist$iso3c) %>%
  distinct(key, .keep_all = TRUE)


  print(n=60)


 %>% glimpse()
content(r)[[1]]


# POC get recent research articles with info
# ##########################################

# query all top-level items, for those journal articles with tag research_roundup
r_url <- paste(api_base_url, 'users', user_id, 'items/top', sep = '/')
r <- GET(
  r_url, 
  add_headers('Zotero-API-Version' = 3, 'Zotero-API-Key' = user_api_key),
  # additionally return citation and bibliographic reference in HTML
  query = list(tag = "research_roundup", itemType = 'journalArticle', format = "json", include='data,bib,citation', style = 'apa')
)
r

fromJSON(content(r, as = 'text', encoding = 'utf-8')) %>%
  glimpse()

# get the child note of the journal article
r <- GET(
  url = paste(api_base_url, 'users', user_id, 'items', d$data$key, 'children', sep = '/'), 
  query = list(itemType = 'note', tag = "research_roundup"), # also tagged with research_roundup
  add_headers(
    'Zotero-API-Version' = 3,
    'Zotero-API-Key' = user_api_key))
fromJSON(content(r, as = 'text', encoding = 'utf-8'))$data$note


# get the attached pdf of an article
r <- GET(
  url = paste(api_base_url, 'users', user_id, 'items', d$data$key, 'children', sep = '/'), 
  query = list(itemType = 'attachment'), # 
  add_headers(
    'Zotero-API-Version' = 3,
    'Zotero-API-Key' = user_api_key))
# GET(paste(fromJSON(content(r, as = 'text', encoding = 'utf-8'))$links$up$href, 'item', sep = '/'),
#     add_headers(
#       'Zotero-API-Version' = 3,
#       'Zotero-API-Key' = user_api_key)
#     )
