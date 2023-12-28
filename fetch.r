library(httr)
library(purrr)
library(dplyr)
library(jsonlite) 
library(tidyr)
library(countrycode)
library(lubridate)


api_base_url = 'https://api.zotero.org'
source(here::here('zotero_api_keys.r'))
#user_id = 
#user_api_key = 

# items(collection_id = NULL, params, collection = NULL, top = FALSE)
# item(<item_id>, <params>)
# children(<item_id>, <params>)
# file()

items <- function(user_id, api_key = NULL, collection_id = NULL, top = TRUE, format = 'json', 
                  include = 'data,bib,citation', style = 'apa', limit = 100)  {
  
  query_url = paste('https://api.zotero.org', 'users', user_id, 'collections', collection_id, 'items', sep = '/')
  
  if (top) { query_url = paste(query_url, 'top', sep = '/')}
  
  r <- httr::GET(
    url = query_url, 
    query = list(
      #tag = "research_roundup", 
      format = format, include = include, style = style),
    add_headers(
      'Zotero-API-Version' = 3,
      'Zotero-API-Key' = user_api_key))

  reponse_items = jsonlite::fromJSON(httr::content(r, as = 'text', encoding = 'utf-8'))
  
  return(reponse_items)
}

data <- items(user_id = user_id, api_key = user_api_key, collection_id = 'R7E2F4UJ')

data %>% 
  glimpse()


# POC: get article records with formatted citations and country tags for evidence map
# ###################################################################################

r <- GET(
  url = paste(api_base_url, 'users', user_id, 'collections/R7E2F4UJ/items/top', sep = '/'), 
  query = list(
    #tag = "research_roundup", 
    format = "json", include='data,bib,citation', style = 'apa'),
  add_headers(
    'Zotero-API-Version' = 3,
    'Zotero-API-Key' = user_api_key))

records <- fromJSON(content(r, as = 'text', encoding = 'utf-8'))
records %>% glimpse()

records_countrystudies <- records$data %>%
  unnest(cols = c(tags)) %>%
  filter(tag %in% codelist$iso3c)

records_countrystudies %>% select(key, tag) %>%
  left_join(
    records %>%
      filter(key %in% records_countrystudies$key) %>%
      select(key, bib),
    by = 'key'
)




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
  # query = list(tag = "research_roundup", format = "json", include='data,bib,citation', style = 'apa')
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


# list RRR articles

# query all top-level items, for those journal articles with tag research_roundup
r <- GET(paste(api_base_url, 'users', user_id, 'items/top', sep = '/'),
  add_headers('Zotero-API-Version' = 3, 'Zotero-API-Key' = user_api_key),
  query = list(tag = "research_roundup", itemType = 'journalArticle', format = "json", include='data,bib,citation', style = 'apa')
)
response_data <- fromJSON(content(r, as = 'text', encoding = 'utf-8'))

response_data$data$title

response_data$data$abstractNote

response_data$data$url
response_data$data$DOI
response_data$citation
response_data$bib
response_data$data$date

month(ymd(response_data$data$date))




install.packages("reactable")



# experiment Vlaams Parlement
# ===========================

http://ws.vlpar.be/api/swagger/#!/search/getSearchResult
  http://ws.vlpar.be/e/opendata/api/
  
# query_url <- "http://ws.vlpar.be/api/swagger/api/search/query/HIVA?page=1&max=100&collection=feed&sort=date"


fetch_mentions <- function(query, page = 1, max = 100) {
  query_url <- paste("http://ws.vlpar.be/api/swagger/api/search/query", query, sep = '/')
  r <- httr::GET(url = query_url,
           query = list(page = as.character(page), max = as.character(max), collection = 'feed', sort ='date'),
           add_headers('Accept' = 'application/json')
  )

  response_data <- jsonlite::fromJSON(content(r, as = 'text', encoding = 'utf-8'))

  # return(r)
  return(response_data)
  
}

fetch_mentions('HIVA', page = 1)

clean_mentions <- function(response_data) {
  data <- response_data$result
  data_cleaned <- tibble(
    datum = data %>% pluck('metatags', 'metatag') %>% 
      map( ~filter(.x, name == 'datum')) %>% 
      map_chr( ~pull(.x, 'value')) %>%
      ymd(),
    legislatuur = data %>% pluck('metatags', 'metatag') %>% 
      map( ~ filter(.x, name == 'legislatuur')) %>% 
      map_chr(~pull(.x, 'value')),
    snippet = data %>% pluck('snippet'),
    title = data %>% pluck('title'),
    url = data %>% pluck('url')
  )
  
  return(data_cleaned)
  
}

r_p1 <- fetch_mentions('HIVA', page = 1)
r_p2 <- fetch_mentions('HIVA', page = 2)

str(r_p1)
str(r_p2)

mentions <- bind_rows(
  clean_mentions(fetch_mentions('HIVA', page = 1)),
  clean_mentions(fetch_mentions('HIVA', page = 2)),
  clean_mentions(fetch_mentions('HIVA', page = 3)),
  clean_mentions(fetch_mentions('HIVA', page = 4)),
  clean_mentions(fetch_mentions('HIVA', page = 5))
)

#mentions %>% purrr::pluck('metatags') %>% pluck(1)

# mentions %>% purrr::pluck('metatags', 'metatag') %>% 
#   map( ~ filter(.x, name == 'onderwerp')) %>% 
#   map( ~ select(.x, 'value'))




library(reactable)

reactable(
  mentions %>% 
    mutate(
      snippet = paste0('...', snippet, '...'),
      title_html = paste0('<a href="', url, '">', title, '</a>')) %>%
    select(legislatuur, datum, title_html, snippet, -title, -url), 
  columns = list(
    title_html = colDef(name = 'titel', html = TRUE),
    snippet = colDef(name = 'vermelding', html = TRUE)),
  striped = TRUE
)

# aggregaat
# thema
# legislatuur
# onderwerp



library(purrr)
library(crosstalk)

mentions_l <- mentions %>% 
  mutate(
    snippet = paste0('...', snippet, '...'),
    title_html = paste0('<a href="', url, '">', title, '</a>')) %>%
  select(legislatuur, datum, title_html, snippet, -title, -url)

mentions_l <- SharedData$new(mentions_l)


bscols(
  widths = c(3, 9),
  list(
    filter_slider("datum", "Datum", mentions_l, ~datum, width = "100%"),
    filter_checkbox("legislatuur", "Legislatuur", mentions_l, ~legislatuur)
    #filter_select("mfr", "Manufacturer", data, ~Manufacturer)
  ),
  
  reactable(
    mentions_l,
    columns = list(
      title_html = colDef(name = 'titel', html = TRUE),
      snippet = colDef(name = 'vermelding', html = TRUE)),
    striped = TRUE
  )
)


# install.packages('pdftools')
# library(pdftools)


