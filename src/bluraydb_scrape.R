#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_scrape.R
# Description:   Scrape Blu-ray.com community title data
# Author:        Brandon Monier
# Created:       2018-08-16 at 16:55:05
# Last Modified: 2018-08-17 at 12:42:14
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to scrape community
#    metadata from https://www.blu-ray.com in order to get my current
#    collection of movie titles. This data will be used to scrape
#    IMDB for additional metadata and eventually be added to my
#    personal movie collection database.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages
library(dplyr, quietly = TRUE)
library(rvest, quietly = TRUE)
library(stringr, quietly = TRUE)


## Get URL base parameters

### Working
con_base <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page="

### Out of bounds test
con_bad  <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page=40"

### Test page
con_test <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page=4"



# === Web scraping ==================================================

## Scrape movie titles

### Create objects
title_data <- list()
media_data <- list()
url_data   <- list()
i          <- 0      # start at page "0"

### Repeat until all data is scraped
repeat {
    ## Console messages
    message(paste0("Scraping page: ", i + 1))
    con <- paste0(con_base, i)

    ## Scrape titles
    titles <- xml2::read_html(con) %>%
        rvest::html_nodes(".middle .noline h3") %>%
        rvest::html_text()

    ## Scrape physical media
    media <- xml2::read_html(con) %>%
        rvest::html_nodes(".middle a") %>%
        rvest::html_attr("title") %>%
        unique() %>%
        purrr::discard(is.na)

    ## Scrape URL data
    # urls <- xml2::read_html(con) %>%
    #     rvest::html_nodes(".middle a") %>%
    #     rvest::html_attr("href") %>%
    #     .[dplyr::matches("https://www.blu-ray.com/(movies|dvd)", vars = .)] %>%
    #     unique()

    urls <- con %>%
        xml2::read_html() %>%
        rvest::html_nodes(".middle .noline") %>%
        rvest::html_attr("href")

    ## Break out
    if (length(titles) == 0) {
        break()
    } else {
        title_data[[i + 1]] <- titles
        media_data[[i + 1]] <- media
        url_data[[i + 1]] <- urls
        i <- i + 1
    }
}

### Remove unwanted variables
rm(urls, titles, media, i, con)

### Convert lists to vectors
# title_data <- title_data %>% unlist()
media_data <- media_data %>% unlist()
# url_data <- url_data %>% unlist()

### Create title tibble
url_df <- tibble::tibble(
    title = title_data %>% unlist(),
    bluray_url   = url_data %>% unlist()
)


## Get IMDB URLs...

### Make object
imdb_urls <- vector("character", length(url_df[[1]]))

### Scrape the URL (slow)...
for (i in seq_along(url_df[[1]])) {
    ## Message info
    message(paste0("Scraping IMDB urls for title: ", url_df[["title"]][i]))

    ## Iterate (slow)
    imdb_urls[[i]] <- url_df[["bluray_url"]][i] %>%
        xml2::read_html() %>%
        rvest::html_nodes("#content_overview div div center table") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        unique() %>%
        .[dplyr::matches("https://www.imdb.com/title", vars = .)] %>%
        {if (length(.) == 0) NA else .}
}

### Add IMDB url info to prior tibble
url_df$imdb_url <- imdb_urls; rm(imdb_urls)



# === Tests =========================================================

# ## Subsets
# dvd_test <- 685 # bad test: 685; good test: 703
#
#
# meta_data <- url_data[dvd_test] %>%
#     xml2::read_html() %>%
#     rvest::html_nodes(".subheading") %>%
#     rvest::html_text() %>%
#     .[2] %>%
#     stringr::str_split(pattern = " \\| ") %>%
#     unlist() %>%
#     stringr::str_replace(pattern = "\\\n|Rated ", replacement = "")
#
# imdb_url <- url_data[dvd_test] %>%
#     xml2::read_html() %>%
#     rvest::html_nodes("#imdb_icon") %>%
#     rvest::html_attr("href")
#
# media_type <- dplyr::if_else(
#     condition = grepl(
#         pattern = "https://www.blu-ray.com/movies",
#         x = url_data[dvd_test]
#     ),
#     true = "Blu-ray",
#     false = "DVD"
# )
#
# meta_data <- list(
#     bluray_url = url_data[dvd_test],
#     media_type = media_type,
#     imdb_url   = imdb_url
# )
#
# links[17] %>%
#     xml2::read_html() %>%
#     rvest::html_nodes("#content_overview div div center table") %>%
#     rvest::html_nodes("a") %>%
#     rvest::html_attr("href") %>%
#     unique() %>%
#     .[dplyr::matches("https://www.imdb.com/title", vars = .)]












