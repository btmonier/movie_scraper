#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_scrape.R
# Description:   Scrape Blu-ray.com community title data
# Author:        Brandon Monier
# Created:       2018-08-16 at 16:55:05
# Last Modified:
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
library(rvest)


## Get URL base parameters
con_base <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page="
con_bad  <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page=40"
con_test <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid"



# === Web scraping ==================================================

## Scrape movie titles
title_data <- list()
media_data <- list()
url_data <- list()
i <- 0
repeat {
    ## Console messages
    message(paste0("Scraping page: ", i))
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
    urls <- xml2::read_html(con) %>%
        rvest::html_nodes(".middle .noline") %>%
        rvest::html_attr("href") %>%
        unique()

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
title_data <- title_data %>% unlist()
media_data <- media_data %>% unlist()
url_data <- url_data %>% unlist()














