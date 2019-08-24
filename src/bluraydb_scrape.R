#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_scrape.R
# Description:   Scrape Blu-ray.com community title data
# Author:        Brandon Monier
# Created:       2018-08-16 at 16:55:05
# Last Modified: 2019-08-24 at 10:08:51
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



# === Functions =====================================================

## Convert IMDB string time to total minutes
time_converter <- function(time_string) {
    tmp <- time_string %>%
        stringr::str_split(pattern = "h") %>%
        unlist() %>%
        stringr::str_replace(pattern = "min", replacement = "") %>%
        as.numeric()

    if (length(tmp) == 2) {
        tmp[1] <- tmp[1] * 60
        min_sum <- sum(tmp)
    } else if (length(tmp) == 1) {
        min_sum <- tmp
    } else {
        stop("Incorrect time usage")
    }

    return(as.numeric(min_sum))
}


## Date converter
date_converter <- function(date_string) {
    tmp <- date_string %>%
        stringr::str_replace(pattern = "\\)", replacement = "") %>%
        stringr::str_replace(pattern = "\\(", replacement = " ") %>%
        stringr::str_split(pattern = " ") %>%
        unlist()

    return(tmp)
}




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
url_df$imdb_url <- imdb_urls; rm(imdb_urls, i)


## Get IMDB metadata
link_meta <- list()
for (i in seq_along(url_df[[1]])) {
    ## Message info
    message(paste0("Scraping IMDB metadata for title: ", url_df[["title"]][i]))

    ## Check
    if (is.na(url_df[["imdb_url"]][i])) {
        meta <- list(
            year          = NA,
            rating        = NA,
            run_time_min  = NA,
            genres        = NA,
            release_date  = NA,
            country       = NA,
            summary       = NA,
            director      = NA,
            writers       = NA,
            stars         = NA,
            avg_score     = NA,
            total_ratings = NA
        )
    } else {
        ## Get temporary connection for each element in tibble
        tmp_con <- url_df[["imdb_url"]][i] %>%
            xml2::read_html()

        ## Scrape metadata
        message("  - scraping meta string...")
        meta <- tmp_con %>%
            rvest::html_nodes(".subtext") %>%
            rvest::html_text() %>%
            stringr::str_split("\\|") %>%
            unlist() %>%
            stringr::str_replace_all(pattern = c(" |\\n"), replacement = "")

        if (length(meta) == 3) {
            meta <- c(NA, meta)
        }

        message("  - scraping year...")
        year <- tmp_con %>%
            rvest::html_nodes(".title_wrapper #titleYear") %>%
            rvest::html_text() %>%
            stringr::str_replace(pattern = "\\(", replacement = "") %>%
            stringr::str_replace(pattern = "\\)", replacement = "")

        message("  - scraping summary...")
        summary <-  tmp_con %>%
            rvest::html_nodes(".summary_text") %>%
            rvest::html_text() %>%
            stringr::str_replace_all(pattern = "\\n", replacement = "") %>%
            stringr::str_replace(pattern = "See full summary.*", replacement = "") %>%
            stringr::str_trim(side = "both")

        message("  - scraping credits...")
        cred_data <- tmp_con %>%
            rvest::html_nodes(".credit_summary_item") %>%
            rvest::html_text() %>%
            stringr::str_replace_all(pattern = "\\\n", " ") %>%
            stringr::str_replace_all(pattern = "\\|.*", " ") %>%
            stringr::str_trim(side = "both") %>%
            stringr::str_replace_all(pattern = ".*: ", "") %>%
            unlist()

        message("  - scraping IMDB scores...")
        scores <- tmp_con %>%
            rvest::html_nodes(".ratingValue") %>%
            rvest::html_nodes("strong") %>%
            rvest::html_attr("title") %>%
            stringr::str_replace(pattern = " based on ", replacement = " ") %>%
            stringr::str_replace(pattern = " user ratings", replacement = "") %>%
            stringr::str_replace(pattern = ",", replacement = "") %>%
            stringr::str_split(pattern = " ") %>%
            unlist()

        ## Convert time and dates from string data
        meta[2]  <- time_converter(meta[2])
        date_tmp <- date_converter(meta[4])

        ## Output metadata to list
        meta <- list(
            year          = year %>% as.numeric(),
            rating        = meta[1],
            run_time_min  = meta[2] %>% as.numeric(),
            genres        = meta[3] %>% stringr::str_split(pattern = ",") %>% unlist(),
            release_date  = date_tmp[1] %>% lubridate::as_date(format = "%d%B%Y", tz = ""),
            country       = date_tmp[2],
            summary       = summary,
            director      = cred_data[1] %>% stringr::str_split(pattern = ", ") %>% unlist(),
            writers       = cred_data[2] %>% stringr::str_split(pattern = ", ") %>% unlist(),
            stars         = cred_data[3] %>% stringr::str_split(pattern = ", ") %>% unlist(),
            avg_score     = scores[1] %>% as.numeric(),
            total_ratings = scores[2] %>% as.numeric()
        )
    }

    ## Populate main list
    link_meta[[url_df[["title"]][i]]] <- meta
}
