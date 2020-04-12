#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_movie_scraper.R
# Description:   Scrape blu-ray.com for movie metadata
# Author:        Brandon Monier
# Created:       2020-04-11 at 22:04:54
# Last Modified: 2020-04-12 at 18:50:40
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to scrape through all
#    pages of my blu-ray.com profile in order to obtain movie
#    information.
#
# Note:
#    This will extract information related the film that is
#    contained on related media.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load libraries ----
library(magrittr)
library(rvest)
library(stringr)
library(tibble)
library(xml2)


## Variables ----

### Base
con_base <- paste0(
    "https://www.blu-ray.com/community/collection.php",
    "?u=483309&action=hybrid&page="
)


## Functions ----

### Create cross-ref table for movies and media IDs
###   @param urls........ scraped whole vector of URLs
###   @param movies...... movie specific URLs
###   @param id_spacer... default spacer ID
movieXRefMedia <- function(urls, movies, id_spacer = "MOVIE_ID") {
    urls[urls %in% movies] <- id_spacer

    tmp_str <- paste(urls, collapse = ";")

    tmp_ls <- strsplit(tmp_str, id_spacer) %>%
        unlist() %>%
        .[-1]

    tmp_ls <- lapply(seq_along(tmp_ls), function(i) {
        tmp_ls[i] %>%
            strsplit(split = ";") %>%
            unlist() %>%
            .[-1] %>%
            stringr::str_extract("/[:digit:]*/$") %>%
            stringr::str_replace("^/", "") %>%
            stringr::str_replace("/$", "") %>%
            stringr::str_c(collapse = ";")
    })

    tmp_ls <- data.frame(
        x = matrix(
            unlist(tmp_ls),
            nrow = length(tmp_ls),
            byrow = TRUE
        )
    ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(movie_url = movies) %>%
        dplyr::select(movie_url, br_id = x)

    return(tmp_ls)
}



# === Scrape blu-ray.com ============================================

## Set up initial variables ----
j <- 0
xref_data <- list()


## Iterate through each page ----
repeat {
    con <- paste0(con_base, j)

    ## Scrape for URL data
    urls <- xml2::read_html(con) %>%
        rvest::html_nodes("td.middle a") %>%
        rvest::html_attr("href")

    movies <- xml2::read_html(con) %>%
        rvest::html_nodes("td.middle a.noline") %>%
        rvest::html_attr("href")


    ## Break out
    if (length(movies) == 0) {
        message("Finished!")
        break()
    } else {
        message("Scraping page ", j + 1, " ...")
        xref_data[[j + 1]]   <- movieXRefMedia(urls, movies)
        j <- j + 1
    }
}


## Bind all data ----
xref_data <- do.call("rbind", xref_data)



# === Get film metadata (WIP) =======================================

## Get test data ----
mov_urls <- sample(xref_data$movie_url, size = 10)
mov_len  <- length(mov_urls)
mov_title <- vector("character", mov_len)
genre     <- vector("character", mov_len)
country   <- vector("character", mov_len)
language  <- vector("character", mov_len)


## Iterate (WIP) ----
for (i in seq_along(mov_urls)) {
    message("Getting film data: ", mov_urls[i])

    html_data <- mov_urls[i] %>%
        xml2::read_html()

    check <- html_data %>%
        rvest::html_nodes("div h1.eurostile") %>%
        rvest::html_text()c

    if (check == 0) {
        mov_title[i] <- NA
        genre[i]     <- NA
        country[i]   <- NA
        language[i]  <- NA
    } else {
        # Title
        mov_title[i] <- html_data %>%
            rvest::html_nodes("div h1.eurostile") %>%
            rvest::html_text() %>%
            stringr::str_replace("\t.*$", "")

        # Genre
        genre[i] <- html_data %>%
            rvest::html_nodes("div.genreappeal") %>%
            rvest::html_text() %>%
            stringr::str_c(collapse = ";")


        # Get metadata table
        data_box <- html_data %>%
            rvest::html_nodes("table.menu") %>%
            rvest::html_table() %>%
            .[[1]]

        ## Country
        country[i] <- data_box %>%
            dplyr::filter(X1 == "Country") %>%
            dplyr::select(X2) %>%
            as.character()

        ## Language
        language[i] <- data_box %>%
            dplyr::filter(X1 == "Language") %>%
            dplyr::select(X2) %>%
            as.character()
    }



    # break
}


## Make data frame object ----
mov_data_final <- tibble::tibble(
    title    = mov_title,
    genre    = genre,
    country  = country,
    language = language,
    mov_url  = mov_urls
)
rm(mov_title, genre, country, language, i)



# === Prototyping ===================================================

### TODO: Add better checks for metadate table. For example, certain
###       movie titles contain very little information which screws
###       up normal table gathering procedures.


## Initial HTML data ----
html_data <- mov_urls[10] %>%
    xml2::read_html()


## Title ----
html_data %>%
    rvest::html_nodes("h1.eurostile") %>%
    rvest::html_text() %>%
    stringr::str_replace("\t.*$", "")


## Genre ----
html_data %>%
    rvest::html_nodes("div.genreappeal") %>%
    rvest::html_text() %>%
    stringr::str_c(collapse = ";")


## Metadata box ----

### Get table
data_box <- html_data %>%
    rvest::html_node("table.menu") %>%
    rvest::html_table() %>%
    .[[1]]

### Country
data_box %>%
    dplyr::filter(X1 == "Country") %>%
    dplyr::select(X2) %>%
    as.character()

### Release date
data_box %>%
    dplyr::filter(X1 == "Language") %>%
    dplyr::select(X2) %>%
    as.character()


