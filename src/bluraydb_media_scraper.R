#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_media_scraper.R
# Description:   Scrape blu-ray.com for media metadata
# Author:        Brandon Monier
# Created:       2020-03-28 at 21:18:48
# Last Modified: 2020-03-29 at 12:01:57
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to scrape through all
#    pages of my blu-ray.com profile in order to obtain media
#    information.
#
# Note:
#    This script will only parse metadata related to media (i.e.
#    physical disc information).
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load libraries ----
library(forcats)
library(ggplot2)
library(magrittr)
library(readr)
library(rvest)
library(tibble)
library(xml2)


## Variables ----

### Base
con_base <- "https://www.blu-ray.com/community/collection.php?u=483309&categoryid="

### Media types
media <- c("dvd" = "21", "blu_ray" = "7", "4K" = "7&4k=1")


## Load data (if available) ----
if (file.exists("data/media_data.csv")) {
    media_check <- readr::read_csv(file = "data/media_data.csv")
} else {
    media_check <- NULL
}



# === Scrape blu-ray.com ============================================

## Get HTML data ----
media_data <- list()
for (i in seq_len(length(media))) {

    message("\n\n=== Parsing media: ", names(media)[i], " ===\n")
    con_base_med <- paste0(con_base, media[[i]], "&page=")

    j <- 0
    url_data   <- list()
    title_data <- list()

    repeat {
        con <- paste0(con_base_med, j)

        urls <- xml2::read_html(con) %>%
            rvest::html_nodes(".hoverlink") %>%
            rvest::html_attr("href")

        titles <- xml2::read_html(con) %>%
            rvest::html_nodes(".hoverlink") %>%
            rvest::html_attr("title")

        ## Break out
        if (length(titles) == 0) {
            message("Finished!")
            break()
        } else {
            message("Scraping page ", j + 1, " ...")
            message("   -- ", con)
            url_data[[j + 1]]   <- urls
            title_data[[j + 1]] <- titles
            j <- j + 1
        }

    }

    url_data   <- url_data %>% unlist()
    title_data <- title_data %>% unlist()


    if (!is.null(media_check)) {
        url_data <- url_data[which(!(title_data %in% media_check$title))]
        title_data <- setdiff(title_data, media_check$title)
    }

    # Get unique IDs from blu-ray.com
    br_ids <- url_data %>%
        stringr::str_extract(pattern = "/(\\d*)/$") %>%
        stringr::str_replace_all("/", "") %>%
        as.numeric()



    ## Set up data ----
    meta_len       <- length(url_data)
    distributor    <- vector(mode = "character", length = meta_len)
    genres         <- vector(mode = "character", length = meta_len)
    ratings        <- vector(mode = "numeric",   length = meta_len)
    bluray_ratings <- vector(mode = "character", length = meta_len)
    media_img      <- vector(mode = "character", length = meta_len)
    release_date   <- vector(mode = "character", length = meta_len)


    message("\n--- Parsing ", names(media)[i], " web data...")

    ## Go through each title ----
    for (k in seq_len(meta_len)) {

        message("Getting data for title: ", title_data[k])

        html_data <- url_data[k] %>%
            xml2::read_html()

        # Media distributor
        tmp_dist <- html_data %>%
            rvest::html_nodes("span.subheading.grey a.grey") %>%
            rvest::html_text()
        if (length(tmp_dist) == 2 || length(tmp_dist) == 1) {
            distributor[k] <- "No title given."
        } else {
            distributor[k] <- tmp_dist[1]
        }

        # Run time
        genres[k] <- html_data %>%
            rvest::html_nodes("div.genreappeal") %>%
            rvest::html_text() %>%
            paste(sep = "", collapse = "; ")

        # Ratings
        ratings[k] <- html_data %>%
            rvest::html_nodes("td.middle") %>%
            rvest::html_text() %>%
            .[2] %>%
            as.numeric()


        # Blu-ray ratings
        if (names(media)[i] == "dvd") {
            bluray_ratings_tmp <- c("Video\n\n0.0Audio\n\n0.0Extras\n\n0.0")
        } else {
            bluray_ratings_tmp <- html_data %>%
                rvest::html_nodes("div#bluray_rating table") %>%
                rvest::html_text()
        }

        bluray_ratings_tmp <- bluray_ratings_tmp %>%
            stringr::str_split("(?<=[0-9].[0-9])(?=[A-Za-z]|4K)") %>%
            unlist() %>%
            stringr::str_replace("\n\n", " = ")

        if (!any(grepl("^3D", bluray_ratings_tmp))) {
            bluray_ratings_tmp <- c("3D = 0.0", bluray_ratings_tmp)
        }
        if (!any(grepl("^4K", bluray_ratings_tmp))) {
            bluray_ratings_tmp <- c("4K = 0.0", bluray_ratings_tmp)
        }
        if (!any(grepl("^Video", bluray_ratings_tmp))) {
            bluray_ratings_tmp <- c("Video = 0.0", bluray_ratings_tmp)
        }
        if (!any(grepl("^Audio", bluray_ratings_tmp))) {
            bluray_ratings_tmp <- c("Audio = 0.0", bluray_ratings_tmp)
        }
        if (!any(grepl("^Extras", bluray_ratings_tmp))) {
            bluray_ratings_tmp <- c("Extras = 0.0", bluray_ratings_tmp)
        }
        bluray_ratings_tmp <- bluray_ratings_tmp[order(bluray_ratings_tmp)]
        bluray_ratings[k] <- paste(bluray_ratings_tmp, collapse = "; ")


        # Media image
        media_img[k] <- html_data %>%
            rvest::html_nodes("img#frontimage_overlay.coverfront") %>%
            rvest::html_attr("src")

        # Release date
        release_date[k] <- html_data %>%
            rvest::html_nodes("span.subheading.grey a.grey") %>%
            rvest::html_text() %>%
            .[3]
    }

    movie_data <- tibble::tibble(
        title          = title_data[seq_len(meta_len)],
        br_id          = br_ids[seq_len(meta_len)],
        media_type     = names(media)[i],
        distributor    = distributor,
        genres         = genres,
        rating         = ratings,
        bluray_ratings = bluray_ratings,
        media_img      = media_img,
        release_date   = release_date
    )

    media_data[[i]] <- movie_data
}


## Row bind media types ----
media_data_final <- do.call("rbind", media_data)


## Write flat file to disk ----

### Check for prior data
if (!is.null(media_check)) {
    append <- TRUE
} else {
    append <- FALSE
}

### Write or append
readr::write_csv(
    x = media_data_final,
    path = "data/media_data.csv",
    append = append
)


