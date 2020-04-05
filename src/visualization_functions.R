#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   visualization_functions.R
# Description:   Functions for visualizing data
# Author:        Brandon Monier
# Created:       2020-04-04 at 22:42:42
# Last Modified: 2020-04-05 at 00:41:55
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to create concepts for
#    visualizing media data.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(readr)


## Load data ----
media_data <- readr::read_csv(file = "data/media_data.csv")


## Parameters ----
film <- 912 # WIP



# === Data processing ===============================================

## Parse media ratings ----
tmp_ratings <- media_data[film, ]$bluray_ratings %>%
    strsplit(split = ";") %>%
    unlist() %>%
    stringr::str_trim() %>%
    strsplit(split = " = ") %>%
    unlist() %>%
    matrix(data = ., nrow = length(.) / 2, byrow = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    set_colnames(c("property", "value")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
        property = factor(
            property,
            levels = c("3D", "4K", "Video", "Audio", "Extras")
        ),
        value = value %>% as.numeric()
    )



# === Visualizations ================================================

## Visualize ratings ----
tmp_ratings %>%
    ggplot() +
    aes(x = property, y = value) +
    geom_col() +
    ylim(0, 5) +
    scale_x_discrete(limits = rev(levels(tmp_ratings$property))) +
    ggtitle(paste("Ratings for:", media_data[film, ]$title)) +
    coord_flip() +
    theme_minimal() +
    theme(
        aspect.ratio = 0.5,
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10)
    )


## Distribution of distributors... ----
media_data %>%
    ggplot() +
    aes(x = forcats::fct_infreq(distributor) %>% forcats::fct_rev()) +
    geom_bar() +
    coord_flip() +
    xlab("Distributor") +
    ylab("Number of films")
