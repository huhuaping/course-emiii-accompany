# load necessary R  packages

# system
library(rmarkdown)
library(renv)       # package management
library(yaml)       # quarto project

# basic pkgs
library(tidyverse)  # most useful
library(magrittr)   # pipe %>%
library(knitr)      # R render engine

# read file
library(openxlsx)  # for excel file
library(foreign)   # for stata file


# viz
library("ggplot2")       # graph
library("DT")            # DT table
library("webshot2")      # snapshot
library("htmlwidgets")   # interactivity
#library("fontawesome")


# github repo
#renv::install("KWB-R/kwb.utils")
#renv::install("huhuaping/xmerit")  # developed by `Huhuaping`
#library(xmerit)                    # export Latex equation

