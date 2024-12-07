# load packages ####
library(tidyverse)
library(DBI)
library(readxl)
library(cli)
library(lubridate)
library(stringr)
library(uuid)
library(ggplot2)
library(viridis)

# load raw data ####
az_database_list <- read.csv('raw_data/az_database_list.csv')
az_database_usage <- read.csv('raw_data/database_usage_by_month.csv')
research_outputs <- read.csv('raw_data/research-outputs-by-ay-and-college.csv')

spring_enrollment <- read_xlsx('raw_data/spring-enrollment_by_college-major-and-semester.xlsx')
fall_enrollment <- read_xlsx('raw_data/fall-enrollment_by_college-major-and-semester.xlsx')
grant_funding <- read_xlsx('raw_data/funding-reports-combined_by_college-and-fy.xlsx')