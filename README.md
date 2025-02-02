# WEB-SCRAPING_IMDb

IMDb Top 250 Web Scraper

This project is an R script designed to extract, process, and analyze data from IMDb's Top 250 movies. Additionally, it enriches the retrieved data with budget information using the TMDb API.

## 📌 Features

Web scraping IMDb to obtain the list of the Top 250 movies.

Data processing in R using libraries such as rvest, dplyr, and jsonlite.

Retrieving each movie's budget via the TMDb API.

Statistical analysis and data visualization with ggplot2.

## 📦 Dependency Installation

To run the script, ensure you have the following R packages installed:

install.packages(c("rvest", "jsonlite", "dplyr", "purrr", "httr", "forcats", "ggplot2", "tidyr", "stringr", "gridExtra", "scales"))

## 🚀 Usage

Run the script in RStudio or an R session.

The script will scrape data from IMDb and enrich it with budget information from TMDb.

Various visualizations and statistical analyses will be generated.

## 📊 Analyses Conducted

Distribution of average movie ratings.

Relationship between release year and ranking position.

Analysis of movie budgets, adjusted for inflation.

Frequency of movie genres.

Correlation between movie duration and ranking position.

## 🔑 API Keys

To retrieve data from TMDb, an API key is required. Modify the following script variables with your key:

api_key <- "YOUR_API_KEY"
access_token <- "YOUR_ACCESS_TOKEN"

## 📄 License

This project is distributed under the MIT License.

## 🔧 Possible Additions

Correct for inflation: the countries of production of the films were not considered. To improve the comparison of budgets between films it is necessary to consider the country of production of the film and use the CPI of that country in the specific year.

Enjoy exploring movie data! 🎬🍿



