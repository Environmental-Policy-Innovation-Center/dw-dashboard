![](www/epic-logo-transparent.png)

# EPIC's Drinking Water Funding Dashboard

This repository maintains the code for running [EPIC's Drinking Water Funding Dashboard](https://www.policyinnovation.org/water/tracking-iija), as well as the necessary data cleaning and processing steps. The dashboard serves as a tool for exploring drinking water projects that states intend to fund in the next year through state revolving funds based on the type of project, the funding amount, which communities are impacted, and other features.

For important context and caveats regarding how to interpret and use this data, a glossary of terms used throughout the project, and data dictionaries for each included state, [visit the Drinking Water Funding Dashboard page](https://www.policyinnovation.org/water/tracking-iija).

The project's pipeline includes:

-   Scraping data from tables in state documents, typically stored as PDFs, into CSVs. This process must be done individually for each document. Within each state folder for a given year, a python notebook details the scraping process. Each of the resulting CSVs are available in the state's `data` folder.

-   Cleaning the raw scraped data. Within each state folder, a R script takes in the resulting scraped CSVs and details how each table is cleaned and made ready to be standardized.

-   Standardizing state project tables into a single dataset. The `state-standardization.Rmd` notebook sources the cleaning script for each state. It then combines and finalizes the cleaned data to be presented in the dashboard and integrates the external data needed for additional dashboard features.

-   Developing the dashboard. The `dw-dashboard-app.R` script contains code for creating and implementing the Shiny application for the dashboard itself.
