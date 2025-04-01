![](www/epic-logo-transparent.png)

# EPIC'S DWSRF Funding Tracker

This repository maintains the code and data powering the DWSRF Fucking Tracker. The website serves as an in-depth resource that monitors proposed funding and policies in the annual Intended Use Plans of **15** focus states. The data captured in this repository explores drinking water projects that states intend to fund in the next year through state revolving funds based on the type of project, the funding amount, which communities are impacted, and other features.

For important context and caveats regarding how to interpret and use this data, a glossary of terms used throughout the project, and data dictionaries for each included state, view the website's resources page (link forthcoming).

The project's pipeline includes:

-   **Scraping**. The first step is extracting project data from tables in state documents, typically stored as PDFs, into CSVs. This process must be done individually for each document. This step is typically done using Tabula (a Java-based GUI that can be run locally) or generative AI. For many Y1 states, python notebook details the scraping process. The resulting CSVs are available in the state's `data` folder.

-   **Cleaning.** For each year, within each state folder, an R script takes in the resulting scraped CSVs and details how each table is cleaned and made ready to be standardized. These functions follow directions provided in a data dictionary written by EPIC's Water Policy team.

-   **Standardizing.** Combining state project tables into a single dataset. The `combine-states.Rmd` notebook sources the cleaning script for each state, creating the standardized source of all project data for each state over each year.

-   **Aggregating.** The `state-dataset.Rmd` notebook groups project-level data by state. It documents feature completeness, sums of financial features like funding amount and project costs, and counts of projects by categorical features like disadvantaged status, expecting funding, and project type.

-   **Analyzing & Visualizing.** Creating data visualization to power the SRF Funding Tracker website is currently done in a series of notebooks denoted by the data source and the time frame considered. For instance, `project-data-yoy.Rmd` considers only data from the above pipeline analyzed for each year of available data. The other primary data source ("funding data") comes from a comprehensive dataset maintained in Google Sheets by the Water Policy team. It is used in the funding data notebooks as well as the `combined-data-yoy.Rmd` data visualizations. This data is ingested in functions defined in the `resources.R` script. The visualizations from these notebooks are pushed to AWS and then referenced in a Google Sheets directory for easy access.
