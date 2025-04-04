![](www/epic-logo-transparent.png)

# EPIC'S DWSRF Funding Tracker


This repository maintains the code and data powering the DWSRF Funding Tracker. The website serves as an in-depth resource that monitors proposed funding and policies in the annual Intended Use Plans (IUPs) of 15 focus states.

The datasets developed in this repo ("project data") explore which drinking water infrastructure projects apply for funds and which projects states intend to fund through state revolving funds in a given year. The information available about projects varies from state-to-state and year-to-year, but generally includes descriptions of the project, its costs or how much funding might be expected and whether it includes principal forgiveness, and details about the borrower and community served.

The other primary data source ("funding data") comes from a comprehensive dataset maintained by EPIC's Water Policy team. Also extracted from state IUPs, this data focuses on the sources of funding through various federal capitalization grants, the use of set aside funds, and the distribution of principal forgiveness.

For important context and caveats regarding how to interpret and use this data, a glossary of terms used throughout the project, and data dictionaries for each included state, view the website's resources page (link forthcoming).

The project data pipeline includes:

-   **Scraping**. We first extract project data from PDF project priority lists (PPLs) often contained within the IUPs. This step typically involves using Tabula (a Java-based GUI that can be run locally) or generative AI to access the data. For many Y1 states, python notebooks detail the scraping process. The resulting CSVs are available in the state's `data` folder.

-   **Cleaning & Standardizing.** For each year of data for each state, we write a function that imports the scraped CSVs, details how each table is cleaned, and returns a standardized dataframe. These functions follow guidance provided in a data dictionary written by EPIC's Water Policy team. The `combine-states.Rmd` notebook sources the cleaning scripts for each state, creating the final source of all project data for each state over each year.

-   **Aggregating.** The `state-dataset.Rmd` notebook groups project-level data by state for each state fiscal year. The resulting datasets include:

    -   Feature completeness, denoting whether a particular column is Missing, Partial, or Complete for a given state and state fiscal year

    -   Counts of projects by categorical features like disadvantaged status and expecting funding

    -   Aggregated statistics of financial features like funding amount and project costs

    -   Counts and aggregated statistics further subset by project types (General, Lead, Emerging Contaminants)

    -   Counts and aggregated statistics subset by whether they serve small communities (populations of less than 10,000)

-   **Analyzing & Visualizing.** Finally, using both the project and funding data, we create the static and interactive data visualization to power the SRF Funding Tracker website. This graphics are maintained in a series of notebooks denoted by the data source and the time frame considered. For instance, `project-data-yoy.Rmd` considers only data from the above pipeline analyzed for each year of available data. The visualizations are then pushed to AWS and then referenced in a Google Sheets directory for easy access.
