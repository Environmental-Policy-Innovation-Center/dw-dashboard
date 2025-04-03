![](www/epic-logo-transparent.png)

# EPIC'S DWSRF Funding Tracker

This repository maintains the code and data that powers the [DWSRF Funding Tracker](). The website serves as a tool for exploring drinking water projects that states intend to fund in the next year through state revolving funds based on the type of project, the funding amount, which communities are impacted, and other features.

For important context and caveats regarding how to interpret and use this data, a glossary of terms used throughout the project, and data dictionaries for each included state, view [the website's resources page]().

The project's pipeline includes:

-   Scraping data from tables in state documents, typically stored as PDFs, into CSVs. This process must be done individually for each document. This step is typically done using Tabula (a Java-based GUI that can be run locally) or generative AI. For many Y1 states, python notebook details the scraping process. Each of the resulting CSVs are available in the state's `data` folder.

-   Cleaning the raw scraped data. Within each state folder, a R script takes in the resulting scraped CSVs and details how each table is cleaned and made ready to be standardized following a data dictionary provided by EPIC's Water Policy team.

-   Combining state project tables into a single dataset. The `combine-states.Rmd` notebook sources the cleaning script for each state. It then combines and finalizes the cleaned data to be presented in the dashboard and integrates the external data needed for additional dashboard features.

-   Developing the dashboard. The `dw-dashboard-app.R` script contains code for creating and implementing the Shiny application for the dashboard itself.
