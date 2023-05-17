![](www/epic-logo-transparent.png){width="806"}

# EPIC's Drinking Water Funding Dashboard

This repository maintains the code for running [EPIC's Drinking Water Funding Dashboard](https://www.policyinnovation.org/water/tracking-iija), as well as the necessary data cleaning and processing steps. The dashboard serves as a tool for exploring drinking water projects states intend to fund in the next year through state revolving funds based on the type of project, the funding amount, which communities are impacted, and other features.

For important context and caveats regarding how to interpret and use this data, a glossary of terms used throughout the project, and data dictionaries for each included state, [visit the Drinking Water Funding Dashboard page](https://www.policyinnovation.org/water/tracking-iija).

The project's pipeline includes:

-   Parsing state documents, typically stored as PDFs, into CSVs. This process must be done individually for each document. A sample of the parsing code is available in the `pdf-parsing-code` folder along with scripts used to correct parsed tables when needed. Each of the resulting CSVs are available in the `data/srf-ppl` folder.

-   Standardizing state project tables into a single dataset. The `srf-ppl-standardization.Rmd` notebook takes in CSVs of each state's parsed PDF tables. It transforms each state's particular projects into the variables presented in the dashboard.

-   Transforming the standardized data for dashboard presentation. The `dw-dashboard-data-cleaner.R` script prepares the standardized data for use in the dashboard. It also integrates external data needed for additional dashboard features.

-   Developing the dashboard. The `dw-dashboard-app.R` script contains code for creating and implementing the Shiny application for the dashboard itself.
