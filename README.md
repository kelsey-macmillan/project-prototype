# Overview

**Name**: Kelsey MacMillan
**Email**: kelsey.macmillan@gmail.com

This is a data visualization project in Shiny to explore Seattle housing market data from a neighborhood-centric point of view. There are three "perspectives" on the data -- geospatial, correlations, and historical. All visualizations include interactions that allow selected lines / map objects to be highlighted and thus more readily distinguished from the full population of data observations.

# Data

Neighborhood historical data is from Redfin: https://www.redfin.com/blog/data-center

Neighborhood map data (shapefile) is from the City of Seattle: https://data.seattle.gov/dataset/Neighborhoods/2mbt-aqqx

# Perspectives

### Geospatial
* Map coloring aesthetic can be changed.
* Clicking on a neighborhood provides details in a side panel.

![Alt text](/screenshots/geo.png?raw=true "Optional Title")

### Historical
* Hovering over a line highlights it.
* Clicking on a neighborhood in the time series plot or the map plot highlights it in both (interactive filtering).
* Can adjust the time range of the time series plot.
* Can select historical data plotted, and highlighting is maintained.

![Alt text](/screenshots/historical.png?raw=true "Optional Title")

### Correlations
* Parallel coordinates plot shows relationships between variables.
* Clicking on a neighborhood in the time series plot or the map plot highlights it in both (interactive filtering).
* Can select month of data plotted, and highlighting is maintained.

![Alt text](/screenshots/corr.png?raw=true "Optional Title")

# TODO
* Combine three Shiny apps into a single app with tabs to switch between the views.


