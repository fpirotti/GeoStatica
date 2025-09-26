# Use Rocker base with R + Shiny Server
FROM rocker/shiny:4.4.1

# System dependencies for geospatial packages
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('shiny', 'leaflet', 'sf', 'terra', 'sp', 'stars', 'gstat', 'dplyr'), repos='https://cloud.r-project.org/')"

# Copy your Shiny app into the container
COPY ui.R /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY global.R /srv/shiny-server/
COPY users.pwds.rda /srv/shiny-server/
COPY functions_auth.R /srv/shiny-server/
COPY data /srv/shiny-server/data/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny server port
EXPOSE 3838

# Run Shiny server
CMD ["/usr/bin/shiny-server"]
