# Use rocker/shiny with R 4.5
FROM rocker/shiny:4.5

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    zlib1g-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Use Posit Package Manager for pre-compiled binaries (much faster!)
ENV CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/noble/latest

# Accept GitHub PAT as build argument to avoid rate limits
ARG GITHUB_PAT
ENV GITHUB_PAT=$GITHUB_PAT

# Install pak for faster and more reliable package installation
RUN R -e "install.packages('pak', repos=Sys.getenv('CRAN_REPO'))"

# Install CRAN dependencies first (using pre-compiled binaries - much faster!)
RUN R -e "install.packages(c('shiny', 'dm', 'dplyr', 'ggplot2', 'DiagrammeR', 'glue', 'htmltools'), repos=Sys.getenv('CRAN_REPO'))"

# Install GitHub packages (blockr ecosystem) using pak - in stages to avoid conflicts
RUN R -e "pak::pak('cynkra/dockViewR')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr.core')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr.dock')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr.dag')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr.dplyr')"
RUN R -e "pak::pak('BristolMyersSquibb/blockr.ggplot')"

# Install blockr.dm package from local source
COPY . /tmp/blockr.dm
RUN R -e "pak::pak('local::/tmp/blockr.dm')"

# Copy the Shiny app to the default Shiny Server location
RUN rm -rf /srv/shiny-server/*
COPY deploy /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server

# Make sure the app is readable
RUN chmod -R 755 /srv/shiny-server

# Expose port 3838 for Shiny Server
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
