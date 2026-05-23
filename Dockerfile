FROM rocker/r-ver:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    make \
    gcc \
    g++ \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy package files
COPY DESCRIPTION /app/DESCRIPTION
COPY NAMESPACE /app/NAMESPACE
COPY R /app/R
COPY plumber.R /app/plumber.R

# Install R dependencies and local package
RUN R -e "install.packages(c('survival','dplyr','tidyr','tibble','ggplot2','scales','plumber'), repos='https://cloud.r-project.org/', dependencies=TRUE)"

RUN R CMD INSTALL /app

EXPOSE 10000

CMD ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '10000')))"]
