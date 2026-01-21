FROM rocker/r-ubuntu:22.04

LABEL maintainer="Advance Services <advancesvs.com>"


RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    curl \
    git \
    pandoc \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    r-cran-tidyverse \
    r-cran-readxl \
    r-cran-flextable \
    r-cran-officer \
    r-cran-ggplot2 \
    r-cran-ggpmisc \
    r-cran-ggfortify \
    r-cran-openxlsx \
    r-cran-broom \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('optparse','webshot2','multcompView'), repos='https://cran.r-project.org')"

COPY . /usr/local/src/
WORKDIR /usr/local/src/

ENTRYPOINT ["Rscript", "agrecolvre_PCA.R"]
