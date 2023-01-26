# Build Docker image from rocker/tidyverse dockerfile
FROM rocker/tidyverse:latest

# Maintainer info
LABEL maintainer "Eric Largy <eric.largy@u-bordeaux.fr>"

# System libraries
RUN apt-get update && apt-get install -y \
sudo \
pandoc \
pandoc-citeproc \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
libssl-dev \
libssh2-1-dev \
#libssl1.1 \
libssl3 \
libnetcdf-dev \
libcairo2-dev 

RUN apt-get update && apt-get install -y \
libmpfr-dev

# Configure dynamic linker for share objects
# Necessary for mzR install
RUN sudo ldconfig

# Install R packages
RUN install2.r --error \
    librarian \
    ncdf4 
RUN R -e "librarian::shelf(tidyverse, devtools, BiocManager, readr, readxl, data.table, DT, magrittr, stringr, formattable, gnm, DescTools, ggsci, ggpubr, ggrepel, ggthemes, ggpmisc, thematic, zoo, BiocManager, V8, matrixStats)"
RUN R -e "librarian::shelf(shiny, shinydashboard, shinydashboardPlus, shinyBS, shinyWidgets, bslib, DT, Cairo, colourpicker, DavidBarke/QWUtils)"
RUN R -e "librarian::shelf(peakPick)"
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"
RUN R -e "devtools::install_github('cran/peakPick')"
RUN Rscript -e "BiocManager::install('mzR')"

# Copy all app files (R and Rdata files) onto the image in folder /root/oligor
RUN mkdir /root/oligor/
COPY app /root/oligor

# Copy Rprofile.site set up file to the image.
# Ensures app runs on expected port
COPY Rprofile.site /usr/lib/R/etc/

# Instruct Docker to expose port 3838
EXPOSE 3838

# Launch Shiny app when is started
CMD ["R", "-e", "shiny::runApp('/root/oligor', port = 3838L, host = '0.0.0.0')"]
