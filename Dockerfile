FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libudunits2-dev \
libprotobuf-dev \
protobuf-compiler \ 
libproj-dev \
libgdal-dev \
libmagick++-dev \ 
    && rm -rf /var/lib/apt/lists/*
    
RUN mkdir -p /home/epic
RUN cd /home/epic


RUN install2.r --error shiny \
leaflet \
googlesheets4 \
rgdal \
dplyr \
readr \
geojsonsf \
jsonlite \
sf \
scales \
htmltools \
shinyBS \
aws.s3 \
shinyjs \
DT \
shinyalert \
janitor \
formattable \
reactable \
shinybrowser \
reactablefmtr \
stringr \
shinyWidgets \
waiter \
tippy \
shinycssloaders \
zip \
googledrive 

RUN install2.r --error aws.ec2metadata 

#------------
RUN ls -al '/home/epic'
ADD dw-dashboard-app.R /home/epic/

ADD ./www /home/epic/www

EXPOSE 2000

WORKDIR /home/epic

CMD ["R", "-e", "shiny::runApp('/home/epic/dw-dashboard-app.R', port = 2000, host = '0.0.0.0')"]

#------------