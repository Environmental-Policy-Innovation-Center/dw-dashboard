FROM rocker/tidyverse:latest

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


RUN install2.r --error RSocrata tidyverse tidygeocoder mapboxapi lubridate httr jsonlite aws.s3 aws.ec2metadata plyr

#------------
RUN ls -al '/home/epic'
ADD Worker.R /home/epic/

CMD Rscript /home/epic/Worker.R

#------------