FROM s22s/geo-swak

RUN set -eux \
	&& apt-get update \
	&& env DEBIAN_FRONTEND=noninteractive apt-get install --yes gnupg

RUN set -eux \
	&& apt-key adv --keyserver hkp://keys.gnupg.net:80 --recv-keys E19F5F87128899B192B1A2C2AD5F960A256A04AF \
	&& echo 'deb http://cloud.r-project.org/bin/linux/debian buster-cran35/' > /etc/apt/sources.list.d/r-project.list \
	&& apt-get update --yes

RUN set -eux \
	&& env DEBIAN_FRONTEND=noninteractive \
		apt install --yes \
		libnetcdf-dev \
		libssl-dev \
		libxml2-dev \
		r-base-dev

RUN set -eux \
	&& R -e 'install.packages("devtools")' \
	&& R -e 'devtools::install_github("ecmwf/caliver")'
