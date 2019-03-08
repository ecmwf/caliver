FROM s22s/geo-swak

RUN set -eux \
	&& apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
	&& add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu cosmic-cran35/' \
	&& apt-get update --yes

RUN set -eux \
	&& env DEBIAN_FRONTEND=noninteractive \
		apt install --yes \
		libssl-dev \
		r-base-dev

RUN set -eux \
	&& R -e 'install.packages("devtools")' \
	&& R -e 'devtools::install_github("ecmwf/caliver")'
