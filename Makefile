
studies = wsw2-main wsw3-p2 wsw3-main

results = $(patsubst %, studies/%/results.html, $(studies))
rating_files = $(patsubst %, studies/%/data/rating.rds, $(studies))

all:	$(results) README.md

$(results): %results.html: %results.rmd %data/mpt_data.rds %data/mpt_data.rds %data/rating.rds %data/mpt_data.rds %data/mpt_data_hierarchical.rds
	R -q -e 'rmarkdown::render("$<", knit_root_dir = rprojroot::find_rstudio_root_file())'

$(rating_files): %data/rating.rds: %R/prepare-data.R
	@echo "Processing $@ from $^"
	R -f '$<'

README.md: README.rmd
	R -e 'rmarkdown::render("README.rmd")'
