
studies = wsw2-main wsw3-p2 wsw3-main

results = $(patsubst %, studies/%/results.html, $(studies))
data_files = $(patsubst %, studies/%/data/data.rds, $(studies))

models = $(patsubst %, studies/%/model-objects/trait-mpt.rds, $(studies))

all:	$(results) README.md

$(results): %results.html: %results.rmd %data/data.rds %model-objects/trait-mpt.rds
	@echo "Knitting $@ from $^"
	R -q -e 'rmarkdown::render("$<", knit_root_dir = rprojroot::find_rstudio_root_file())'

$(models): %model-objects/trait-mpt.rds: %R/trait-mpt.R %data/data.rds
	@echo "Processing $@ from $^"
	R -f '$<'

# data files (reverse chronological order) ----
# we define these for each study, separately, to ensure that wildcard expansion works
studies/wsw3-main/data/data.rds: studies/wsw3-main/R/prepare-data.R studies/wsw3-main/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'

studies/wsw3-p2/data/data.rds: studies/wsw3-p2/R/prepare-data.R studies/wsw3-p2/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'
	
studies/wsw2-main/data/data.rds: studies/wsw2-main/R/prepare-data.R studies/wsw2-main/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'


# README ----
README.md: README.rmd
	R -e 'rmarkdown::render("README.rmd")'
