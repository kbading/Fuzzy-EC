
studies = wsw1 wsw2-main wsw3-p2 wsw3-main wsw3-joint-analysis

results = $(patsubst %, studies/%/results.html, $(studies))
data_files = $(patsubst %, studies/%/data/data.rds, $(studies))

models = $(patsubst %, studies/%/model-objects/trait-mpt.rds, $(studies))
extended_models = $(patsubst %, studies/%/model-objects/trait-mpt-with-ec.rds, $(studies))
wsw8b_models = $(patsubst %, studies/%/model-objects/trait-mpt-wsw-8b.rds, $(studies))
wsw8b_with_ec_models = $(patsubst %, studies/%/model-objects/trait-mpt-wsw-8b-with-ec.rds, $(studies))
treestan_models = $(patsubst %, studies/%/model-objects/treestan.rds, $(studies))

all:	$(results) README.md studies/wsw2-main/model-objects/waic-wsw-8b.rds

studies/wsw2-main/model-objects/waic-wsw-8b.rds: studies/wsw2-main/R/waic-wsw-8b.R studies/wsw2-main/model-objects/trait-mpt-wsw-8b-whole-family.rds
	R -q -f studies/wsw2-main/R/waic-wsw-8b.R
	
studies/wsw2-main/model-objects/trait-mpt-wsw-8b-whole-family.rds: studies/wsw2-main/R/waic-wsw-8b.R studies/wsw2-main/data/data.rds
	R -q -f studies/wsw2-main/R/waic-wsw-8b.R

$(results): %results.html: %results.rmd %data/data.rds \
  %model-objects/trait-mpt.rds \
  %model-objects/trait-mpt-with-ec.rds \
  %model-objects/trait-mpt-wsw-8b.rds \
  %model-objects/trait-mpt-wsw-8b-with-ec.rds \
  %model-objects/treestan.rds
	@echo "Knitting $@ from $^"
	R -q -e 'rmarkdown::render("$<", knit_root_dir = rprojroot::find_rstudio_root_file())'

$(models): %model-objects/trait-mpt.rds: %R/trait-mpt.R %data/data.rds
	@echo "Estimating latent-trait MPT model $@ from $^"
	R -f '$<'
	
$(extended_models): %model-objects/trait-mpt-with-ec.rds: %R/trait-mpt-with-ec.R %data/data.rds
	@echo "Estimating latent-trait MPT model $@ from $^"
	R -f '$<'
	
$(wsw8b_models): %model-objects/trait-mpt-wsw-8b.rds: %R/trait-mpt-wsw-8b.R %data/data.rds
	@echo "Estimating latent-trait MPT model $@ from $^"
	R -f '$<'
	
$(wsw8b_with_ec_models): %model-objects/trait-mpt-wsw-8b-with-ec.rds: %R/trait-mpt-wsw-8b-with-ec.R %data/data.rds
	@echo "Estimating latent-trait MPT model $@ from $^"
	R -f '$<'

$(treestan_models): %model-objects/treestan.rds: %R/treestan.R %data/data.rds
	@echo "Estimating TreeStan model $@ from $^"
	R -f '$<'

# data files (reverse chronological order) ----
# we define these for each study, separately, to ensure that wildcard expansion works
studies/wsw3-joint-analysis/data/data.rds: studies/wsw3-joint-analysis/R/prepare-data.R \
		studies/wsw3-main/data/data.rds studies/wsw3-p2/data/data.rds
	@echo "Processing $@ from $^"
	R -f '$<'

studies/wsw3-main/data/data.rds: studies/wsw3-main/R/prepare-data.R studies/wsw3-main/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'

studies/wsw3-p2/data/data.rds: studies/wsw3-p2/R/prepare-data.R studies/wsw3-p2/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'
	
studies/wsw2-main/data/data.rds: studies/wsw2-main/R/prepare-data.R studies/wsw2-main/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'
	
studies/wsw1/data/data.rds: studies/wsw1/R/prepare-data.R studies/wsw1/data-raw/* 
	@echo "Processing $@ from $^"
	R -f '$<'

# README ----
README.md: README.rmd
	R -e 'rmarkdown::render("README.rmd")'
