R=R
RSCRIPT=Rscript
PKG=TransmartUploader
DB=

check:
	[ -e .check ] || mkdir .check
	$(R) CMD build TransmartUploader
	$(R) CMD check TransmartUploader*.gz

	
tests: 
	 TRANSMART_DB=$(DB) $(RSCRIPT) -e 'library(methods);devtools::test("$(PKG)")'
	 
set-local-db:
	$(eval DB := localhost@5432)
	 
tests-on-local-db: set-local-db tests
	

roxygen:
	 $(RSCRIPT) -e 'library(methods);devtools::document("$(PKG)")'

check-man: roxygen
	$(RSCRIPT) -e 'devtools::document("$(PKG)");devtools::check_man("$(PKG)")'
	 
doc: roxygen
	rm -f Rd2.pdf TransmartUploader.pdf
	R CMD Rd2pdf -o TransmartUploader.pdf TransmartUploader

dev.pdf: roxygen
	rm -f $@
	R CMD Rd2pdf -o $@ TransmartUploader
	
coverage-html:
	TRANSMART_DB=$(DB) Rscript -e 'library(covr); cov<-package_coverage("$(PKG)"); covr::report(cov, browse=T); cat("Press [0+enter] to after viewing the report:"); b <- scan("stdin", character(), n=1)'
