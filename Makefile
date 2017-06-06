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

check-doc: roxygen
	$(RSCRIPT) -e 'devtools::document("$(PKG)");devtools::check_doc("$(PKG)")'
	 
doc: roxygen
	rm -f Rd2.pdf TransmartUploader.pdf
	R CMD Rd2pdf -o TransmartUploader.pdf TransmartUploader
