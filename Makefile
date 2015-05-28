R=R
RSCRIPT=Rscript


check:
	[ -e .check ] || mkdir .check
	$(R) CMD build TransmartUploader
	$(R) CMD check TransmartUploader*.gz

	
tests: 
	 $(RSCRIPT) -e 'library(methods);devtools::test("TransmartUploader")'
	 
	 
doc:
	rm -f Rd2.pdf TransmartUploader.pdf
	R CMD Rd2pdf -o TransmartUploader.pdf TransmartUploader