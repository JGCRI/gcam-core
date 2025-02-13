%:
	cd ./cvs/objects/build/linux && $(MAKE) $@

xml:
	cd input/gcamdata && Rscript -e "devtools::load_all('.')" -e "driver(write_output=FALSE, write_xml=TRUE)"

xml_drake:
    cd input/gcamdata && Rscript -e "devtools::load_all('.')" -e "driver_drake()"
    
install_hector:
	git submodule update --init cvs/objects/climate/source/hector

