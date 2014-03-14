xml: emiss2 aglu2 energy2 socio2 modeltime
	$(MAKE) -C emissions-data XML_BATCH
	$(MAKE) -C aglu-data XML_BATCH
	$(MAKE) -C energy-data XML_BATCH
	$(MAKE) -C socioeconomics-data XML_BATCH
	$(MAKE) -C modeltime-data XML_BATCH 

common:
	$(MAKE) -C _common all

emiss2: emiss1
	$(MAKE) -C emissions-data level2

emiss1: common modeltime energy1b aglu1b
	$(MAKE) -C emissions-data level1

energy1a: common modeltime socio2
	$(MAKE) -C energy-data level1A

energy1b: energy1a aglu1a
	$(MAKE) -C energy-data level1B

energy2: energy1b
	$(MAKE) -C energy-data level2

aglu1a: common modeltime socio2
	$(MAKE) -C aglu-data level1A

aglu1b: aglu1a energy1a
	$(MAKE) -C aglu-data level1B

aglu2: aglu1b
	$(MAKE) -C aglu-data level2

socio1: common
	$(MAKE) -C socioeconomics-data level1

socio2: socio1
	$(MAKE) -C socioeconomics-data level2

modeltime: common
	$(MAKE) -C modeltime-data level2

clean:
	$(MAKE) -C emissions-data clean
	$(MAKE) -C energy-data clean
	$(MAKE) -C aglu-data clean
	$(MAKE) -C modeltime-data clean
	$(MAKE) -C socioeconomics-data clean


## add water-data build commands here eventually

## What about the emissions-data directory?

