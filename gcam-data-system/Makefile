xml: aglu2 energy2 socio2 modeltime
	$(MAKE) -C aglu-data XML_BATCH
	$(MAKE) -C energy-data XML_BATCH
	$(MAKE) -C socioeconomics-data XML_BATCH
	$(MAKE) -C modeltime-data XML_BATCH 

common:
	$(MAKE) -C _common all

energy1a: common
	$(MAKE) -C energy-data level1A

energy1b: energy1a aglu1a
	$(MAKE) -C energy-data level1B

energy2: energy1b
	$(MAKE) -C energy-data level2

aglu1a: common
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

## add water-data build commands here eventually

## What about the emissions-data directory?

