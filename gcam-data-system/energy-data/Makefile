# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export ENERGYPROC = ../energy-processing-code
export ENERGYDAT	= .
export ENERGYXML = ../xml/energy-xml
export COMMON = ../_common

L1SCRIPTS = $(wildcard $(ENERGYPROC)/level1/*.R)
L1LOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1SCRIPTS)))

L2SCRIPTS = $(wildcard $(ENERGYPROC)/level2/*.R)
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(ENERGYPROC)/logs/*.d)

XML_BATCH_FILES = $(wildcard $(ENERGYPROC)/xml-batch/*.xml)
XML_FILES = $(subst batch_,,$(subst $(ENERGYPROC)/xml-batch,$(ENERGYXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

all: level1 level2 XML_BATCH

level1: $(L1LOGS)

level2: level1 $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(ENERGYPROC)/logs/L1%.log: $(ENERGYPROC)/level1/L1%
	@echo $?
	$(RCMD) $< $(ENERGYPROC)/logs/L1.Rout

$(ENERGYPROC)/logs/L2%.log: $(ENERGYPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(ENERGYPROC)/logs/L2.Rout

$(ENERGYXML)/%.xml: $(ENERGYPROC)/xml-batch/batch_%.xml
	@echo $?
	java -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(ENERGYPROC)/logs/*.log
	-rm -f $(ENERGYPROC)/logs/*.d
	-rm -f $(ENERGYPROC)/xml-batch/*.xml
	-rm -f $(ENERGYXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
