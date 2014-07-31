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

L1ASCRIPTS = $(sort $(wildcard $(ENERGYPROC)/level1/LA*.R))
L1ALOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1ASCRIPTS)))

L1BSCRIPTS = $(sort $(wildcard $(ENERGYPROC)/level1/LB*.R))
L1BLOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1BSCRIPTS)))


L2SCRIPTS = $(sort $(wildcard $(ENERGYPROC)/level2/*.R))
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(ENERGYPROC)/logs/*.d)

XML_BATCH_FILES = $(sort $(wildcard $(ENERGYPROC)/xml-batch/*.xml))
XML_FILES = $(subst batch_,,$(subst $(ENERGYPROC)/xml-batch,$(ENERGYXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

#all: level1A level1B level2 XML_BATCH

level1A: $(L1ALOGS)

## Level 1B also depends on the first phase of the AGLU processing.
## This dependency is captured in the upper-level makefile.
level1B: $(L1BLOGS)

level2: level1B $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(ENERGYPROC)/logs/LA1%.log: $(ENERGYPROC)/level1/LA1%
	@echo $?
	$(RCMD) $< $(ENERGYPROC)/logs/LA1.Rout

$(ENERGYPROC)/logs/LB1%.log: $(ENERGYPROC)/level1/LB1%
	@echo $?
	$(RCMD) $< $(ENERGYPROC)/logs/LB1.Rout 

$(ENERGYPROC)/logs/L2%.log: $(ENERGYPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(ENERGYPROC)/logs/L2.Rout

$(ENERGYXML)/%.xml: $(ENERGYPROC)/xml-batch/batch_%.xml
	@echo $?
	java -Xmx2g -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(ENERGYDAT)/level1/*.csv
	-rm -f $(ENERGYDAT)/level2/*.csv
	-rm -f $(ENERGYPROC)/logs/*.log
	-rm -f $(ENERGYPROC)/logs/*.d
	-rm -f $(ENERGYPROC)/xml-batch/*.xml

level2clean:
	-rm -f $(ENERGYPROC)/logs/L2*.log
	-rm -f $(ENERGYPROC)/logs/L2*.d
	-rm -f $(ENERGYPROC)/xml-batch/*.xml
	-rm -f $(ENERGYXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
