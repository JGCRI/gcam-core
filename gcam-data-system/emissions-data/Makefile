# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export EMISSIONSPROC = ../emissions-processing-code
export EMISSIONSDAT	= .
export EMISSIONSXML = ../xml/emissions-xml
export COMMON = ../_common

L1SCRIPTS = $(sort $(wildcard $(EMISSIONSPROC)/level1/*.R))
L1LOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1SCRIPTS)))

L2SCRIPTS = $(sort $(wildcard $(EMISSIONSPROC)/level2/*.R))
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(EMISSIONSPROC)/logs/*.d)

XML_BATCH_FILES = $(sort $(wildcard $(EMISSIONSPROC)/xml-batch/*.xml))
XML_FILES = $(subst batch_,,$(subst $(EMISSIONSPROC)/xml-batch,$(EMISSIONSXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

#all: level1 level2 XML_BATCH

level1: $(L1LOGS)

level2: level1 $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(EMISSIONSPROC)/logs/L1%.log: $(EMISSIONSPROC)/level1/L1%
	echo $?
	$(RCMD) $< $(EMISSIONSPROC)/logs/L1.Rout

$(EMISSIONSPROC)/logs/L2%.log: $(EMISSIONSPROC)/level2/L2%
	echo $?
	$(RCMD) $< $(EMISSIONSPROC)/logs/L2.Rout

$(EMISSIONSXML)/%.xml: $(EMISSIONSPROC)/xml-batch/batch_%.xml
	echo $?
	java -Xmx2g -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(EMISSIONSPROC)/logs/*.log
	-rm -f $(EMISSIONSPROC)/logs/*.d
	-rm -f $(EMISSIONSPROC)/xml-batch/*.xml
	-rm -f $(EMISSIONSXML)/*.xml

level2clean:
	-rm -f $(EMISSIONSPROC)/logs/L2*.log
	-rm -f $(EMISSIONSPROC)/logs/L2*.d
	-rm -f $(EMISSIONSPROC)/xml-batch/*.xml
	-rm -f $(EMISSIONSXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
