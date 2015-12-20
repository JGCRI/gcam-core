# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export MODELTIMEPROC = ../modeltime-processing-code
export MODELTIMEDAT	= .
export MODELTIMEXML = ../xml/modeltime-xml
export CLIMATEXML = ../../climate
export COMMON = ../_common

L2SCRIPTS = $(wildcard $(MODELTIMEPROC)/level2/*.R)
L2LOGS	  = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(MODELTIMEPROC)/logs/*.d)

XML_BATCH_FILES = $(wildcard $(MODELTIMEPROC)/xml-batch/*.xml)
#XML_FILES = $(subst batch_,,$(subst $(MODELTIMEPROC)/xml-batch,$(MODELTIMEXML),$(XML_BATCH_FILES)))
XML_FILES = $(MODELTIMEXML)/modeltime.xml ../../climate/magicc.xml ../../climate/hector.xml

include $(LOGDEPS)

all: level2 XML_BATCH

level2: $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(MODELTIMEPROC)/logs/L2%.log: $(MODELTIMEPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(MODELTIMEPROC)/logs/L2.Rout

$(MODELTIMEXML)/%.xml: $(MODELTIMEPROC)/xml-batch/batch_%.xml
	@echo $?
	java -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

$(CLIMATEXML)/%.xml: $(MODELTIMEPROC)/xml-batch/batch_%.xml
	@echo $?
	java -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(MODELTIMEDAT)/level1/*.csv
	-rm -f $(MODELTIMEDAT)/level2/*.csv
	-rm -f $(MODELTIMEPROC)/logs/*.log
	-rm -f $(MODELTIMEPROC)/logs/*.d
	-rm -f $(MODELTIMEPROC)/xml-batch/*.xml

level2clean:
	-rm -f $(MODELTIMEPROC)/logs/L2*.log
	-rm -f $(MODELTIMEPROC)/logs/L2*.d
	-rm -f $(MODELTIMEPROC)/xml-batch/*.xml
	-rm -f $(MODELTIMEXML)/*.xml

test:
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
