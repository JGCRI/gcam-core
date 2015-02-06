# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export GCAMUSAPROC = ../gcam-usa-processing-code
export GCAMUSADAT	= .
export GCAMUSAXML = ../xml/gcam-usa-xml
export COMMON = ../_common

L1ASCRIPTS = $(sort $(wildcard $(GCAMUSAPROC)/level1/LA*.R))
L1ALOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1ASCRIPTS)))

L1BSCRIPTS = $(sort $(wildcard $(GCAMUSAPROC)/level1/LB*.R))
L1BLOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1BSCRIPTS)))


L2SCRIPTS = $(sort $(wildcard $(GCAMUSAPROC)/level2/*.R))
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(GCAMUSAPROC)/logs/*.d)

XML_BATCH_FILES = $(sort $(wildcard $(GCAMUSAPROC)/xml-batch/*.xml))
XML_FILES = $(subst batch_,,$(subst $(GCAMUSAPROC)/xml-batch,$(GCAMUSAXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

#all: level1A level1B level2 XML_BATCH

level1A: $(L1ALOGS)

level1B: $(L1BLOGS)

level2: level1B $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

xml: XML_BATCH

$(GCAMUSAPROC)/logs/LA1%.log: $(GCAMUSAPROC)/level1/LA1%
	@echo $?
	$(RCMD) $< $(GCAMUSAPROC)/logs/LA1.Rout

$(GCAMUSAPROC)/logs/LB1%.log: $(GCAMUSAPROC)/level1/LB1%
	@echo $?
	$(RCMD) $< $(GCAMUSAPROC)/logs/LB1.Rout 

$(GCAMUSAPROC)/logs/L2%.log: $(GCAMUSAPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(GCAMUSAPROC)/logs/L2.Rout

$(GCAMUSAXML)/%.xml: $(GCAMUSAPROC)/xml-batch/batch_%.xml
	@echo $?
	java -Xmx2g -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(GCAMUSADAT)/level1/*.csv
	-rm -f $(GCAMUSADAT)/level2/*.csv
	-rm -f $(GCAMUSAPROC)/logs/*.log
	-rm -f $(GCAMUSAPROC)/logs/*.d
	-rm -f $(GCAMUSAPROC)/xml-batch/*.xml

level2clean:
	-rm -f $(GCAMUSAPROC)/logs/L2*.log
	-rm -f $(GCAMUSAPROC)/logs/L2*.d
	-rm -f $(GCAMUSAPROC)/xml-batch/*.xml
	-rm -f $(GCAMUSAXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
