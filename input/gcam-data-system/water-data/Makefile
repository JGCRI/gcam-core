# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export WATERPROC = ../water-processing-code
export WATERDAT	= .
export WATERXML = ../xml/water-xml
export COMMON = ../_common

L1SCRIPTS = $(wildcard $(WATERPROC)/level1/*.R)
L1LOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1SCRIPTS)))

L2SCRIPTS = $(wildcard $(WATERPROC)/level2/*.R)
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(WATERPROC)/logs/*.d)

XML_BATCH_FILES = $(wildcard $(WATERPROC)/xml-batch/*.xml)
XML_FILES = $(subst batch_,,$(subst $(WATERPROC)/xml-batch,$(WATERXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

all: level1 level2 xml

xml: level2
	$(MAKE) XML_BATCH

level1: $(L1LOGS)

level2: level1 $(L2LOGS)

XML_BATCH: $(XML_FILES)

$(WATERPROC)/logs/L1%.log: $(WATERPROC)/level1/L1%
	@echo $?
	$(RCMD) $< $(WATERPROC)/logs/L1.Rout

$(WATERPROC)/logs/L2%.log: $(WATERPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(WATERPROC)/logs/L2.Rout

$(WATERXML)/%.xml: $(WATERPROC)/xml-batch/batch_%.xml
	@echo $?
	java -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(WATERDAT)/level1/*.csv
	-rm -f $(WATERDAT)/level2/*.csv
	-rm -f $(WATERPROC)/logs/*.log
	-rm -f $(WATERPROC)/logs/*.d
	-rm -f $(WATERPROC)/xml-batch/*.xml

level2clean:
	-rm -f $(WATERPROC)/logs/L2*.log
	-rm -f $(WATERPROC)/logs/L2*.d
	-rm -f $(WATERPROC)/xml-batch/*.xml
	-rm -f $(WATERXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
