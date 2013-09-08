# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export AGLUPROC = ../aglu-processing-code
export AGLUDAT	= .
export AGLUXML = ../xml/aglu-xml
export COMMON = ../_common

L1SCRIPTS = $(wildcard $(AGLUPROC)/Level1/*.R)
L1LOGS	  = $(patsubst %.R,%.R.log,$(subst /Level1/,/logs/,$(L1SCRIPTS)))

L2SCRIPTS = $(wildcard $(AGLUPROC)/Level2/*.R)
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /Level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(AGLUPROC)/logs/*.d)

XML_BATCH_FILES = $(wildcard $(AGLUPROC)/xml-batch/*.xml)
XML_FILES = $(subst batch_,,$(subst $(AGLUPROC)/xml-batch,$(AGLUXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

all: Level1 Level2 XML_BATCH

Level1: $(L1LOGS)

Level2: Level1 $(L2LOGS)

XML_BATCH: Level2 $(XML_FILES)

$(AGLUPROC)/logs/L1%.log: $(AGLUPROC)/Level1/L1%
	echo $?
	$(RCMD) $< $(AGLUPROC)/logs/L1.Rout

$(AGLUPROC)/logs/L2%.log: $(AGLUPROC)/Level2/L2%
	echo $?
	$(RCMD) $< $(AGLUPROC)/logs/L2.Rout

$(AGLUXML)/%.xml: $(AGLUPROC)/xml-batch/batch_%.xml
	echo $?
	java -Xmx2g -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(AGLUPROC)/logs/*.d
	-rm -f $(AGLUPROC)/logs/*.log
	-rm -f $(AGLUPROC)/xml-batch/*.xml
	-rm -f $(AGLUXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
