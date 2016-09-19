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
export WATERPROC = ../water-processing-code

L1ASCRIPTS = $(sort $(wildcard $(AGLUPROC)/level1/LA*.R))
L1ALOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1ASCRIPTS)))

L1BSCRIPTS = $(sort $(wildcard $(AGLUPROC)/level1/LB*.R))
L1BLOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1BSCRIPTS))) 

L2SCRIPTS = $(sort $(wildcard $(AGLUPROC)/level2/*.R))
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(AGLUPROC)/logs/*.d)

XML_BATCH_FILES = $(sort $(wildcard $(AGLUPROC)/xml-batch/*.xml))
XML_FILES = $(subst batch_,,$(subst $(AGLUPROC)/xml-batch,$(AGLUXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

#all: level1 level2 XML_BATCH

level1A: $(L1ALOGS)

## level 1B also depends on the first phase of energy processing.
## This dependency is captured in the top-directory makefile
level1B: $(L1BLOGS)

level2: level1 $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(AGLUPROC)/logs/LA1%.log: $(AGLUPROC)/level1/LA1%
	echo $?
	$(RCMD) $< $(AGLUPROC)/logs/LA1.Rout

$(AGLUPROC)/logs/LB1%.log: $(AGLUPROC)/level1/LB1%
	echo $?
	$(RCMD) $< $(AGLUPROC)/logs/LB1.Rout

$(AGLUPROC)/logs/L2%.log: $(AGLUPROC)/level2/L2%
	echo $?
	$(RCMD) $< $(AGLUPROC)/logs/L2.Rout

$(AGLUXML)/%.xml: $(AGLUPROC)/xml-batch/batch_%.xml
	echo $?
	java -Xmx2g -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(AGLUDAT)/level1/*.csv
	-rm -f $(AGLUDAT)/level2/*.csv
	-rm -f $(AGLUPROC)/logs/*.d
	-rm -f $(AGLUPROC)/logs/*.log
	-rm -f $(AGLUPROC)/xml-batch/*.xml

level2clean:
	-rm -f $(AGLUPROC)/logs/L2*.d
	-rm -f $(AGLUPROC)/logs/L2*.log
	-rm -f $(AGLUXML)/*.xml
	-rm -f $(AGLUPROC)/xml-batch/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'l1A logs:  ' $(L1ALOGS)
	echo 'l1B logs:  ' $(L1BLOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
