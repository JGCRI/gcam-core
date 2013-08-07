# The arguments to R --no-save and --no-restore tells R not to use / create
# the .R_data workspace file.  This speeds up the processing time greatly
# however the .R_data is useful for debugging.  If a user does indeed want
# it generated they should remove the --no-save --no-restore from the command
# below:
RCMD	= R CMD BATCH --no-save --no-restore

## The way we handle directories here is kind of a mess.  We should replace with vpath (gpath?)
export SOCIOPROC = ../socioeconomics-processing-code
export SOCIODAT	= .
export SOCIOXML = ../xml/socioeconomics-xml
export COMMON = ../_common

L1SCRIPTS = $(wildcard $(SOCIOPROC)/level1/*.R)
L1LOGS	  = $(patsubst %.R,%.R.log,$(subst /level1/,/logs/,$(L1SCRIPTS)))

L2SCRIPTS = $(wildcard $(SOCIOPROC)/level2/*.R)
L2LOGS    = $(patsubst %.R,%.R.log,$(subst /level2/,/logs/,$(L2SCRIPTS)))

LOGDEPS	  = $(wildcard $(SOCIOPROC)/logs/*.d)

XML_BATCH_FILES = $(wildcard $(SOCIOPROC)/xml-batch/*.xml)
XML_FILES = $(subst batch_,,$(subst $(SOCIOPROC)/xml-batch,$(SOCIOXML),$(XML_BATCH_FILES)))

include $(LOGDEPS)

all: level1 level2 XML_BATCH

level1: $(L1LOGS)

level2: level1 $(L2LOGS)

XML_BATCH: level2 $(XML_FILES)

$(SOCIOPROC)/logs/L1%.log: $(SOCIOPROC)/level1/L1%
	@echo $?
	$(RCMD) $< $(SOCIOPROC)/logs/L1.Rout

$(SOCIOPROC)/logs/L2%.log: $(SOCIOPROC)/level2/L2%
	@echo $?
	$(RCMD) $< $(SOCIOPROC)/logs/L2.Rout

$(SOCIOXML)/%.xml: $(SOCIOPROC)/xml-batch/batch_%.xml
	@echo $?
	java -jar $(COMMON)/ModelInterface/src/CSVToXML.jar $<

clean:
	-rm -f $(SOCIOPROC)/logs/*.log
	-rm -f $(SOCIOPROC)/logs/*.d
	-rm -f $(SOCIOPROC)/xml-batch/*.xml
	-rm -f $(SOCIOXML)/*.xml

test:
	echo 'deps:  ' $(L1DEPS)
	echo 'logfiles:  ' $(L1LOGS)
	echo 'l2logs:  ' $(L2LOGS)
	echo 'xml files:  ' $(XML_FILES)
