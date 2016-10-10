
all: CSVToXML.jar

CSVToXML.jar: ModelInterface/ModelGUI2/csvconv/*.class
	jar -cmf MANIFEST.MK $@ ModelInterface

ModelInterface/ModelGUI2/csvconv/%.class: ModelInterface/ModelGUI2/csvconv/%.java
	javac $^

# NOTE: dependency tracking is handled internally by javac however
# is very flakey and we recommnded to always do a clean build.
ModelInterface.jar: clean_MI ModelInterface/InterfaceMain.class
	jar -cmf MANIFEST_MI.MK $@ ModelInterface

ModelInterface/InterfaceMain.class: ModelInterface/InterfaceMain.java
	# NOTE: The third party jars are assumed to be listed in the CLASSPATH
	# environment variable
	javac $^

clean:
	rm -f CSVToXML.jar
	rm -f ModelInterface.jar
	find ModelInterface -name *.class -delete

clean_CSVToXML:
	rm -f CSVToXML.jar
	find ModelInterface -name *.class -delete

clean_MI:
	rm -f ModelInterface.jar
	find ModelInterface -name *.class -delete

