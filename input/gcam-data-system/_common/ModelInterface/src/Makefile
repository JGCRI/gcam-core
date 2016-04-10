
all: CSVToXML.jar

CSVToXML.jar: ModelInterface/ModelGUI2/csvconv/*.class
	jar -cmf MANIFEST.MK $@ ModelInterface

ModelInterface/ModelGUI2/csvconv/%.class: ModelInterface/ModelGUI2/csvconv/%.java
	javac $^

ModelInterface.jar: ModelInterface/InterfaceMain.class
	jar -cmf MANIFEST_MI.MK $@ ModelInterface

ModelInterface/InterfaceMain.class: ModelInterface/InterfaceMain.java
	# Note needed libraries are assumped to be in the jars directory
	# WARNING: dependency tracking is handled internally by javac however
	# is very flakey and it is recommnded to always do a clean build.
	javac -cp jars/*:. $^

clean:
	rm CSVToXML.jar
	find ModelInterface -name *.class -delete
