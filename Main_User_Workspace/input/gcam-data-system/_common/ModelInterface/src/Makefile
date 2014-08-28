
all: CSVToXML.jar

CSVToXML.jar: ModelInterface/ModelGUI2/csvconv/*.class
	jar -cmf MANIFEST.MK $@ ModelInterface

ModelInterface/ModelGUI2/csvconv/%.class: ModelInterface/ModelGUI2/csvconv/%.java
	javac $^

clean:
	rm CSVToXML.jar
	find ModelInterface -name *.class -delete
