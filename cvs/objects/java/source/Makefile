
install: XMLDBDriver.jar
	cp XMLDBDriver.jar ../../../../exe

# NOTE: The third party jars are assumed to be listed in the CLASSPATH
# environment variable
XMLDBDriver.jar: *.class
	jar -cf $@ *.class

%.class: %.java
	javac $^

clean:
	rm -f XMLDBDriver.jar
	rm -f *.class
