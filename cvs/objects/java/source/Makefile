# Java compiler options to target specific JVM versions to allow
# backwards compatibility.
JAVA_TARGET = -target 1.8 -source 1.8

install: XMLDBDriver.jar
	cp XMLDBDriver.jar ../../../../exe

# NOTE: The third party jars are assumed to be listed in the CLASSPATH
# environment variable
XMLDBDriver.jar: *.class
	jar -cf $@ *.class

%.class: %.java
	javac $(JAVA_TARGET) $^

clean:
	rm -f XMLDBDriver.jar
	rm -f *.class
