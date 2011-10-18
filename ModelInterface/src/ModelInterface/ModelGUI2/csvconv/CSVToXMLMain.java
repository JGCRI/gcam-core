package ModelInterface.ModelGUI2.csvconv;

import java.io.File;
import java.io.FileInputStream;
import java.io.DataInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileWriter;
import java.io.File;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import javax.swing.JOptionPane;
import javax.swing.JFrame;

import org.w3c.dom.Document;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

/**
 * A stand alone driver to run the CSV to XML conversion tool.
 * This class serves as both a static utility to to run the conversion as well
 * as a main command line program.  The commandline expects at least three arguments
 * the first of which is one or more CSV files, next the header file and finally
 * where to write the resulting XML file.
 *
 * @author Pralit Patel
 */ 
public class CSVToXMLMain {
    public static void main(String[] args) {
        if(args.length < 3) {
            System.err.println("Usage: CSVToXMLMain <CSV file> [<CSV file> ..] <header file> <output XML file>");
            System.exit(1);
        }
        try {
            File xmlOutputFile = new File(args[args.length - 1]);
            File headerFile = new File(args[args.length - 2]);
            File[] csvFiles = new File[args.length - 2];
            for(int i = 0; i < args.length - 2; ++i) {
                csvFiles[i] = new File(args[i]);
            }
            Document doc = runCSVConversion(csvFiles, headerFile, null);
            writeFile(xmlOutputFile, doc);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }

    }

    /**
	 * Takes a CSV file, and Headers file, then processes the files by building
	 * a new tree with the DOMTreeBuilder class.  The resulting XML document is
     * returned
	 * 
	 * @param csvFiles
	 *            the CSV files
	 * @param headerFile
	 *            the Headers file
     * @param parentFrame
     *            A GUI frame which may be used to display error messages to
     *            if it is not null.
     * @return
     *            The generated XML dom document
	 */
    public static Document runCSVConversion(File[] csvFiles, File headerFile, JFrame parentFrame) {
        StringTokenizer st;
        String intValueStr = null;
        String strToReplace;
        int counter;
        int dollarindex = 0;
        String inputLine;

        ArrayList<String> dataArr;
        HashMap<String, String> nickNameMap = new HashMap<String, String>(); // shortname -> long string to
        // append to end
        HashMap<String, String> tableIDMap = new HashMap<String, String>(); // tableID -> long string of headers
        DOMTreeBuilder tree = new DOMTreeBuilder();

        try {

            FileInputStream hashfis = new FileInputStream(headerFile);
            DataInputStream hashfin = new DataInputStream(hashfis);
            BufferedReader hashInput = new BufferedReader(
                    new InputStreamReader(hashfin));
            hashInput.readLine(); // ignores first line of file
            inputLine = hashInput.readLine().trim();
            while (inputLine != null && inputLine.length() > 0 && inputLine.charAt(0) == '$') { // read in
                // header
                // nick
                // names
                st = new StringTokenizer(inputLine, ",", false);
                intValueStr = st.nextToken(); // $nickname
                inputLine = inputLine.substring(intValueStr.length() + 1)
                    .trim();
                nickNameMap.put(intValueStr, inputLine);
                if ((inputLine = hashInput.readLine()) != null) {
                    inputLine.trim();
                }
            }
            while (inputLine != null) {
                if (!inputLine.equals("")) {
                    st = new StringTokenizer(inputLine, ",", false);
                    intValueStr = st.nextToken(); // numID
                    inputLine = inputLine.substring(intValueStr.length() + 1); // everything
                    // but
                    // numID
                    try {

                        inputLine = inputLine.replaceAll("[,][\\s]*[,]", ""); // gets
                        // rid
                        // of
                        // end
                        // commas
                        if (inputLine.endsWith(",")) { // gets ride of last
                            // comma if there is one
                            inputLine = inputLine.substring(0, inputLine
                                    .length() - 1);
                        } // extra commas are now all gone

                        dollarindex = 0;
                        while ((dollarindex = inputLine.indexOf('$')) != -1) {
                            counter = dollarindex;
                            while (counter < inputLine.length()
                                    && inputLine.charAt(counter) != ',') {
                                counter++;
                                    }
                            strToReplace = inputLine.substring(dollarindex,
                                    counter);
                            if (nickNameMap.containsKey(strToReplace)) {
                                //strToReplace = strToReplace.substring(1);
                                //strToReplace = "^[.]*"+strToReplace+"[.]*$";
                                inputLine = inputLine.replaceAll("\\"
                                        + strToReplace, nickNameMap.get(strToReplace));
                            } else {
                                System.out
                                    .println("***Couldn't find replacement for "
                                            + strToReplace + "!***");
                                if(parentFrame != null) {
                                    JOptionPane.showMessageDialog(parentFrame,
                                            "Couldn't find replacement for "
                                            + strToReplace, "Warning",
                                            JOptionPane.WARNING_MESSAGE);
                                }
                            }
                        }
                        tableIDMap.put(intValueStr, inputLine);
                    } catch (NumberFormatException e) {
                        System.out
                            .println("*** Hashtable file formatted incorrectly ***"
                                    + e);
                        if(parentFrame != null) {
                            JOptionPane.showMessageDialog(parentFrame,
                                    "Hashtable file formatted incorrectly\n" + e,
                                    "Exception", JOptionPane.ERROR_MESSAGE);
                        }
                    }
                }
                if ((inputLine = hashInput.readLine()) != null) {
                    inputLine.trim();
                }
            }

            // tableIDMap should now be all set up ...

            for(int j = 0; j < csvFiles.length; ++j) {
                FileInputStream fis = new FileInputStream(csvFiles[j]);
                DataInputStream fin = new DataInputStream(fis);
                BufferedReader stdInput = new BufferedReader(new InputStreamReader(
                            fin));

                inputLine = stdInput.readLine().trim(); // read one line of input

                while (inputLine != null) {
                    while (inputLine != null
                            && !inputLine.startsWith("INPUT_TABLE")) {
                        inputLine = stdInput.readLine();
                            }
                    if (inputLine == null) {
                        break;
                    }
                    stdInput.readLine(); // reads/ignores "Variable ID" line
                    inputLine = stdInput.readLine().trim(); // should have just the
                    // id number
                    st = new StringTokenizer(inputLine, ",", false);
                    intValueStr = st.nextToken();

                    if (tableIDMap.containsKey(intValueStr)) {
                        tree.setHeader(tableIDMap.get(intValueStr));
                        stdInput.readLine(); // ignores this line
                        stdInput.readLine(); // ignores header line

                        inputLine = stdInput.readLine().trim(); // start reading in
                        // data
                        while (inputLine != null && !inputLine.equals("")
                                && inputLine.charAt(0) != ',') {
                            st = new StringTokenizer(inputLine, ",", false);
                            int NUM_COLS = st.countTokens();
                            dataArr = new ArrayList<String>(NUM_COLS);
                            for (int i = 0; i < NUM_COLS; i++) {
                                dataArr.add(i, (st.nextToken()).trim());
                            } // one line of data stores in arraylist
                            tree.addToTree(dataArr);
                            //makeTree( rootElement, docName );
                            dataArr.clear();
                            if ((inputLine = stdInput.readLine()) != null) {
                                inputLine.trim();
                            }
                                }
                    } else {
                        System.out.println("***Warning: skipping table: "
                                + intValueStr + "!***");
                    }

                    if ((inputLine = stdInput.readLine()) != null) {
                        inputLine.trim();
                    }
                }
                fin.close();
                hashfin.close();
            }

            return tree.getDoc();


        } catch (Exception e) {
            System.out.println("intValueStr "+intValueStr);
            System.out
                .println("Excpetion thrown while trying to read csv and header files");
            e.printStackTrace();
            if(parentFrame != null) {
                JOptionPane.showMessageDialog(parentFrame,
                        "Excpetion thrown while trying to read csv and header files\n"
                        + e, "Exception", JOptionPane.ERROR_MESSAGE);
            }
            return null;
        }
    }

    /**
	 * Writes the DOM document to the specified file.
     * Note this is used in many places and should really be in a utility
     * however in this case we need that utility to have no extra library
     * dependencies.
	 * 
	 * @param file
	 *            where the XML tree will be written to
	 * @param thDoc
	 *            the tree that should be written
	 * @return whether the file was actually written or not
	 */
	public static boolean writeFile(File file, Document theDoc) {
		// specify output formating properties
		OutputFormat format = new OutputFormat(theDoc);
		format.setEncoding("UTF-8");
		format.setLineSeparator("\r\n");
		format.setIndenting(true);
		format.setIndent(3);
		format.setLineWidth(0);
		format.setPreserveSpace(false);
		format.setOmitDocumentType(true);

		// create the searlizer and have it print the document

		try {
			FileWriter fw = new FileWriter(file);
			XMLSerializer serializer = new XMLSerializer(fw, format);
			serializer.asDOMSerializer();
			serializer.serialize(theDoc);
			fw.close();
		} catch (java.io.IOException e) {
			System.err.println("Error outputing tree: " + e);
			return false;
		}
		return true;
	}
}
