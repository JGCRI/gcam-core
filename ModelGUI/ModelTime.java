/*
 * ModelTime.java
 *
 * Created on June 4, 2003, 4:37 PM
 */

package ModelGUI;

import org.jdom.*;
import java.util.*;

/** This class is used by ControlPanel for year-related calculations, 
 * specifically when determining how to display values the table view of nodes 
 * that have "year" attributes. If a node has a "year" attribute, ControlPanel
 * assumes that this node is one cell in a row; the first column of that row 
 * corresponds to startYear, and the last column is endYear.
 *
 * This class assumes that the original XML file has a modeltime node with 
 * children startYear, interYear1, interYear2, endYear, timeStep1, timeStep2,
 * timeStep3.
 * 
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

public class ModelTime {
    private int startYear;
    private int interYear1;
    private int interYear2;
    private int endYear;
    private int timeStep1;
    private int timeStep2;
    private int timeStep3;
    private Vector years;
    
    /** Creates a new instance of ModelTime. Reads in time data from the XML 
     * file and populates class variables.
     *
     * @param root the root of the JDOM tree representing the XML file. 
     *      <code> root </code> should either be the "modeltime" node, or be an 
     *      ansestor of "modeltime" */
    public ModelTime(AdapterNode root) {
        
        Vector queue = new Vector();
        while (!root.getName().equals("modeltime")) {
            queue.addAll(root.getChildren());
            root = (AdapterNode)queue.elementAt(0);
            queue.removeElementAt(0);
        }

        startYear = new Integer(root.getChild("startyear", "").getText()).intValue();
        interYear1 = new Integer(root.getChild("interyear1", "").getText()).intValue();
        interYear2 = new Integer(root.getChild("interyear2", "").getText()).intValue();
        endYear = new Integer(root.getChild("endyear", "").getText()).intValue();
        timeStep1 = new Integer(root.getChild("timestep1", "").getText()).intValue(); 
        timeStep2 = new Integer(root.getChild("timestep2", "").getText()).intValue();
        timeStep3 = new Integer(root.getChild("timestep3", "").getText()).intValue();
        
        //list out the model years
        years = new Vector();
        int j;
        for (j = startYear; j < interYear1; j += timeStep1) {
            years.addElement(new Integer(j));
        }
        for (j = interYear1; j < interYear2; j += timeStep2) {
            years.addElement(new Integer(j));
        }
        for (j = interYear2; j <= endYear; j += timeStep3) {
            years.addElement(new Integer(j));
        }
    }
    
    /** Returns the first year that can be used in the data.
     *
     * @return int representing the first year*/    
    public int getStart() {
        return startYear;
    }
    
    /** Returns the last year that can be used in the data.
     *
     * @return int representation of last year */    
    public int getEnd() {
        return endYear;
    }

    /** Returns the total number of discrete time intervals that can 
     * appear in the data.
     *
     * @return the number of columns needed to display a row that contains
     * time-sensitive values */    
    public int getNumOfSteps() {        
        return years.size();
    }
    
    /** Retrieves a chronological Vector of ints with the numerical values of 
     * all years used in the data.
     * @return the years that need to be represented for elements that have a
     *      "year" attribute */    
    public Vector getTimeIntervals() {
        return years;
    }
    
    /** Retrieves the year at a specified index. This function is particularly 
     * useful for creating headers of table columns if one of the dimentions 
     * of the table is years or time intervals.
     *
     * @param index the index of the desired value in the years array, starting 
     *      at 0
     * @return a String representaion of the desired year */    
    public String getYear(int index) {
        if (index < years.size()) {
            Integer year = (Integer)years.elementAt(index);
            return year.toString();
        }
        else return null;
    }
    
    /** Retrieves the index in the years array of a specific year.
     *
     * @param yearStr String representation of target year
     * @return the index of the year, or -1 if the year is not part of the data */    
    public int getYearIndex(String yearStr) {
        Integer year = new Integer(yearStr);
        return years.indexOf(year);
    }
    
}
