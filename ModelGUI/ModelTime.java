/*
 * ModelTime.java
 *
 * Created on June 4, 2003, 4:37 PM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */
import org.jdom.*;
import java.util.*;

public class ModelTime {
    private int startYear;
    private int interYear1;
    private int interYear2;
    private int endYear;
    private int timeStep1;
    private int timeStep2;
    private int timeStep3;
    private Vector years;
    
    /** Creates a new instance of ModelTime */
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
    
    public int getStart() {
        return startYear;
    }
    
    public int getEnd() {
        return endYear;
    }
    
    /*public int getStep() {
        return timeStep;
    }*/
    
    public int getNumOfSteps() {        
        return years.size();
    }
    
    public Vector getTimeIntervals() {
        return years;
    }
    
    public String getYear(int index) {
        if (index < years.size()) {
            Integer year = (Integer)years.elementAt(index);
            return year.toString();
        }
        else return null;
    }
    
    public int getYearIndex(String yearStr) {
        Integer year = new Integer(yearStr);
        return years.indexOf(year);
    }
    
}
