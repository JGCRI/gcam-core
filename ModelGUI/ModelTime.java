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
    private int endYear;
    private int timeStep;
    private int numSteps;
    
    /** Creates a new instance of ModelTime */
    public ModelTime(Document document) {
        startYear = 0;
        endYear = 0;
        timeStep = 0;
        numSteps = 0;
        
        Element root = document.getRootElement();
        root = root.getChild("modeltime");
        if (root != null) {
            String temp = root.getChild("startyear").getText();
            startYear = new Integer(temp).intValue();
            temp = root.getChild("endyear").getText();
            endYear = new Integer(temp).intValue();
            temp = root.getChild("datatimestep").getText();
            timeStep = new Integer(temp).intValue();
        }
    }
    
    public int getStart() {
        return startYear;
    }
    
    public int getEnd() {
        return endYear;
    }
    
    public int getStep() {
        return timeStep;
    }
    
    public int getNumOfSteps() {
        //if numSteps has not been calculated before
        if (numSteps == 0) {
            for (int j = startYear; j < endYear; j += timeStep) numSteps++;
        }
        
        return numSteps;
    }
    
    public Vector getTimeIntervals() {
        Vector intervals = new Vector();
        for (int j = startYear; j <= endYear; j += timeStep) {
            intervals.addElement(String.valueOf(j));
        }
        return intervals;
    }
    
}
