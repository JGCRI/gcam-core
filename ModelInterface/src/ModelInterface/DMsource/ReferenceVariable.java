/*
 * This software, which is provided in confidence, was prepared by employees
        of Pacific Northwest National Laboratory operated by Battelle Memorial
        Institute. Battelle has certain unperfected rights in the software 
        which should not be copied or otherwise disseminated outside your 
        organization without the express written authorization from Battelle. All rights in
        the software are reserved by Battelle.  Battelle makes no warranty,
        express or implied, and assumes no liability or responsibility for the
        use of this software.
 */
/*!
 * \file ReferenceVariable.java
 * \ingroup DataManipulation
 * \brief Extension of Variable which is a collection of region data and so contains shape and location.
 *
 *  The most commonly used extension of Variable. Stores matrices of data which corresponds to previously
 * defined regions. As such additional bounding information and a weightmask for how much of each block
 * lies in this region are stored.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package ModelInterface.DMsource;

import java.text.DecimalFormat;
import java.io.*;


/**
 * Extension of Variable which is a collection of region data and so contains shape and location.
 * The most commonly used extension of Variable. Stores matrices of data which corresponds to previously
 * defined regions. As such additional bounding information and a weightmask for how much of each block
 * lies in this region are stored.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class ReferenceVariable extends Variable
{
  Wrapper[] data;	//the data this variable holds
  String region;	//the region this variable addresses
  String reference;
  String units;
  boolean avg;
  Wrapper[] weight; //um ok, im storing a bitmask for this thing in itself
  double x;
  double y;
  double w;
  double h;
  double res;
  
  //***************************************************************************
  //************************Constructors***************************************
  //***************************************************************************
  
  /**
   * Default reference variable constuctor, assumes region of world.
   *
   */
  public ReferenceVariable()
  {
    avg = true;
    name = null;
    comment = null;
    reference = null;
    units = null;
    region = "world";
    x = 0;
    y = 0;
    w = 0;
    h = 0;
    res = 1;
  }
  /**
   * Copy constructor for reference variable. Uses the bounding information of the
   * passed in variable but does not copy data. Additionally has its own unique
   * name.
   * @param n Name of this new variable.
   * @param var Variable whos information will be used to construct this variable.
   */
  public ReferenceVariable(String n, ReferenceVariable var)
  {
    avg = var.avg;
    name = n;
    comment = null;
    reference = null;
    units = null;
    region = var.region;
    x = var.x;
    y = var.y;
    w = var.w;
    h = var.h;
    res = var.res;
    weight = var.weight;
  }
  /**
   * Creates a reference variable with information from a region, but does not fill data.
   * @param n Name desired for this variable.
   * @param r Region whos shape this variables data will take.
   */
  public ReferenceVariable(String n, Region r)
  {
    avg = true;
    name = n;
    comment = null;
    reference = null;
    units = null;
    region = r.name;
    x = r.x;
    y = r.y;
    w = r.width;
    h = r.height;
    res = r.resolution;
    weight = r.getWorkingM("weight", "0");
  } 
  /**
   * Creates a reference variable with information, including data of passed variable
   * and time, from the given region.
   * @param n Name desired for this variable.
   * @param r Region whos shape this variables data will take.
   * @param v Variable whos data will fill this variable.
   * @param t Time for which to get the variables data.
   * @param a Whether or not the data in this variable is averaged (or additive) on aggregation.
   */
  public ReferenceVariable(String n, Region r, String v, String t, boolean a)
  {
    avg = a;
    name = n;
    comment = null;
    reference = null;
    units = null;
    region = r.name;
    x = r.x;
    y = r.y;
    w = r.width;
    h = r.height;
    res = r.resolution;
    data = r.getWorkingM(v, t);
    weight = r.getWorkingM("weight", "0");
  }
  
  //***************************************************************************
  //******************Variable Functions***************************************
  //***************************************************************************

  public void printStandard(BufferedWriter out) throws IOException
  {
    double[][] toPrint;
    
    //building a complete matrix of values to print from sub regions
    toPrint = buildMatrix();
    
    out.newLine();
    for(int i = 0; i < toPrint.length; i++)
    {
      for(int k = 0; k < (toPrint[i].length-1); k++)
      {
        if(Double.isNaN(toPrint[i][k]))
          out.write("NaN,");
        else
          out.write(toPrint[i][k]+",");
      }
      if(Double.isNaN(toPrint[i][toPrint[i].length-1]))
        out.write("NaN");
      else
        out.write(toPrint[i][toPrint[i].length-1]+"");
      out.newLine();
      out.flush();
    }
    
  }
  public void printVerbose(BufferedWriter out) throws IOException
  {
    DecimalFormat form = new DecimalFormat("0.0");
    double[][] toPrint;
    
    
    //building a complete matrix of values to print from sub regions
    toPrint = buildMatrix();
    
    out.newLine();
    out.write(name+":");
    out.newLine();
    out.write("\tfrom x: "+x+" to "+(x+w)+" y: "+y+" to "+(y+h)+" at resolution: "+res);
    out.newLine();
    if(units != null)
    {
      out.write("\tIn units of: "+units);
      out.newLine();
    }
    if(comment != null)
    {
      out.write("\tDescription: "+comment);
      out.newLine();
    }
    if(reference != null)
    {
      out.write("\tReferenced From: "+reference);
      out.newLine();
    }
    for(int i = 0; i < toPrint.length; i++)
    {
      out.write("\t");
      for(int k = 0; k < (toPrint[i].length-1); k++)
      {
        if(Double.isNaN(toPrint[i][k]))
          out.write("NaN,");
        else
          out.write(form.format(toPrint[i][k])+",");
      }
      if(Double.isNaN(toPrint[i][toPrint[i].length-1]))
        out.write("NaN");
      else
        out.write(form.format(toPrint[i][toPrint[i].length-1]));
      out.newLine();
      out.flush();
    }
    
  }
  public Wrapper[] getData()
  {
    return data;
  }
  public Wrapper[] getWeight() {
	  return weight;
  }
  public void setData(Wrapper[] d)
  {
    data = d;
  }
  public boolean isReference()
  {
    return true;
  }
  public boolean isGroup()
  {
    return false;
  }
  public Variable getCopy(String n)
  {
    ReferenceVariable hold = new ReferenceVariable(n, this);
    hold.data = data;
    return hold;
  }
  public Variable getShape(String n)
  {
    return new ReferenceVariable(n, this);
  }
  
  //***************************************************************************
  //******************Personal Functions***************************************
  //***************************************************************************
  /**
   * Sets this variables data equal to the data in the passed matrix.
   * 
   * @param r Region from which to get data.
   * @param v Variable from which to get data.
   * @param t Time at which to get data.
   */
  public void fillData(Region r, String v, String t)
  { //this is necessary if this doesnt just reference a region but starts with that regions data
    data = r.getWorkingM(v, t);
  }
  public double[][] buildMatrix()
  {
    int offsetY, offsetX;
    double[][] holdM;
    double[][] holdWM;
    ReferenceWrapper holdD;
    ReferenceWrapper holdW;
    double[][] toPrint = new double[(int)(h/res)][(int)(w/res)];
    //double[][] toPrint = new double[(int)(180/res)][(int)(360/res)];
    
    for(int i = 0; i < toPrint.length; i++)
    {
      for(int k = 0; k < toPrint[i].length; k++)
      {
        toPrint[i][k] = Double.NaN;
      }
    }
    
    //building a complete matrix of values to print from sub regions
    for(int i = 0; i < data.length; i++)
    {
      holdD = (ReferenceWrapper)data[i];
      holdM = holdD.data;
      holdW = (ReferenceWrapper)weight[i];
      holdWM = holdW.data;
      offsetY = (int)(((y+h)-(holdD.y+holdD.height))/res);
      offsetX = (int)((holdD.x-x)/res);
      //offsetY = (int)(((90)-(holdD.y+holdD.height))/res);
      //offsetX = (int)((holdD.x+180)/res);
      
      for(int iY = 0; iY < holdM.length; iY++)
      {
        for(int iX = 0; iX < holdM[iY].length; iX++)
        {
          if(!Double.isNaN(holdM[iY][iX]))
          {
            if(Double.isNaN(toPrint[(offsetY+iY)][(offsetX+iX)]))
            {
		    if(avg) {
			    toPrint[(offsetY+iY)][(offsetX+iX)] = ((holdM[iY][iX])*holdWM[iY][iX]);
		    } else {
			    toPrint[(offsetY+iY)][(offsetX+iX)] = holdM[iY][iX];
		    }
            } else
            {
		    if(avg) {
			    toPrint[(offsetY+iY)][(offsetX+iX)] += ((holdM[iY][iX])*holdWM[iY][iX]);
		    } else {
			    toPrint[(offsetY+iY)][(offsetX+iX)] += holdM[iY][iX];
		    }
              /*
              if(toPrint[(offsetY+iY)][(offsetX+iX)] > 1)
              {
                System.out.println("still a problem: "+(holdM[iY][iX])+"->"+toPrint[(offsetY+iY)][(offsetX+iX)]+" at "+(offsetY+iY)+", "+(offsetX+iX)+" with weight "+holdWM[(iY)][(iX)]);
              }
              */
            }
          }
        }
      }
    }
    return toPrint;
  }
  public double[][] buildWorldMatrix()
  {
    //System.out.println(x+", "+y+", "+w+", "+h);
    int offsetY, offsetX;
    int offsetWy, offsetWx;
    double[][] holdM;
    double[][] holdWM;
    ReferenceWrapper holdD;
    ReferenceWrapper holdW;
    double[][] toPrint = new double[(int)(180/res)][(int)(360/res)];
    
    for(int i = 0; i < toPrint.length; i++)
    {
      for(int k = 0; k < toPrint[i].length; k++)
      {
        toPrint[i][k] = Double.NaN;
      }
    }
    
    //building a complete matrix of values to print from sub regions
    for(int i = 0; i < data.length; i++)
    {
      //System.out.println("adding region: "+((ReferenceWrapper)data[i]).name);
      holdD = (ReferenceWrapper)data[i];
      holdM = holdD.data;
      holdW = (ReferenceWrapper)weight[i];
      holdWM = holdW.data;
      offsetY = (int)(((90)-(holdD.y+holdD.height))/res);
      offsetX = (int)((holdD.x+180)/res);
      //System.out.println(offsetX+", "+offsetY);
      
      for(int iY = 0; iY < holdM.length; iY++)
      {
        for(int iX = 0; iX < holdM[iY].length; iX++)
        {
          if(!Double.isNaN(holdM[iY][iX]))
          {
            if(Double.isNaN(toPrint[(offsetY+iY)][(offsetX+iX)]))
            {
		    if(avg) {
			    toPrint[(offsetY+iY)][(offsetX+iX)] = ((holdM[iY][iX])*holdWM[iY][iX]);
		    } else {
			    toPrint[(offsetY+iY)][(offsetX+iX)] = holdM[iY][iX];
		    }

            } else
            {
              //System.out.println("added twice: "+(90-((offsetY+iY)*res))+", "+(((offsetX+iX)*res)-180));
		    if(avg) {
			    toPrint[(offsetY+iY)][(offsetX+iX)] += ((holdM[iY][iX])*holdWM[iY][iX]);
		    } else {
			    toPrint[(offsetY+iY)][(offsetX+iX)] += holdM[iY][iX];
		    }
              /*
              if(toPrint[(offsetY+iY)][(offsetX+iX)] > 1)
              {
                System.out.println("still a problem: "+(holdM[iY][iX])+"->"+toPrint[(offsetY+iY)][(offsetX+iX)]+" at "+(90-((offsetY+iY)*res))+", "+(((offsetX+iX)*res)-180)+" with weight "+holdWM[(iY)][(iX)]);
              }
              */
            }
          }
        }
      }
    }
    
    return toPrint;
    
    /*
     * if((holdM[iY][iX]) > 1)
              {
                System.out.println("serious problem: "+(holdM[iY][iX])+" at "+(offsetY+iY)+", "+(offsetX+iX)+" with weight "+holdWM[(iY)][(iX)]);
              }
     * 
     * if(toPrint[(offsetY+iY)][(offsetX+iX)] > 1)
              {
                System.out.println("still a problem: "+(holdM[iY][iX])+"->"+toPrint[(offsetY+iY)][(offsetX+iX)]+" at "+(offsetY+iY)+", "+(offsetX+iX)+" with weight "+holdWM[(iY)][(iX)]);
              }
     */
  }
  
  //***************************************************************************
}

