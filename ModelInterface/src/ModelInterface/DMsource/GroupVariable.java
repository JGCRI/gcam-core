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
 * \file *file*
 * \ingroup *project*
 * \brief *brief description*
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
package ModelInterface.DMsource;

import java.util.*;
import java.io.*;


/**
 * 
 */
public class GroupVariable extends Variable
{
  String region;	//the region this variable addresses
  TreeMap data;	//list of the variables in this group
  boolean isRef;	//are the variables reference vars
  boolean isRegion;	//is this group a region of subregions
  boolean isTime;	//is this group a single region & field, over many times
  
  //***************************************************************************
  //************************Constructors***************************************
  //***************************************************************************
  public GroupVariable()
  {
    name = null;
    comment = null;
    isRef = true;
    data = new TreeMap();
  }
  
  public GroupVariable(String n)
  {
    name = n;
    comment = null;
    isRef = true;
    data = new TreeMap();
  }
  
  public GroupVariable(String n, GroupVariable copy)
  {
    name = n;
    comment = copy.comment;
    region = copy.region;
    isRef = copy.isRef;
    isRegion = copy.isRegion;
    isTime = copy.isTime;
    data = new TreeMap();
  }
  
  //***************************************************************************
  //******************Variable Functions***************************************
  //***************************************************************************
  public void printStandard(BufferedWriter out) throws IOException
  {
    Map.Entry ent;
    Variable hold;
    Iterator it = data.entrySet().iterator();
    
    while(it.hasNext())
    {
      ent = (Map.Entry)it.next();
      hold = (Variable)ent.getValue();
      hold.printStandard(out);
    }
  }
  public void printVerbose(BufferedWriter out) throws IOException
  {
    Map.Entry ent;
    Variable hold;
    Iterator it = data.entrySet().iterator();
    
    out.newLine();
    out.write("Group: "+name);
    out.newLine();
    if(comment != null)
    {
      out.write("\tDescription: "+comment);
      out.newLine();
    }
    while(it.hasNext())
    {
      ent = (Map.Entry)it.next();
      hold = (Variable)ent.getValue();
      hold.printVerbose(out);
    }
    out.write("End of Group: "+name);
    out.newLine();
  }  
  public Wrapper[] getData() {
	  return getDataOrWeight(true);
  }
  public Wrapper[] getWeight() {
	  return getDataOrWeight(false);
  }
  /**
   * Gets the data or weight from each of the vars in
   * this group and returns it.  If isData is true it
   * returns Wrappers of data, if false then Wrappers
   * of weights.
   * @param isData Whether to get data or weights
   * @return An array of wrappers for the data or 
   * 	weights depending on the isData param.
   */
  public Wrapper[] getDataOrWeight(boolean isData)
  {
    ArrayList toRet = new ArrayList(0);
    Map.Entry ent;
    Variable holdVar;
    Wrapper[] holdWrap;
    Iterator it = data.entrySet().iterator();
    
    while(it.hasNext())
    { //iterate through all contained variables
      ent = (Map.Entry)it.next();
      holdVar = (Variable)ent.getValue();
      if(isData) {
	      holdWrap = holdVar.getData();
      } else {
	      holdWrap = holdVar.getWeight();
      }
      toRet.ensureCapacity(toRet.size()+holdWrap.length);
      
      for(int i = 0; i < holdWrap.length; i++)
      { //iterate through each variables data, add it all
        toRet.add(holdWrap[i]);
      }
    }
    return (Wrapper[])toRet.toArray();
  }
  public void setData(Wrapper[] d)
  {
    Map.Entry ent;
    Variable holdVar;
    Wrapper[] holdWrap;
    int num = 0;
    Iterator it = data.entrySet().iterator();
    
    while(it.hasNext())
    { //iterate through all contained variables
      ent = (Map.Entry)it.next();
      holdVar = (Variable)ent.getValue();
      holdWrap = holdVar.getData();
      
      for(int i = 0; i < holdWrap.length; i++)
      { //iterate through each variables data, writing over each wrapper
        holdWrap[i] = d[num];
        num++;
      }
      
      holdVar.setData(holdWrap);
    }
  }
  public boolean isReference()
  {
    return isRef;
  }
  public boolean isGroup()
  {
    return true;
  }
  public Variable getCopy(String n)
  {
    return new GroupVariable(n, this);
  }
  public Variable getShape(String n)
  {
    GroupVariable hold = new GroupVariable(n, this);
    hold.data = this.data;
    return hold;
  }
  
  //***************************************************************************
  //******************Personal Functions***************************************
  //***************************************************************************
  public void addData(Variable var)
  {
    if(!var.isReference())
    {
      isRef = false;
    }
    data.put(var.name, var);
  }

  //***************************************************************************
}
