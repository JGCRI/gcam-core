/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
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
	  return getDataOrWeight(0);
  }
  public Wrapper[] getWeight() {
	  return getDataOrWeight(1);
  }
  public Wrapper[] getLandFract() {
	  return getDataOrWeight(2);
  }
  /**
   * Gets the data or weight from each of the vars in
   * this group and returns it.  If isData is 0 it
   * returns Wrappers of data, if 1 then Wrappers
   * of weights and if it is 2 it will return landFract.
   * @param isData Which type of data to get(data/weight/landFract).
   * @return An array of wrappers for the data or 
   * 	weights depending on the isData param.
   */
  private Wrapper[] getDataOrWeight(int isData)
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
      switch(isData) {
	      case 0:
		      holdWrap = holdVar.getData();
		      break;
	      case 1:
		      holdWrap = holdVar.getWeight();
		      break;
	      case 2:
		      holdWrap = holdVar.getLandFract();
		      break;
	      default:
		      System.out.println("ERROR while trying to get group data");
		      holdWrap = null;
		      break;
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
