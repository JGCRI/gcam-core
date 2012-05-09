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
 * \file coordCompare.java
 * \ingroup Preprocess
 * \brief Comparator class which sorts DataBlock objects by coordinate.
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
package ModelInterface.PPsource;

import java.util.Comparator;


/**
 * Comparator class which sorts DataBlock objects by coordinate.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class coordCompare implements Comparator 
{
  public int compare(Object o1, Object o2) 
  {
    DataBlock d1 = (DataBlock)o1;
    DataBlock d2 = (DataBlock)o2;
    
    //sort on X coordinate first
    if(d1.x > d2.x)
    {
      return 1;
    }
    else if(d2.x > d1.x)
    {
      return -1;
    } //if X coord's are equal sort on Y
    else if(d1.y > d2.y)
    {
      return 1;
    }
    else if(d2.y > d1.y)
    {
      return -1;
    }
    else
      return 0;
  }
}
