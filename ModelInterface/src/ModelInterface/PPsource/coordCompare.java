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
