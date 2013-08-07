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
 * \file coordComparePoint.java
 * \ingroup Preprocess
 * \brief Comparator class which sorts Point2D objects by coordinate.
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date: 2005-08-09 12:34:06 -0400 (Tue, 09 Aug 2005) $
 * \version $Revision: 2228 $
 */
package ModelInterface.PPsource;

import java.util.Comparator;
import java.awt.geom.*;

/**
 * Comparator class which sorts Point2D objects by coordinate.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class coordComparePoint implements Comparator 
{
  public int compare(Object o1, Object o2) 
  {
    Point2D.Double d1 = (Point2D.Double)o1;
    Point2D.Double d2 = (Point2D.Double)o2;
    
    //sort on Y coordinate first
    if(d1.y > d2.y)
    {
      return -1;
    }
    else if(d2.y > d1.y)
    {
      return 1;
    } //if Y coord's are equal sort on X
    else if(d1.x > d2.x)
    {
      return 1;
    }
    else if(d2.x > d1.x)
    {
      return -1;
    }
    else
      return 0;
  }
}
