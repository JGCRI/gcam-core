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
package ModelInterface.PPsource;

import java.awt.geom.Point2D;
import java.util.logging.*;

public class CoordConversions
{
  
  public CoordConversions()
  {
   }
  
//*********************************************************
//*************Begin Functions**********************
//*********************************************************

// Y index starts with 0 at LL of -90.  
public static Point2D.Double point2index( Point2D.Double p, double resolution, boolean down )
  {
    //down is wether we should round down or up
    int xIdx, yIdx;//indexes are always ints
    
    //translate from a point in lat/lon to an index

    if(down)
    {
      //Add small value so won't round down due to numerical error. Results will always be integer, so won't change result otherwise.
      xIdx = (int)Math.floor( ( (p.x/resolution) + ( 180/resolution ) + resolution/100 ) );
      yIdx = (int)Math.floor( ( (p.y/resolution) + ( 90/resolution ) + resolution/100 ) );
    } else
    {
      //Subtract small value so won't round down due to numerical error. Results will always be integer, so won't change result otherwise.
      xIdx = (int)Math.ceil( (p.x/resolution) + 180/resolution  -  resolution/100 );
      yIdx = (int)Math.ceil( (p.y/resolution) + 90/resolution -  resolution/100 );
    }
    return new Point2D.Double(xIdx, yIdx);
  }

 public static Point2D.Double index2point( Point2D.Double i, double resolution )
  {
    double x, y;
    
    //translate from index to point
    //no need to know wether we round up or down because index is always an
    //exact translation to lat/lon
    x = ((i.x-(180/resolution))*(resolution));
    y = ((i.y-(90/resolution))*(resolution));
    
    //return this index
    return new Point2D.Double(x, y);
  }

/**
* Returns the area of a block
* Lat should be the central lat of the block 
*
*/
public static double area( double height, double width, double latitude )
  {
	  final double POLAR_CIRCUM = 40008.00;
	  final double EQUAT_CIRCUM = 40076.5;
	  
	  double blockHeightKm = ( POLAR_CIRCUM/( 360/height ) );
	  double circumAtLat = Math.abs( EQUAT_CIRCUM * Math.cos( latitude * (Math.PI/180) ) );
	  double blockWidthKm = ( circumAtLat / ( 360/width ) );
	  return blockHeightKm * blockWidthKm;
  }

}

