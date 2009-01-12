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
    //possible given that we know resolution, which we do

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

