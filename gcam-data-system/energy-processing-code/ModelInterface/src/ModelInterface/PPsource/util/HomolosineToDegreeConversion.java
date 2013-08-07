package ModelInterface.PPsource.util;

import java.awt.geom.Point2D;

/**
 * This class is used to convert from Goode's Homolosine mapping projection
 * coodrinates (x, y) to latitude and longitude in degrees.
 */
public class HomolosineToDegreeConversion {

	/**
	 * Number of regions in the Goode's Homolosine Projection
	 */
	private static int NUM_REGIONS = 12;

	/**
	 * Private instance of this class.
	 */
	private static HomolosineToDegreeConversion thisConverter = new HomolosineToDegreeConversion();

	/**
	 * Center meridians, one for each region.
	 */
	private double[] lon_center;

	/**
	 * False easting, one for each region.
	 */
	private double[] feast;

	/**
	 * Private constructor, this is a singleton class.  Initialize the center meridians, and
	 * false eastings.
	 */
	private HomolosineToDegreeConversion() {
		lon_center = new double[NUM_REGIONS];
		feast = new double[NUM_REGIONS];

		double rad;
		double deg;
		for(int i = 0; i < NUM_REGIONS; i++) {
			// These are the degree locations for the various regions
			switch(i) {
				case 0:
					deg = -100.0;
					break;
				case 1:
					deg = -100.0;
					break;
				case 2:
					deg = 30.0;
					break;
				case 3:
					deg = 30.0;
					break;
				case 4:
					deg = -160.0;
					break;
				case 5:
					deg = -60.0;
					break;
				case 6:
					deg = -160.0;
					break;
				case 7:
					deg = -60.0;
					break;
				case 8:
					deg = 20.0;
					break;
				case 9:
					deg = 140.0;
					break;
				case 10:
					deg = 20.0;
					break;
				case 11:
					deg = 140.0;
					break;
				default:
					deg = 0.0;
					// error? how did it get here?
			}
			rad = Math.toRadians(deg);
			lon_center[i] = rad;
			// false easting should be location(radians) * radius
			// radius is not available and thus this value must be 
			// access through the method getFalseEasting
			feast[i] = rad;
		}
	}

	/**
	 * Get the instance of this class.
	 * return The instance of this class.
	 */
	public static HomolosineToDegreeConversion getInstance() {
		return thisConverter;
	}

	/**
	 * Get the central meridian for a region.
	 * param region The region for which to get the meridian.
	 * return The longitude of the meridian in radians.
	 */
	private double getCentralMeridian(int region) {
		// put in checks to make sure this is a valid region?
		return lon_center[region];
	}

	/**
	 * Get the false easting for a region, given the radius of the earth.
	 * param region The region for which to get the easting.
	 * param radius The radius of the earth.
	 * return The false easting in eastings? .. meters? .. something else?
	 */
	private double getFalseEasting(int region, double radius) {
		// checks on regions and radius?
		return feast[region] * radius;
	}

	/**
	 * Convert map projection (x, y) to latitude and longitude.
	 * param point Input map projection coordinates(x, y)
	 * param radius The radius of the earth used for the projection.
	 * return The corresponding degree coordinates (lat, long)
	 */
	public Point2D.Double convert(Point2D.Double point, double radius, boolean checkInt) {
		// declare to throw exception if invalid point such as if it happens to
		// fall in an interrupted zone?

		double arg;
		double theta;
		int region;
		double lat;
		double lon;
		// should I compute these values here instead of hard coding them?
		if(point.getX() >= radius * 0.710987989993) { // above 40 44' 11.8"
			if(point.getX() <= radius * -0.698131700798) { // left of -40
				region = 0;
			} else {
				region = 2;
			}
		} else if(point.getY() >= 0.0) { // between 0 and 40 44'11.8"
			if(point.getX() <= radius * -0.698131700798) { // left of -40
				region = 1;
			} else {
				region = 3;
			}
		} else if(point.getY() >= radius * -0.710987989993) { // between 0 and -40 44' 11.8"
			if(point.getX() <= radius * -1.74532925199) { // between -180 and -100
				region = 4;
			} else if(point.getX() <= radius * -0.349065850399) { // between -100 and -20	
				region = 5;
			} else if(point.getX() <= radius * 1.3962634016) { // between -20 and 80
				region = 8;
			} else { // between 80 and 180
				region = 9;
			}
		} else { // below -40 44' 11.8"
			if(point.getX() <= radius * -1.74532925199) { // between -180 and -100
				region = 6;
			} else if(point.getX() <= radius * -0.349065850399) { // between -100 and -20
				region = 7;
			} else if(point.getX() <= radius * 1.3962634016) { // between -20 and 80
				region = 10;
			} else { // between 80 and 180
				region = 11;
			}
		}
		point = new Point2D.Double(point.getX() - getFalseEasting(region, radius), point.getY());
		double piOverTwo = Math.PI / 2;
		if(region == 1 || region == 2 || region == 3 || region == 4 || region == 5 ||
				region == 8 || region == 9) {
			lat = point.getY() / radius;
			if(Math.abs(lat) > piOverTwo) {
				// error?
              System.out.println("dont know what this is, throwing null at x: "+point.x+" y: "+point.y);
				return null;
			}
			// maybe give a little more flex the Double.MIN_VALUE
			if(Math.abs(lat - piOverTwo) <= Double.MIN_VALUE) {
				lon = getCentralMeridian(region);
			} else {
				lon = getCentralMeridian(region) + point.getX() / (radius * Math.cos(lat));
				// adjust??
			}
		} else {
			arg = (point.getY() + 0.0528035274542 * radius * Math.signum(point.getY())) / (Math.sqrt(2) * radius);
			if(arg > 1.0) {
				// this is an interrupted value throw exception?
              System.out.println("got interrupted space, throwing null at x: "+point.x+" y: "+point.y);
				return null;
			}
			theta = Math.asin(arg);
			lon = getCentralMeridian(region) + (Math.PI * point.getX()) / (2 * Math.sqrt(2) * radius * Math.cos(theta));
			if(lon < -1* Math.PI) {
				// this is an interrupted value throw exception?
              System.out.println("got interrupted space2, throwing null at x: "+point.x+" y: "+point.y);
				return null;
			}
			arg = (2*theta + Math.sin(2*theta)) / Math.PI;
			if(arg > 1.0) {
				// this is an interrupted value throw exception?
              System.out.println("got interrupted space3, throwing null at x: "+point.x+" y: "+point.y);
				return null;
			}
			lat = Math.asin(arg);
		}
		// becuase of precision problems 180 and -180 may be switched
		if(((point.getX() < 0) && ((Math.PI - lon) < Double.MIN_VALUE)) || ((point.getX() > 0) && ((Math.PI + lon) < Double.MIN_VALUE))) {
			System.out.println("Did flip");
			lon *= -1;
		}
		if(checkInt&&checkInInterrupted(lat, lon, region)) {
			// this is an interrupted value throw exception?
          System.out.println("got interrupted space4, throwing null at x: "+point.x+" y: "+point.y+" lat: "+Math.toDegrees(lat)+" lon: "+Math.toDegrees(lon)+" in region: "+region);
			return null;
		} else {
			// we did it!!
			return new Point2D.Double(Math.toDegrees(lat), Math.toDegrees(lon));
		}
	}

	/**
	 * Check whether the calculated lat / long are in the interrupted space.
	 * param lat The lattitude to check.
	 * param lon The longitude to check.
	 * param region The region this point is supposed to be in.
	 * return True if it is in interrupted space, false otherwise.
	 */
	private boolean checkInInterrupted(double lat, double lon, int region) {
		double pi = Math.PI;
		double ep = Double.MIN_VALUE;
		switch(region) {
			case 0:
				return (lon < -(pi+ep) || lon > -0.698131700798);
			case 1:
				return (lon < -(pi+ep) || lon > -0.698131700798);
			case 2:
				return (lon < -0.698131700798 || lon > (pi+ep));
			case 3:
				return (lon < -0.698131700798 || lon > (pi+ep));
			case 4:
				return (lon < -(pi+ep) || lon > -1.74532925199);
			case 5:
				return (lon < -1.74532925199 || lon > -0.349065850399);
			case 6:
				return (lon < -(pi+ep) || lon > -1.74532925199);
			case 7:
				return (lon < -1.74532925199 || lon > -0.349065850399);
			case 8:
				return (lon < -0.349065850399 || lon > 1.3962634016);
			case 9:
				return (lon < 1.3962634016 || lon > (pi+ep));
			case 10:
				return (lon < -0.349065850399 || lon > 1.3962634016);
			case 11:
				return (lon < 1.3962634016 || lon > (pi+ep));
			default:
				// error?
				return true;
		}
	}
}
