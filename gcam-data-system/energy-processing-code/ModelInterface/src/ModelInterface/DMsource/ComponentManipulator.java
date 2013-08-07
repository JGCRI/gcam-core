
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
 * \file ComponentManipulator.java
 * \ingroup DataManipulation
 * \brief 
 *
 *  Contains manipulation functions based on arrays of the Wrapper class.
 * Abstract because these are just basic math and manipulation functions, there
 * is no need for carry through information, though this may change in the future.
 *
 * \author Vincent Nibali
 * \date $Date: 2009-12-08 16:48:48 -0500 (Tue, 08 Dec 2009) $
 * \version $Revision: 3638 $
 */
package ModelInterface.DMsource;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.security.*;
import org.apache.commons.math.special.*;
import org.apache.commons.math.*;

public final class ComponentManipulator
{
  static Logger log = Logger.getLogger("DataManipulation");
  
//*****************************************************************************
//*****************Component Manipulators**************************************
//*****************************************************************************
  
  public static Wrapper[] addVar(Wrapper[] R1, Wrapper[] R2)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if(Double.isNaN(holdM1[iY][iX]))
          {
            if(Double.isNaN(holdM2[iY][iX]))
            {
              holdMR[iY][iX] = Double.NaN;
            } else
            {
              holdMR[iY][iX] = holdM2[iY][iX];
            }
          } else if(Double.isNaN(holdM2[iY][iX]))
          {
            holdMR[iY][iX] = holdM1[iY][iX];
          } else
          {
            holdMR[iY][iX] = holdM1[iY][iX] + holdM2[iY][iX];
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] addVar(Wrapper[] R, double change)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR, holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            holdMR[iY][iX] = holdMS[iY][iX] + change;
          }
        }
      }
      
      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] subtractVar(Wrapper[] R1, Wrapper[] R2)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if(Double.isNaN(holdM1[iY][iX]))
          {
            holdMR[iY][iX] = Double.NaN;
          } else if(Double.isNaN(holdM2[iY][iX]))
          {
            holdMR[iY][iX] = holdM1[iY][iX];
          } else
          {
            holdMR[iY][iX] = holdM1[iY][iX] - holdM2[iY][iX];
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] multiplyVar(Wrapper[] R1, Wrapper[] R2)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if((Double.isNaN(holdM1[iY][iX]))||(Double.isNaN(holdM2[iY][iX])))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            holdMR[iY][iX] = (holdM1[iY][iX] * holdM2[iY][iX]);
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] multiplyVar(Wrapper[] R, double factor)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR, holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            holdMR[iY][iX] = (holdMS[iY][iX] * factor);
          }
        }
      }
      
      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] divideVar(Wrapper[] R1, Wrapper[] R2)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if((Double.isNaN(holdM1[iY][iX]))||(Double.isNaN(holdM2[iY][iX])))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            holdMR[iY][iX] = (holdM1[iY][iX] / holdM2[iY][iX]);
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] divideVar(Wrapper[] R, double factor)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR, holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            holdMR[iY][iX] = (holdMS[iY][iX] / factor);
          }
        }
      }
      
      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] greaterThan(Wrapper[] R, double limit, boolean snap)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    double fail = Double.NaN;
    if(snap)
    { //failing values will always be set to this, which will be either limit of NaN
      fail = limit;
    }
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(holdMS[iY][iX] > limit)
          	{
              holdMR[iY][iX] = holdMS[iY][iX];
          	} else
          	{
          	  holdMR[iY][iX] = fail;
          	}
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] greaterThanRegion(Wrapper[] R, Wrapper[] M)
  { 
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if((!Double.isNaN(holdMS[iY][iX]))&&(!Double.isNaN(holdMM[iY][iX])))
          {
            if(holdMS[iY][iX] > holdMM[iY][iX])
            {
              holdMR[iY][iX] = holdMS[iY][iX];
            } else
            {
              holdMR[iY][iX] = Double.NaN;
            }
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] lessThan(Wrapper[] R, double limit, boolean snap)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    double fail = Double.NaN;
    if(snap)
    { //failing values will always be set to this, which will be either limit of NaN
      fail = limit;
    }
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(holdMS[iY][iX] < limit)
          	{
            	holdMR[iY][iX] = holdMS[iY][iX];
          	} else
          	{
            	holdMR[iY][iX] = fail;
          	}
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] lessThanOrEqual(Wrapper[] R, double limit, boolean snap)
  {
	  log.log(Level.FINER, "begin function");
	  double[][] holdMR;
	  double[][] holdMS;
	  Wrapper[] toReturn = new Wrapper[R.length];
	  double fail = Double.NaN;
	  if(snap)
	  { //failing values will always be set to this, which will be either limit of NaN
		  fail = limit;
	  }

	  for(int i = 0; i < R.length; i++)
	  {
		  holdMS = R[i].data;
		  holdMR = new double[holdMS.length][holdMS[0].length];
		  for(int iY = 0; iY < holdMR.length; iY++)
		  {
			  for(int iX = 0; iX < holdMR[0].length; iX++)
			  {
				  if(!Double.isNaN(holdMS[iY][iX]))
				  {
					  if(holdMS[iY][iX] <= limit)
					  {
						  holdMR[iY][iX] = holdMS[iY][iX];
					  } else
					  {
						  holdMR[iY][iX] = fail;
					  }
				  } else
				  {
					  holdMR[iY][iX] = Double.NaN;
				  }
			  }
		  }

		  toReturn[i] = R[i].makeCopy();
		  toReturn[i].data = holdMR;
	  }
	  return toReturn;
  }
  public static Wrapper[] lessThanRegion(Wrapper[] R, Wrapper[] M)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if((!Double.isNaN(holdMS[iY][iX]))&&(!Double.isNaN(holdMM[iY][iX])))
          {
            if(holdMS[iY][iX] < holdMM[iY][iX])
            {
              holdMR[iY][iX] = holdMS[iY][iX];
            } else
            {
              holdMR[iY][iX] = Double.NaN;
            }
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] removeRandom(Wrapper[] R, double goal)
  {
    log.log(Level.FINER, "begin function");
    SecureRandom rSeeder = new SecureRandom();
    Random rGen = new Random(rSeeder.nextLong());
    
    double[][] holdMR;
    double[][] holdMS;
    double randNum;
    Wrapper[] toReturn = new Wrapper[R.length];

    for(int i = 0; i<R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY<holdMR.length; iY++)
      {
        for(int iX = 0; iX<holdMR[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            randNum = rGen.nextDouble();
            if(randNum<goal)
            {
              holdMR[iY][iX] = holdMS[iY][iX];
            } else
            {
              holdMR[iY][iX] = Double.NaN;
            }
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }
      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    
    return toReturn;
  }
  public static Wrapper[] removeRandomGuided(Wrapper[] R, double goal)
  {
    log.log(Level.FINER, "begin function");
    SecureRandom rSeeder = new SecureRandom();
    Random rGen = new Random(rSeeder.nextLong());
    
    double[][] holdMR;
    double[][] holdMS;
    double randNum;
    double actual = goal;
    double effective = goal;
    long numIn = 0;
    long total = 0;
    Wrapper[] toReturn = new Wrapper[R.length];

    for(int i = 0; i<R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY<holdMR.length; iY++)
      {
        for(int iX = 0; iX<holdMR[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            randNum = rGen.nextDouble();
            if(randNum < effective)
            {
              holdMR[iY][iX] = holdMS[iY][iX];
              numIn++;
              total++;
            } else
            {
              holdMR[iY][iX] = Double.NaN;
              total++;
            }
            actual = numIn/total;
            effective = (((goal-actual)/2)+goal);
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }
      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    
    return toReturn;
  }
  public static Wrapper[] maskCombineOr(Wrapper[] R1, Wrapper[] R2)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if(Double.isNaN(holdM1[iY][iX]))
          {
            if(Double.isNaN(holdM2[iY][iX]))
            {
              holdMR[iY][iX] = Double.NaN;
            } else
            {
              holdMR[iY][iX] = holdM2[iY][iX];
            }
          } else if(Double.isNaN(holdM2[iY][iX]))
          {
            holdMR[iY][iX] = holdM1[iY][iX];
          } else
          {
            holdMR[iY][iX] = holdM1[iY][iX] + holdM2[iY][iX];
            if(holdMR[iY][iX] > 1)
              holdMR[iY][iX] = 1;
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskCombineAnd(Wrapper[] R1, Wrapper[] R2)
  {
    /*
     * fairly conservative approximation of overlap
     */
    //
    log.log(Level.FINER, "begin function");
    double[][] holdM1, holdM2, holdMR;
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdM1 = R1[i].data;
      holdM2 = R2[i].data;
      holdMR = new double[holdM1.length][holdM1[0].length];
      for(int iY = 0; iY < holdM1.length; iY++)
      {
        for(int iX = 0; iX < holdM1[0].length; iX++)
        {
          if((Double.isNaN(holdM1[iY][iX]))||(Double.isNaN(holdM2[iY][iX])))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            if(holdM1[iY][iX] > 1)
              holdM1[iY][iX] = 1;
            if(holdM2[iY][iX] > 1)
              holdM2[iY][iX] = 1;
            
            holdMR[iY][iX] = ((holdM1[iY][iX] + holdM2[iY][iX])-1);
            if(holdMR[iY][iX] <= 0)
              holdM1[iY][iX] = Double.NaN;
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemain(Wrapper[] R, Wrapper[] M, double limit)
  { //0 is defined as removed, all other values stay
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if((holdMM[iY][iX] != 0)&&(!Double.isNaN(holdMM[iY][iX])))
          {
            if(holdMM[iY][iX] > limit)
            {
              holdMR[iY][iX] = holdMS[iY][iX];
            } else
            {
              holdMR[iY][iX] = Double.NaN;
            }
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemain(Wrapper[] R, double remainVal)
{ //0 is defined as removed, all other values stay
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(holdMS[iY][iX] == remainVal)
          {
              holdMR[iY][iX] = holdMS[iY][iX];
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemainWeight(Wrapper[] R, Wrapper[] M, double limit)
  { //0 is defined as removed, all other values stay
    //reamaining values weighted based on mask value
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if((holdMM[iY][iX] != 0)&&(!Double.isNaN(holdMM[iY][iX])))
          {
            if(holdMM[iY][iX] > limit)
            {
              holdMR[iY][iX] = (holdMS[iY][iX]*holdMM[iY][iX]);
            } else
            {
              holdMR[iY][iX] = Double.NaN;
            }
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemainWeight(Wrapper[] R, double remainVal)
{ //0 is defined as removed, all other values stay
    //reamaining values weighted based on mask value
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(holdMS[iY][iX] == remainVal)
          {
              holdMR[iY][iX] = (holdMS[iY][iX]*remainVal);
          } else
          {
            holdMR[iY][iX] = Double.NaN;
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemove(Wrapper[] R, Wrapper[] M, double limit)
  { //0 is defined as remains, all other values are removed
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(Double.isNaN(holdMM[iY][iX]))
          {
            holdMR[iY][iX] = (holdMS[iY][iX]);
          } else
          {
            if(holdMM[iY][iX] > limit)
            {
              holdMR[iY][iX] = Double.NaN;
            } else
            {
              holdMR[iY][iX] = (holdMS[iY][iX]);
            }
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemove(Wrapper[] R, double removeVal)
{ //0 is defined as remains, all other values are removed
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(holdMS[iY][iX] == removeVal)
          {
              holdMR[iY][iX] = Double.NaN;
          } else {
              holdMR[iY][iX] = (holdMS[iY][iX]);
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemoveWeight(Wrapper[] R, Wrapper[] M, double limit)
  { //0 is defined as remains, all other values are removed
    //remaining values weighted based on mask value
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
          if(Double.isNaN(holdMM[iY][iX]))
          {
            holdMR[iY][iX] = (holdMS[iY][iX]);
          } else
          {
            if(holdMM[iY][iX] > limit)
            {
              holdMR[iY][iX] = Double.NaN;
            } else
            {
              holdMR[iY][iX] = (holdMS[iY][iX]*(1-holdMM[iY][iX]));
            }
          }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] maskRemoveWeight(Wrapper[] R, double removeVal)
{ //0 is defined as remains, all other values are removed
    //remaining values weighted based on mask value
    log.log(Level.FINER, "begin function");
    double[][] holdMR;
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[R.length];
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMR.length; iY++)
      {
        for(int iX = 0; iX < holdMR[0].length; iX++)
        {
            if(holdMS[iY][iX] == removeVal)
            {
              holdMR[iY][iX] = Double.NaN;
            } else
            {
              holdMR[iY][iX] = (holdMS[iY][iX]*(removeVal));
            }
        }
      }

      toReturn[i] = R[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] countGreaterThan(Wrapper[] R, double limit)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if((holdMS[iY][iX] > limit)&&(!Double.isNaN(holdMS[iY][iX])))
          {
            holdMR[0][0]++;
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] countGreaterThanRegion(Wrapper[] R, Wrapper[] M)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if((holdMS[iY][iX] > holdMM[iY][iX])&&(!Double.isNaN(holdMS[iY][iX])))
          {
            holdMR[0][0]++;
          }
        }
    }
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] countLessThan(Wrapper[] R, double limit)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if((holdMS[iY][iX] < limit)&&(!Double.isNaN(holdMS[iY][iX])))
          {
            holdMR[0][0]++;
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] countLessThanRegion(Wrapper[] R, Wrapper[] M)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMM; //matrix mask
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMM = M[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if((holdMS[iY][iX] < holdMM[iY][iX])&&(!Double.isNaN(holdMS[iY][iX])))
          {
            holdMR[0][0]++;
          }
        }
    }
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] countElements(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0]++;
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] sumValues(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0] += holdMS[iY][iX];
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] sumValues(Wrapper[] R, Wrapper[] weight, Wrapper[] landFract, double x, double y, double h)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMW;
    double[][] holdMLF;
    //int wX, wY; //the double indexs for weight
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMW = weight[i].data;
      holdMLF = landFract[i].data;
      double sumLandFract = 0.0;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        //wY = (int)(((iY*R[i].getRes())+((y+h)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            //wX = (int)((((iX*R[i].getRes())+R[i].getX())-x)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*holdMW[iY][iX]*holdMLF[iY][iX]);
	    sumLandFract += holdMLF[iY][iX];
          }
        }
      }
      //holdMR[0][0] /= sumLandFract;
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] sumArea(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5; //km
    final double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double cellSize;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //width in km of a block of data
    double blockHeight; //height in km of a block of data
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(iY*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/R[i].getW()));
        blockWidth = (totalWidth/R[i].data[iY].length);
        cellSize = (blockWidth*blockHeight);
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0] += (holdMS[iY][iX]*cellSize);
          }
        }
      }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] sumArea(Wrapper[] R, Wrapper[] weight, Wrapper[] landFract, double x, double y, double h)
  {
    log.log(Level.FINER, "begin function");
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5;
    final double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMW;
    double[][] holdMLF;
    //int wX, wY; //the double indexs for weight
    double cellSize;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //width in km of a block of data
    double blockHeight; //height in km of a block of data
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      holdMW = weight[i].data;
      holdMLF = landFract[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        //wY = (int)(((iY*R[i].getRes())+((y+h)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(R[i].getRes()/2)+((holdMS.length-1-iY)*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/R[i].getW()));
        blockWidth = (totalWidth/R[i].data[iY].length);
        cellSize = (blockWidth*blockHeight);
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            //wX = (int)((((iX*R[i].getRes())+R[i].getX())-x)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*cellSize*holdMW[iY][iX]*holdMLF[iY][iX]);
          }
        }
      }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] sumRegionArea(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5;
    final double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double cellSize;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //width in km of a block of data
    double blockHeight; //height in km of a block of data
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(iY*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/R[i].getW()));
        blockWidth = (totalWidth/R[i].data[iY].length);
        cellSize = (blockWidth*blockHeight);
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0] += (cellSize);
          }
        }
      }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] largestValue(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = Double.NaN;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(Double.isNaN(holdMR[0][0]))
            {
              holdMR[0][0] = holdMS[iY][iX];
            } else if((holdMS[iY][iX] > holdMR[0][0]))
            {
              holdMR[0][0] = holdMS[iY][iX];
            }
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] smallestValue(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = Double.NaN;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(Double.isNaN(holdMR[0][0]))
            {
              holdMR[0][0] = holdMS[iY][iX];
            } else
            {
              if(holdMS[iY][iX] < holdMR[0][0])
              {
                holdMR[0][0] = holdMS[iY][iX];
              }
            }
          }
        }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] avgOverRegion(Wrapper[] R)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    int count = 0;
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0] += holdMS[iY][iX];
            count++;
          }
        }
    }
    holdMR[0][0] = (holdMR[0][0]/count);
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] avgOverRegion(Wrapper[] R, Wrapper[] weight, double x, double y, double h)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMW;
    DataWrapper[] toReturn = new DataWrapper[1];
    int count = 0;
    //int wY, wX;
    //used to use double[][] for weight which is why that absurd math is below
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      holdMW = weight[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        //wY = (int)(((iY*R[i].getRes())+((y+h)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            //wX = (int)((((iX*R[i].getRes())+R[i].getRes())-x)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*holdMW[iY][iX]);
            count++;
          }
        }
      }
    }
    holdMR[0][0] = (holdMR[0][0]/count);
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] avgVariables(Wrapper[][] data)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMS;
    Wrapper[] toReturn = new Wrapper[data[0].length];
    
    for(int i = 0; i < data.length; i++)
    {
      toReturn[i] = data[0][i].makeCopy();
      toReturn[i].data = new double[data[0][i].data.length][data[0][i].data[0].length];
    }
    
    for(int i = 0; i < data.length; i++)
    { //for each variable
      for(int k = 0; k < data[i].length; k++)
      { //for each wrapper
        holdMS = data[i][k].data;
        for(int iY = 0; iY<holdMS.length; iY++)
        {
          for(int iX = 0; iX<holdMS[0].length; iX++)
          {
            if(!Double.isNaN(holdMS[iY][iX]))
            {
              toReturn[k].data[iY][iX] += holdMS[iY][iX];
            }
          }
        }
      }
    }
    
    for(int i = 0; i < data.length; i++)
    { //for each wrapper
        for(int iY = 0; iY<toReturn[0].data.length; iY++)
        {
          for(int iX = 0; iX<toReturn[0].data[0].length; iX++)
          {
            toReturn[i].data[iY][iX] = (toReturn[i].data[iY][iX]/data.length);
          }
        }
    }
    
    return toReturn;
  }
  public static Wrapper[] avgOverRegionByArea(Wrapper[] R, double Rx, double Ry, double Rw, double Rh)
  { //for use by avg variables because weight has already been factored in
    log.log(Level.FINER, "begin function");
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5;
    final double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //width in km of a block of data
    double blockHeight; //height in km of a block of data
    double totalArea; //the area of the ENTIRE region
    double proportion; //the proportion of the whole region a block of data is
    
    //finding the area of the master region, for getting proportions
    circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((Ry)*(PI/180)));
    double lowWidth = (circumAtLat/(360/(Rw)));
    circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((Ry+Rh)*(PI/180)));
    double highWidth = (circumAtLat/(360/(Rw)));
    totalHeight = (POLAR_CIRCUM/(360/(Rh)));
    totalArea = ((highWidth+lowWidth)/2)*totalHeight;
    //done getting that!
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(iY*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/R[i].getW()));
        blockWidth = (totalWidth/R[i].data[iY].length);
        proportion = ((blockWidth*blockHeight)/(totalArea));
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[0][0] += (holdMS[iY][iX]*proportion);
          }
        }
      }
    }
    
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] avgOverRegionByArea(Wrapper[] R, Wrapper[] weight, double Rx, double Ry, double Rw, double Rh)
  {
    log.log(Level.FINER, "begin function");
    double POLAR_CIRCUM = 40008.00; //these are constand but i dont know how to make constants in java...
    double EQUAT_CIRCUM = 40076.5;
    double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double[][] holdMW;
    double total = 0;
    DataWrapper[] toReturn = new DataWrapper[1];
    //int wY, wX;
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //eidth in km of a block of data
    double blockHeight; //height in km of a block of data
    double totalArea; //the area of the ENTIRE region (done as a trapazoid to acount for diff widths)
    double proportion; //the proportion of the whole region a block of data is
    
    //finding the area of the master region, for getting proportions
    circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((Ry)*(PI/180)));
    double lowWidth = (circumAtLat/(360/(Rw)));
    circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((Ry+Rh)*(PI/180)));
    double highWidth = (circumAtLat/(360/(Rw)));
    totalHeight = (POLAR_CIRCUM/(360/(Rh)));
    totalArea = ((highWidth+lowWidth)/2)*totalHeight;
    //done getting that!
    
    holdMR[0][0] = 0;
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/(R[i].getH())));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      holdMW = weight[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        //wY = (int)(((iY*R[i].getRes())+((Ry+Rh)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+((holdMS.length-iY)*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/(R[i].getW())));
        blockWidth = (totalWidth/R[i].data[iY].length);
        proportion = ((blockWidth*blockHeight)/(totalArea));
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            //wX = (int)((((iX*R[i].getRes())+R[i].getX())-Rx)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*proportion*holdMW[iY][iX]);
          }
        }
      }
    }
    
    total /= R.length;
    toReturn[0] = new DataWrapper();
    toReturn[0].data = holdMR;
    return toReturn;
  }
  public static Wrapper[] weightValues(Wrapper[] R1, Wrapper[] R2, double minVal, double maxVal, double minWeight, double maxWeight)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMS, holdMW, holdMR;
    double toTest, p1, p2;
    double thisWeight;
    double weightDiff = (maxWeight-minWeight);
    double valDiff = (maxVal-minVal);
    Wrapper[] toReturn = new Wrapper[R1.length];
    
    for(int i = 0; i < R1.length; i++)
    {
      holdMS = R1[i].data;
      holdMW = R2[i].data;
      holdMR = new double[holdMS.length][holdMS[0].length];
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(Double.isNaN(holdMS[iY][iX]))
          {
            holdMR[iY][iX] = Double.NaN;
          } else
          {
            toTest = holdMW[iY][iX];
            if(toTest < minVal)
            {
              toTest = minVal;
            }
            if(toTest > maxVal)
            {
              toTest = maxVal;
            }
            toTest -= minVal;
            p1 = (toTest/valDiff);
            p2 = (p1*weightDiff);
            thisWeight = (p2+minWeight);
            
            holdMR[iY][iX] = (holdMS[iY][iX] * thisWeight);
          }
        }
      }
      
      toReturn[i] = R1[i].makeCopy();
      toReturn[i].data = holdMR;
    }
    return toReturn;
  }
  public static Wrapper[] freqAnalysis(Wrapper[] R, Wrapper[] weight, Wrapper[] landFract, int bnum, boolean avg)
  {
    log.log(Level.FINER, "begin function");
    /*
     * for each cell, get value in split, decide which bucket that goes into
     * find out area of the cell, for coverage multiply by value in R, otherwise
     * just add area to that bucket, return array of buckets
     */
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5;
    final double PI = 3.1415926535;
    
    double max = largestValue(R)[0].data[0][0];
    double min = smallestValue(R)[0].data[0][0];
    double factor = ((max*1.0001)-min)/bnum; //makes sure max value goes in last bucket
    int store; //the index this value will be added to
    double area; //the area of the current cell we are in in km^2
    double[][] holdMS;
    double[][] holdW;
    double[][] holdLF;
    DataWrapper[] toReturn = new DataWrapper[bnum];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //eidth in km of a block of data
    double blockHeight; //height in km of a block of data
    
    for(int i = 0; i < bnum; i++)
    {
      toReturn[i] = new DataWrapper();
      toReturn[i].data = new double[1][3];
      toReturn[i].data[0][0] = ((factor*i)+min);
      toReturn[i].data[0][1] = 0;
      toReturn[i].data[0][2] = ((factor*(i+1))+min);
    }
    toReturn[bnum-1].data[0][2] = max;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      holdW = weight[i].data;
      holdLF = landFract[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(R[i].getRes()/2)+((holdMS.length-1-iY)*R[i].getRes()))*(PI/180)));
            //circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+((iY)*R[i].getRes()))*(PI/180)));
            totalWidth = (circumAtLat/(360/(R[i].getW())));
            blockWidth = (totalWidth/R[i].data[iY].length);
            area = blockWidth*blockHeight;
            
            store = (int)Math.floor((holdMS[iY][iX]-min)/factor);
            
            //toReturn[store].data[0][1] += (area);
        // If not averaged, then simply adds vars, otherwise returns land area (would need to adjust data var by landfract if want cell area)
	    if(!avg) {
		    toReturn[store].data[0][1] += holdMS[iY][iX];
	    } else {
		    toReturn[store].data[0][1] += (area*holdMS[iY][iX]*holdW[iY][iX]*holdLF[iY][iX]);
	    }
          }
        }
    }
    
    return toReturn;
  }
  public static Wrapper[] freqAnalysis(Wrapper[] R, Wrapper[] split, Wrapper[] weight, Wrapper[] landFract, int bnum, boolean avg)
  {
    log.log(Level.FINER, "begin function");
    /*
     * for each cell, get value in split, decide which bucket that goes into
     * find out area of the cell, for coverage multiply by value in R, otherwise
     * just add area to that bucket, return array of buckets
     */
    final double POLAR_CIRCUM = 40008.00;
    final double EQUAT_CIRCUM = 40076.5;
    final double PI = 3.1415926535;
    
    double max = largestValue(split)[0].data[0][0];
    double min = smallestValue(split)[0].data[0][0];
    double factor = ((max*1.0001)-min)/bnum; //makes sure max value goes in last bucket
    int store; //the index this value will be added to
    double area; //the area of the current cell we are in in km^2
    double[][] holdMS, holdMM;
    double[][] holdW;
    double[][] holdLF;
    DataWrapper[] toReturn = new DataWrapper[bnum];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //eidth in km of a block of data
    double blockHeight; //height in km of a block of data
    
    for(int i = 0; i < bnum; i++)
    {
      toReturn[i] = new DataWrapper();
      toReturn[i].data = new double[1][3];
      toReturn[i].data[0][0] = ((factor*i)+min);
      toReturn[i].data[0][1] = 0;
      toReturn[i].data[0][2] = ((factor*(i+1))+min);
    }
    toReturn[bnum-1].data[0][2] = max;
    
    for(int i = 0; i < R.length; i++)
    {
      totalHeight = (POLAR_CIRCUM/(360/R[i].getH()));
      blockHeight = (totalHeight/R[i].data.length);
      
      holdMS = R[i].data;
      holdMM = split[i].data;
      holdW = weight[i].data;
      holdLF = landFract[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
		if(holdMS[iY][iX] != holdMM[iY][iX] &&
				!(Double.isNaN(holdMS[iY][iX]) && Double.isNaN(holdMM[iY][iX]))) {
			System.out.println("Not same: "+i+" -- "+iX+", "+iY);
			System.out.println("Data: "+holdMS[iY][iX]+" split: "+holdMM[iY][iX]);
			System.out.println("Data NaN: "+Double.isNaN(holdMS[iY][iX])+" split NaN: "+Double.isNaN(holdMM[iY][iX]));
		}
          if(!Double.isNaN(holdMS[iY][iX]) /*&& !Double.isNaN(holdMM[iY][iX])*/)
          {
            circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+(R[i].getRes()/2)+((holdMS.length-1-iY)*R[i].getRes()))*(PI/180)));
            totalWidth = (circumAtLat/(360/(R[i].getW())));
            blockWidth = (totalWidth/R[i].data[iY].length);
            area = blockWidth*blockHeight;
            
            store = (int)Math.floor((holdMM[iY][iX]-min)/factor);
            
       // If not averaged, then simply adds vars, otherwise returns land area (would need to adjust data var by landfract if want cell area)
	    if(!avg) {
		    toReturn[store].data[0][1] += holdMS[iY][iX];
	    } else {
		    toReturn[store].data[0][1] += (area*holdMS[iY][iX]*holdW[iY][iX]*holdLF[iY][iX]);
	    }
          }
        }
    }
    
    return toReturn;
  }
  public static Wrapper[] windPower(double height, double diam, double turbine, double avail, double loss)
  {
    DataWrapper[] toReturn = new DataWrapper[1];
    toReturn[0] = new DataWrapper();
    toReturn[0].data = new double[1][1];
    
    double RatedWS, powerAtHeight, multipleOfClassRated, multipleOfClassCutout;
    double energyCaptured, captureCoeff, maxAnnual, idealized;
    double CutoutWS = 30;
    
    try
    {
      RatedWS = Math.pow(((turbine*8000*27)/(1.225*Math.PI*Math.pow(diam, 2)*16)), (1/3));
      powerAtHeight = Math.pow(((5.8)*(height/10)), (1/7));
      multipleOfClassRated = (RatedWS/powerAtHeight);
      multipleOfClassCutout = (CutoutWS/powerAtHeight);
      energyCaptured = (Erf.erf(0.5*Math.sqrt(Math.PI)*multipleOfClassRated) - (2/3)*multipleOfClassRated*((Math.PI/4)*Math.pow(multipleOfClassRated, 2)+3/2)*Math.pow((Math.E) ,(-0.25*(Math.PI*Math.pow(multipleOfClassRated, 2)))));
      captureCoeff = (energyCaptured+(((Math.pow((multipleOfClassRated), 3))/((3/4)*Math.sqrt(Math.PI)))*(Math.pow((Math.E), (-(Math.pow((multipleOfClassRated), 2))))-Math.pow((Math.E), (-(Math.pow((multipleOfClassCutout), 2)))))));
      maxAnnual = (((((1.225*(Math.pow(((2/3)*(diam)), 2))*(Math.pow(powerAtHeight, 3)))/1000)*8760)/1000)/1000);
      idealized = (maxAnnual*((avail/100)*(1-(loss/100)))*captureCoeff);
      
      toReturn[0].data[0][0] = idealized;
      return toReturn;
    } catch (MathException e)
    {
      log.log(Level.SEVERE, "math exception while calculating wind power generation");
    }
    
    return null;
  }
  
  
//*****************************************************************************
// just needed somewhere to put this... almost definately will never be used
  /**
   * Reads a whitespace delimited word from the provided {@link BufferedReader}.
   * This is a convenience function so that input does not have to be gathered
   * character by characer.
   * @param input BufferedReader from which to gather a 'word' of input.
   * @return First word residing in the passed reader.
   */
  public static String readWord(BufferedReader input)
  {
    //reads an entire word from an input stream rather than just a character
    //words delimited by any whitespace 'space, new line, tab'
    String build = new String();
    int read;
    char hold;
    try {
      while(((read = input.read()) != -1)&&((hold = (char)read) != ' ')&&(hold != '\n')&&(hold != '\t'))
      {
        build = build.concat(String.valueOf(hold));
      }
    } catch (IOException ex)
    {
      log.log(Level.SEVERE, "IOException!!!");
    }

    if(build.length() > 0)
      return build.trim();
    else
      return null;
  }

  /**
   * This method adjusts data by multiplying or divinding by its weights.  Such a
   * command could be use to switch variables from averaged to additive or the other
   * way around or fix double weighting problems. Warning, there could be numerically 
   * unstable floating point numbers in which case values would not be as expected.
   * @param R The wrappers for the data values which should be unweighted.
   * @param W The wrappers for the weight values for the data passed in.
   * @param doWeight If true multiplies by weight, false will divide by weights. 
   * @return The wrappers for the data values multiplied/divided by their weights.
   */
  public static Wrapper[] adjustWeights(Wrapper[] R, Wrapper[] W, boolean doWeight) {
	  log.log(Level.FINER, "begin function");
	  double[][] holdMR, holdMS, holdMW;
	  Wrapper[] toReturn = new Wrapper[R.length];

	  for(int i = 0; i < R.length; i++) {
		  holdMS = R[i].data;
		  holdMW = W[i].data;
		  holdMR = new double[holdMS.length][holdMS[0].length];
		  for(int iY = 0; iY < holdMR.length; iY++) {
			  for(int iX = 0; iX < holdMR[0].length; iX++) {
				  if(Double.isNaN(holdMS[iY][iX])) {
					  holdMR[iY][iX] = Double.NaN;
				  } else {
					  if(doWeight) {
						  holdMR[iY][iX] = holdMS[iY][iX] * holdMW[iY][iX];
					  } else {
						  holdMR[iY][iX] = holdMS[iY][iX] / holdMW[iY][iX];
					  }
				  }
			  }
		  }

		  toReturn[i] = R[i].makeCopy();
		  toReturn[i].data = holdMR;
	  }
	  return toReturn;
  }

  /**
   * This sets a value into every cell of the passed in data.  Note that a copy
   * of the data is not made.
   * @param data The wrapper to set the value into.
   * @param value The value to set.
   */
  public static void setValue(Wrapper[] data, double value) {
    // set the value into the data arrays
    for(int regionWrapperIndex = 0; regionWrapperIndex < data.length; ++regionWrapperIndex) {
	    double[][] currDataArr = data[regionWrapperIndex].getData();
	    for(int row = 0; row < currDataArr.length; ++row) {
		    for(int col = 0; col < currDataArr[0].length; ++col) {
			    currDataArr[row][col] = value;
		    }
	    }
    }
  }
  /**
   * Calculate minimum distance from distanceFrom to targetData.  This algorithm take a brute force approach
   * to find this value by calculating the distance between each non-NaN value (and where values in distanceFrom
   * are greater than the minDataValue threshold) and keeping track of the min.  This command utilizes the
   * DistanceStruct to help organize and keep track of results as well as calculute distance.
   * @param distanceFrom Var to calc distance from such as land.
   * @param distanceFromWeight The weights for distanceFrom since for ties in distance we want
   * 			       to use the region ID of the region with the greater weight.
   * @param targetData Var to calc distance to such as offshore wind.
   * @param distanceOut The var which min distance results will be stored in.
   * @param regionMaskOut The var in which closest region's region ID will be stored.
   * @param regionLevel The region level to use when getting a region ID.
   * @param minDataValue The minimum data value in distanceFrom to consider.
   */
  public static void distanceToData(Wrapper[] distanceFrom, Wrapper[] distanceFromWeight, Wrapper[] targetData, Wrapper[] distanceOut, Wrapper[] regionMaskOut, int regionLevel, double minDataValue) {
	  List<DistanceStruct> distancesToFind = new ArrayList<DistanceStruct>();

	  // initialize the output var and set up the list of grid cells which need to get calculated
	  for(int rIndex = 0; rIndex < targetData.length; ++rIndex) {
		  // all latitude and longitudes are in radians
		  double currLat = targetData[rIndex].getY() * (Math.PI / 180);
		  double currLon = targetData[rIndex].getX() * (Math.PI / 180);
		  double res = targetData[rIndex].getRes() * (Math.PI / 180);
		  double[][] currDataArr = targetData[rIndex].getData();
		  double[][] currDistanceArr = new double[currDataArr.length][currDataArr[0].length];
		  double[][] currRegionArr = new double[currDataArr.length][currDataArr[0].length];
		  distanceOut[rIndex].setData(currDistanceArr);
		  regionMaskOut[rIndex].setData(currRegionArr);

		  // go through the data arr backwards since targetData.getY is of the lower corner
		  // and row 0 is the upper corner
		  for(int row = currDataArr.length-1; row >= 0; --row) {
			  currLon = targetData[rIndex].getX() * (Math.PI / 180);
			  for(int col = 0; col < currDataArr[0].length; ++col) {
				  if(Double.isNaN(currDataArr[row][col])) {
					  // if the target data is NaN at this grid cell we won't
					  // consider it
					  currDistanceArr[row][col] = Double.NaN;
					  currRegionArr[row][col] = Double.NaN;
				  } else {
					  // we must find the min distance to this grid cell
					  // start with a min of MAX_VALUE so that the next
					  // distance comparison will be less than it
					  currDistanceArr[row][col] = Double.MAX_VALUE;
					  // the DistanceStruct represents a single cell in
					  // targetData/distanceOut/regionMaskOut all of which
					  // are of the same shape
					  distancesToFind.add(new DistanceStruct(currLat, currLon, distanceOut[rIndex],
								  regionMaskOut[rIndex], row, col));
				  }
				  currLon += res;
			  }
			  currLat += res;
		  }
	  }

	  // process each cell for which we are calculating distance from
	  for(int rIndex = 0; rIndex < distanceFrom.length; ++rIndex) {
		  // all latitude and longitudes are in radians
		  double currLat = distanceFrom[rIndex].getY() * (Math.PI / 180);
		  double currLon = distanceFrom[rIndex].getX() * (Math.PI / 180);
		  double res = distanceFrom[rIndex].getRes() * (Math.PI / 180);
		  double[][] currDataArr = distanceFrom[rIndex].getData();
		  double[][] currWeightArr = distanceFromWeight[rIndex].getData();
		  double currRegionID = distanceFrom[rIndex].getInternalRegionID(regionLevel);

		  // go through the data arr backwards since distanceFrom.getY is of the lower corner
		  // and row 0 is the upper corner
		  for(int row = currDataArr.length-1; row >= 0; --row) {
			  currLon = distanceFrom[rIndex].getX() * (Math.PI / 180);
			  for(int col = 0; col < currDataArr[0].length; ++col) {
				  // only consider cells which are non-NaN and the value is above the
				  // min value threshold
				  if(!Double.isNaN(currDataArr[row][col]) && currDataArr[row][col] >= minDataValue) {
					  double currWeight = currWeightArr[row][col];
					  // brute force search through all valid distance to cells
					  for(Iterator<DistanceStruct> it = distancesToFind.iterator(); it.hasNext(); ) {
						  DistanceStruct currStruct = it.next();
						  double distance = currStruct.getDistanceTo(currLat, currLon);
						  // set a new min distance if the curr distance is less the current min
						  // or there is a tie in distance but the current region's cell weight
						  // is greater than the curr region's cell weight
						  if(distance < currStruct.getCurrMinDistance() || (distance == currStruct.getCurrMinDistance() && currWeight > currStruct.getCurrRegionWeight())) {
							  currStruct.setDistance(distance, currRegionID, currWeight);
						  }
					  }
				  }
				  currLon += res;
			  }
			  currLat += res;
		  }
	  }
  }
  /**
   * Create a mask out of distanceFrom and extend it to include grid cells within the given distance threshold.
   * This algorithm does a brute force search from valid values in distanceFrom(non-NaN and values greater than
   * minDataValue) to ANY cell in targetData.  Note that this command is very time intensive since there are no
   * constraints in targetData even though chances are most cells would not be contained in the mask.
   * @param distanceFrom The var to start the a distance search from.
   * @param targetData The var to calc distances to, should the distance be less than or equal to maxDistance
   * 		       distanceOut will be set with a value of 1.
   * @param distanceOut The output mask with a value of 1 for any cell a with a distance less than or equal to
   * 			maxDistance to distanceFrom
   * @param maxDistance A max distance threshold in km.
   * @param minDataValue A minimum value threshold of data in distanceFrom to consider.
   */
  public static void maskDistanceFrom(Wrapper[] distanceFrom, Wrapper[] targetData, Wrapper[] distanceOut, double maxDistance, double minDataValue) {
	  // use a linked list since all we need is sequential access and will be preforming random
	  // deletes which should suits a linked list better than an array based list
	  List<DistanceStruct> distancesToFind = new LinkedList<DistanceStruct>();

	  // initialize the output var and set up the list of grid cells which need to get calculated
	  for(int rIndex = 0; rIndex < targetData.length; ++rIndex) {
		  // all latitude and longitudes are in radians
		  double currLat = targetData[rIndex].getY() * (Math.PI / 180);
		  double currLon = targetData[rIndex].getX() * (Math.PI / 180);
		  double res = targetData[rIndex].getRes() * (Math.PI / 180);
		  double[][] currDataArr = targetData[rIndex].getData();
		  double[][] currDistanceArr = new double[currDataArr.length][currDataArr[0].length];
		  distanceOut[rIndex].setData(currDistanceArr);

		  // go through the data arr backwards since targetData.getY is of the lower corner
		  // and row 0 is the upper corner
		  for(int row = currDataArr.length-1; row >= 0; --row) {
			  currLon = targetData[rIndex].getX() * (Math.PI / 180);
			  for(int col = 0; col < currDataArr[0].length; ++col) {
				  // initialize the mask with all NaNs and ALL cells will be
				  // considered for inclusion into the mask
				  currDistanceArr[row][col] = Double.NaN;
				  // note that we are not concerned about region masks so just pass null
				  // the DistanceStruct will know to ignore it
				  distancesToFind.add(new DistanceStruct(currLat, currLon, distanceOut[rIndex],
							  null, row, col));
				  currLon += res;
			  }
			  currLat += res;
		  }
	  }

	  // process each cell for which we are calculating distance from
	  for(int rIndex = 0; rIndex < distanceFrom.length; ++rIndex) {
		  // all latitude and longitudes are in radians
		  double currLat = distanceFrom[rIndex].getY() * (Math.PI / 180);
		  double currLon = distanceFrom[rIndex].getX() * (Math.PI / 180);
		  double res = distanceFrom[rIndex].getRes() * (Math.PI / 180);
		  double[][] currDataArr = distanceFrom[rIndex].getData();

		  // go through the data arr backwards since distanceFrom.getY is of the lower corner
		  // and row 0 is the upper corner
		  for(int row = currDataArr.length-1; row >= 0; --row) {
			  currLon = distanceFrom[rIndex].getX() * (Math.PI / 180);
			  for(int col = 0; col < currDataArr[0].length; ++col) {
				  // only consider cells which are non-NaN and the value is above the
				  // min value threshold
				  if(!Double.isNaN(currDataArr[row][col]) && currDataArr[row][col] >= minDataValue) {
					  // brute force search through all valid distance to cells
					  for(Iterator<DistanceStruct> it = distancesToFind.iterator(); it.hasNext(); ) {
						  DistanceStruct currStruct = it.next();
						  double distance = currStruct.getDistanceTo(currLat, currLon);
						  // if the distance is less than or equal to the maxDistance
						  // threshold then set the mask to a value of 1
						  if(distance <= maxDistance) {
							  // we don't need to worry about region IDs so just pass
							  // anything
							  currStruct.setDistance(1, 0, 0);
							  // since this value has already been set we won't need to
							  // consider it again so remove it from the cells to search
							  it.remove();
						  }
					  }
				  }
				  currLon += res;
			  }
			  currLat += res;
		  }
	  }
  }
}
