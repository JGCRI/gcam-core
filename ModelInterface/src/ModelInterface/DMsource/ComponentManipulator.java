
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
 * \date $Date$
 * \version $Revision$
 */
package ModelInterface.DMsource;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

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
          if((Double.isNaN(holdM1[iY][iX]))||(Double.isNaN(holdM2[iY][iX])))
          {
            holdMR[iY][iX] = Double.NaN;
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
          if((Double.isNaN(holdM1[iY][iX]))||(Double.isNaN(holdM2[iY][iX])))
          {
            holdMR[iY][iX] = Double.NaN;
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
  public static Wrapper[] greaterThan(Wrapper[] R, double limit)
  {
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
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(holdMS[iY][iX] > limit)
          	{
            	holdMR[iY][iX] = holdMS[iY][iX];
          	} else
          	{
            	holdMR[iY][iX] = 0;
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
              holdMR[iY][iX] = 0;
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
  public static Wrapper[] lessThan(Wrapper[] R, double limit)
  {
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
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            if(holdMS[iY][iX] < limit)
          	{
            	holdMR[iY][iX] = holdMS[iY][iX];
          	} else
          	{
            	holdMR[iY][iX] = 0;
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
              holdMR[iY][iX] = 0;
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
            if(holdMM[iY][iX] < 1)
            {
              if(holdMM[iY][iX] < limit)
              {
                holdMR[iY][iX] = (holdMS[iY][iX]);
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
            if(holdMM[iY][iX] < 1)
            {
              if(holdMM[iY][iX] < limit)
              {
                holdMR[iY][iX] = (holdMS[iY][iX]*(1-holdMM[iY][iX]));
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
  public static Wrapper[] sumValues(Wrapper[] R, double[][] weight, double x, double y, double h)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    int wX, wY; //the double indexs for weight
    DataWrapper[] toReturn = new DataWrapper[1];
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        wY = (int)(((iY*R[i].getRes())+((y+h)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            wX = (int)((((iX*R[i].getRes())+R[i].getX())-x)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*weight[wY][wX]);
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
  public static Wrapper[] avgOverRegion(Wrapper[] R, double[][] weight, double x, double y, double h)
  {
    log.log(Level.FINER, "begin function");
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    int count = 0;
    int wY, wX;
    
    holdMR[0][0] = 0;
    
    for(int i = 0; i < R.length; i++)
    {
      holdMS = R[i].data;
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        wY = (int)(((iY*R[i].getRes())+((y+h)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            wX = (int)((((iX*R[i].getRes())+R[i].getRes())-x)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*weight[wY][wX]);
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
    double POLAR_CIRCUM = 40008.00; //these are constand but i dont know how to make constants in java...
    double EQUAT_CIRCUM = 40076.5;
    double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    DataWrapper[] toReturn = new DataWrapper[1];
    
    double circumAtLat; //the circumference of the earth at a specific latitude
    double totalWidth; //width in km of the region
    double totalHeight; //height in km of the region
    double blockWidth; //eidth in km of a block of data
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
  public static Wrapper[] avgOverRegionByArea(Wrapper[] R, double[][] weight, double Rx, double Ry, double Rw, double Rh)
  {
    log.log(Level.FINER, "begin function");
    double POLAR_CIRCUM = 40008.00; //these are constand but i dont know how to make constants in java...
    double EQUAT_CIRCUM = 40076.5;
    double PI = 3.1415926535;
    
    double[][] holdMR = new double[1][1];
    double[][] holdMS;
    double total = 0;
    DataWrapper[] toReturn = new DataWrapper[1];
    int wY, wX;
    
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
      for(int iY = 0; iY < holdMS.length; iY++)
      {
        wY = (int)(((iY*R[i].getRes())+((Ry+Rh)-(R[i].getY()+R[i].getH())))/R[i].getRes());
        circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((R[i].getY()+((holdMS.length-iY)*R[i].getRes()))*(PI/180)));
        totalWidth = (circumAtLat/(360/(R[i].getW())));
        blockWidth = (totalWidth/R[i].data[iY].length);
        proportion = ((blockWidth*blockHeight)/(totalArea));
        
        for(int iX = 0; iX < holdMS[0].length; iX++)
        {
          if(!Double.isNaN(holdMS[iY][iX]))
          {
            wX = (int)((((iX*R[i].getRes())+R[i].getX())-Rx)/R[i].getRes());
            holdMR[0][0] += (holdMS[iY][iX]*proportion*weight[wY][wX]);
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
}
