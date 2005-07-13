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
package source;


public abstract class Wrapper
{
  double[][] data;
  
  //***************************************************************************
  
  
  //***************************************************************************
  
  public abstract Wrapper makeCopy();
  
  public double[][] getData()
  {
    return data;
  }
  
  public void setData(double[][] newData)
  {
    data = newData;
  }
  
  public abstract double getX();
  
  public abstract void setX(double X);
  
  public abstract double getY();
  
  public abstract void setY(double Y);
  
  public abstract double getH();
  
  public abstract void setH(double H);
  
  public abstract double getW();
  
  public abstract void setW(double W);
  
  public abstract double getRes();
  
  public abstract void setRes(double R);
  
  public abstract boolean isData();
  
  public abstract void printStandard();
}
