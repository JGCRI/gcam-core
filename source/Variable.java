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
 * \file Variable.java
 * \ingroup DataManipulation
 * \brief User defined piece of data which can be manipulated.
 *
 *  The base of all variables. Stores data as an array of DataWrappers for efficency of
 * matrix operations. If all region info was aggregated into one matrix there would be
 * large black areas which do not need to be operated on. By storing regions data seperately
 * while working on it runtime is improved.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package source;

/**
 * User defined piece of data which can be manipulated. The base of all variables. 
 * Stores data as an array of DataWrappers for efficency of
 * matrix operations. If all region info was aggregated into one matrix there would be
 * large black areas which do not need to be operated on. By storing regions data seperately
 * while working on it runtime is improved.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public abstract class Variable
{
  String name;	//the name of this variable, how it is accessed
  String comment; //if the user would like additional information printed with the variable
  
  //***************************************************************************
  //************************Constructors***************************************
  //***************************************************************************
  
  //***************************************************************************
  //******************Variable Functions***************************************
  //***************************************************************************
  public Variable getCopy()
  {
    return getCopy(name);
  }
  
  //***************************************************************************
  //******************Abstract Functions***************************************
  //***************************************************************************
  /**
   * Prints only the variable's data component to the screen. This does not print any
   * qualifying information, even the variables name. This function can be used
   * to print in the midst of other set output or user defined output. If a value does
   * not lie in the variables data even though it is in the rectangular bounds, NaN
   * will be printed.
   *
   */
  public abstract void printStandard();
  /**
   * Prints the variables data with qualifying information. This will include at least
   * the name of the variable, but may also include bounding information and so forth.
   * If a value does not lie in the variables data even though it is in the rectangular
   * bounds, NaN will be printed.
   */
  public abstract void printVerbose();
  public abstract Wrapper[] getData();
  public abstract void setData(Wrapper[] d);
  public abstract boolean isReference();
  public abstract boolean isGroup();
  public abstract Variable getCopy(String n);
  public abstract Variable getShape(String n);
}

