/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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
package ModelInterface.DMsource;

import java.io.IOException;
import java.io.Writer;

public class ConsoleWriter extends Writer
{
  Console target;
  
  public ConsoleWriter(Console out)
  {
    target = out;
  }
  
  @Override
  public void write(char[] arg0, int arg1, int arg2) throws IOException
  {
    String hold;
    hold = String.valueOf(arg0, arg1, arg2);
    
    target.write(hold);
  }
  
  @Override
  public void write(String str)
  {
    target.write(str);
  }

  @Override
  public void flush() throws IOException
  {
    /*
     * writes whatever is being kept around in here to the console in question
     * ok this doesnt do anything, i just wanted to sound cool
     */
  }

  @Override
  public void close() throws IOException
  {
    /*
     * this closes this particular stream, which doesnt mean much, or anything
     */
  }

}
