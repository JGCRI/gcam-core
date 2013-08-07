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
