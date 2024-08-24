using System;
using System.Collections.Generic;

namespace function_delegate
{

  class Program
  {
    public  delegate  int IntFn(int x);
  
    public static int apply(int x, IntFn f)
    {
      return f(x);
    }
    
    // Function that meet the IntFn requirement,
    // TODO: check how to explicitly state delegate

    public static int inc1(int x)
    {
      return x + 1;
    }

    public static int x2(int x)
    {
      return x * 2;
    }

    static void Main(string[] args)
    {
      var r1 = apply(2, inc1);
      var r2 = apply(2, x2);
      var r3 = apply(2, x => (int) Math.Pow(x,4));
      Console.WriteLine($"{r1},{r2},{r3}");
    }
  }
}