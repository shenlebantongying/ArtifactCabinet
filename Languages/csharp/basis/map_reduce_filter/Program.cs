using System;
using System.Collections.Generic;
using System.Linq;


// map_reduce_filter, aka powerful tools without composing
namespace Pallets
{
  class Program
  {

    static void map_reduce_filter()
    {
      
      // Map -> Select
      // Reduce -> Aggregate
      // Filter -> Where
      
      Console.WriteLine(
        Enumerable.Range(1, 10)
        .Select(x => x * 2)
        .Where(x=> x>10)
        .Aggregate(0,(acc,x)=> acc+2*x));

      // Alternative syntax -> LINQ
      int[] r = Enumerable.Range(1, 10).ToArray();

      IEnumerable<int> r_queried =
        from rq in r
        where rq > 3
        select rq;
      
      r_queried.ToList().ForEach(Console.Write);


      string[] fruits = { "apple", "mango", "orange", "passionfruit", "grape" };
      
    }

    static void Main(string[] args)
    {

      map_reduce_filter();
      Console.WriteLine("Hello World!");
    }
  }
}