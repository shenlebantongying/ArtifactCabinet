using System;
using System.Net.Http;

namespace event_subscribe
{
  
  public class CustomEventArgs : EventArgs
  {
    public string Message { get; set; }

    public CustomEventArgs(string message)
    {
      Message = message;
    }
  }

  public class Subscriber
  {
    private readonly string _id;

    public Subscriber(string id, Publisher pub)
    {
      _id = id;
      pub.RaiseCustomEvent += HandleCustomEvent;
    }
    
    public void  HandleCustomEvent(object sender, CustomEventArgs a)
    {
      Console.WriteLine($"{_id}->{a.Message}");
    }
  }

  public class Publisher
  {
    public event EventHandler<CustomEventArgs> RaiseCustomEvent;

    protected virtual void OnRaiseCustomEvent(CustomEventArgs e)
    {
      EventHandler<CustomEventArgs> raiseEvent = RaiseCustomEvent;

      if (raiseEvent != null)
      {
        e.Message += $"-> Time {DateTime.Now}";
        raiseEvent(this, e);
      }
    }
    
    public void DoSomething()
    {
      OnRaiseCustomEvent(new CustomEventArgs("Event Triggered")); 
    }
    
  }

  class Program 
  {
    static void Main(string[] args)
    {

      var pub = new Publisher();
      var sub1 = new Subscriber("sub1", pub);
      var sub2 = new Subscriber("sub2", pub);
      
      pub.DoSomething();
      pub.DoSomething();
      pub.DoSomething();
      pub.DoSomething();
      pub.DoSomething();
      pub.DoSomething();
      
      Console.ReadLine();

    }
  }
}