using System.Xml.Linq;

namespace dotnet;

class BaiscSvg
{
    static void Main(string[] args)
    {
        XNamespace ns = "http://www.w3.org/2000/svg";

        var msvg =
            new XElement(
                ns.GetName("svg"), new XAttribute("width", 100), new XAttribute("height", 100),
                new XElement(ns.GetName("circle"), new XAttribute("cx", 50), new XAttribute("cy", "50"),
                    new XAttribute("radius", 50), new XAttribute("fill", "red"))
            );
        Console.WriteLine(msvg);
    }
}
