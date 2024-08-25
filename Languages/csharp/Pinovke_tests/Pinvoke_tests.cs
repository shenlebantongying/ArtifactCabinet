using System.Runtime.InteropServices;

namespace Pinovke_tests
{
    public partial class Program
    {
        [LibraryImport("libminidraw")]
        private static partial IntPtr version();

        [LibraryImport("libminidraw")]
        private static partial void initDrawBoard(int w, int h);

        [LibraryImport("libminidraw")]
        private static partial void setColor(int r, int g, int b);

        [LibraryImport("libminidraw")]
        private static partial void savePng(IntPtr fileName);

        [LibraryImport("libminidraw")]
        private static partial void lineTo(int x, int y);

        [LibraryImport("libminidraw")]
        private static partial void moveTo(int x, int y);

        [LibraryImport("libminidraw")]
        private static partial void stroke();


        public static void Main()
        {
            Console.Out.WriteLine(Marshal.PtrToStringAuto(version()));

            const int w = 800;

            initDrawBoard(w, w);

            setColor(255, 0, 0);

            for (var i = 100; i < w; i += 800 / (8 - 2))
            {
                moveTo(i, 0);
                lineTo(i, w);
            }

            stroke();

            savePng(Marshal.StringToHGlobalAnsi("a.png"));
        }
    }
}