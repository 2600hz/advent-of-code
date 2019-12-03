using System;
using System.Linq;

namespace CrossedWires
{
    public class MinMax
    {
        public int Min { set; get; }
        public int Max { set; get; }
    }

    public class Dimensions
    {
        public int Width { set; get; }
        public int Height { set; get; }
        public int OffsetX { set; get; }
        public int OffsetY { set; get; }
    }

    class Program
    {
        enum Side
        {
            HORIZONTAL,
            VERTICAL
        }

        static void Main(string[] args)
        {
            string line1 = args[0];
            string line2 = args[1];

            Dimensions d = GetDimensions(line1, line2);
        }

        static MinMax GetLineMinMax(string line, Side side)
        {
            var filter = (side == Side.HORIZONTAL)
                ? (Func<string, bool>)(x => "LR".Contains(x[0]))
                : x => "UD".Contains(x[0]);
            int min = 0;
            int max = 0;
            int pos = 0;

            foreach (var dir in line.Split(',').Where(filter))
            {
                int sign = "LD".Contains(dir[0]) ? -1 : 1;
                int value = int.Parse(dir.Substring(1));
                pos += (sign * value);

                if (pos < min)
                {
                    min = pos;
                }
                if (pos > max)
                {
                    max = pos;
                }
            }

            return new MinMax
            {
                Min = min,
                Max = max
            };
        }

        static Dimensions GetDimensions(string line1, string line2)
        {
            var mm1X = GetLineMinMax(line1, Side.HORIZONTAL);
            var mm1Y = GetLineMinMax(line1, Side.VERTICAL);
            var mm2X = GetLineMinMax(line2, Side.HORIZONTAL);
            var mm2Y = GetLineMinMax(line2, Side.VERTICAL);

            var minX = Math.Min(mm1X.Min, mm2X.Min);
            var minY = Math.Min(mm1Y.Min, mm2Y.Min);
            var maxX = Math.Min(mm1X.Max, mm2X.Max);
            var maxY = Math.Min(mm1Y.Max, mm2Y.Max);

            return new Dimensions
            {
                Width = maxX - minX,
                Height = maxY - minY,
                OffsetX = -minX,
                OffsetY = -minY
            };
        }
    }
}
