
public final class Mandel {
    public static void main(String[] args) {
        System.out.println(Mandel.mandel(-2.3, -2.0, 0.025 / 2., 0.05 / 2., 156., 80.));
    }

    static char printdensity(int d) {
        if (d > 127) {
            return ' ';
        } else if (d > 8) {
            return '.';
        } else if (d > 4) {
            return '+';
        } else {
            return '*';
        }
    }

    static int mandelconverge(double creal, double cimag) {
        var r = creal;
        var i = cimag;
        for (var iter = 0; iter < 255; iter++) {
            if (r*r + i*i > 4.) {
                return iter;
            };
            var next_r = r * r - i * i + creal;
            i = 2. * r * i + cimag;
            r = next_r;
        }
        return 255;
    }

    static String mandel(double xmin, double ymin, double xstep, double ystep, double xsteps, double ysteps) {
        var xmax = xmin + xstep * xsteps;
        var ymax = ymin + ystep * ysteps;
        int ixsteps = (int)(xsteps);
        int iysteps = (int)(ysteps);
        var ret = "";
        for (var iy = 0; iy < iysteps; iy++) {
            var y = (double)iy * (ymax - ymin) * ystep + ymin;
            for (var ix = 0; ix < ixsteps; ix++) {
                var x = (double)ix * (xmax - xmin) * xstep + xmin;
                ret += printdensity(mandelconverge(x,y));
            }
            ret += "\n";
        }
        return ret;
    }
}
