function printdensity(d) {
  if (d > 127)
    return " ";
  else if (d > 8)
    return ".";
  else if (d > 4)
    return "+";
  else
    return "*";
}

function mandelconverge(creal, cimag) {
    let r = creal;
    let i = cimag;
    for (it in [...Array(255)]) {
        if (r*r + i*i > 4.)
            return it;
        let next_r = r * r - i * i + creal
        i = 2. * r * i + cimag
        r = next_r
    }
    return 255;
}

function mandel(xmin, ymin, xstep, ystep, xsteps, ysteps) {
    let xmax = xmin + xstep * xsteps;
    let ymax = ymin + ystep * ysteps;
    let ixsteps = xsteps;
    let iysteps = ysteps;
    let ret = "";
    for (iy in [...Array(iysteps)]) {
        let y = iy * (ymax - ymin) * ystep + ymin;
        for (ix in [...Array(ixsteps)]) {
            let x = ix * (xmax - xmin) * xstep + xmin;
            ret += printdensity(mandelconverge(x,y))
        }
        ret += "\n";
    }
    return ret;
}

function main() {
    console.log(mandel(-2.3, -2.0, 0.025 / 2., 0.05 / 2., 156., 80.));
}

main();
