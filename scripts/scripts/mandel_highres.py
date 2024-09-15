def printdensity(d: int) -> int:
  if d > 127:
    return chr(32)
  elif d > 8:
    return chr(46)
  elif d > 4:
    return chr(43)
  else:
    return chr(42)

def mandelconverge(creal: float, cimag: float) -> int:
    r: float = creal
    i: float = cimag
    for it in range(255):
        if r*r + i*i > 4.:
            return it
        next_r: float = r * r - i * i + creal
        i = 2. * r * i + cimag
        r = next_r
    return it

def mandel(xmin: float, ymin: float, xstep: float, ystep: float, xsteps: float, ysteps: float):
    xmax: float = xmin + xstep * xsteps
    ymax: float = ymin + ystep * ysteps
    ixsteps: i32 = int(xsteps)
    iysteps: i32 = int(ysteps)
    ret = ""
    for iy in range(iysteps):
        y: float = float(iy) * (ymax - ymin) * ystep + ymin
        for ix in range(ixsteps):
            x: float = float(ix) * (xmax - xmin) * xstep + xmin
            ret += printdensity(mandelconverge(x,y))
        ret += "\n"
    return ret

def main():
    print(mandel(-2.3, -2.0, 0.025 / 2., 0.05 / 2., 156., 80.))

if __name__ == "__main__":
    print("print")
    main()
