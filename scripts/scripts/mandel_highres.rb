def printdensity(d)
    if d > 127
      return ' '
    elsif d > 8
      return '.'
    elsif d > 4
      return '+'
    else
      return '*'
    end
end

def mandelconverge(creal, cimag)
    r = creal
    i = cimag
    for it in 0..255
        if r*r + i*i > 4
            return it
        end
        next_r = r * r - i * i + creal
        i = 2 * r * i + cimag
        r = next_r
    end
    return it
end

def mandel(xmin, ymin, xstep, ystep, xsteps, ysteps)
    xmax = xmin + xstep * xsteps
    ymax = ymin + ystep * ysteps
    ret = ""
    for iy in 0..ysteps
        y = iy * (ymax - ymin) * ystep + ymin
        for ix in 0..xsteps
            x = ix * (xmax - xmin) * xstep + xmin
            ret += printdensity(mandelconverge(x,y))
        end
        ret += "\n"
    end
    return ret
end

print(mandel(-2.3, -2.0, 0.025 / 2.0, 0.05 / 2.0, 156.0, 80.0))
