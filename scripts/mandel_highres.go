package main

func printdensity(d int) string {
	if d > 127 {
		return " "
	} else if d > 8 {
		return "."
	} else if d > 4 {
		return "+"
	} else {
		return "*"
	}
}

func mandelconverge(creal, cimag float64) int {
	r := creal
	i := cimag
	for it := 0; it < 255; it++ {
		if r*r+i*i > 4. {
			return it
		}
		next_r := r*r - i*i + creal
		i = 2.*r*i + cimag
		r = next_r
	}
	return 255
}

func mandel(xmin, ymin, xstep, ystep, xsteps, ysteps float64) string {
	xmax := xmin + xstep*xsteps
	ymax := ymin + ystep*ysteps
	ixsteps := int(xsteps)
	iysteps := int(ysteps)
	ret := ""
	for iy := 0; iy < iysteps; iy++ {
		y := float64(iy)*(ymax-ymin)*ystep + ymin
		for ix := 0; ix < ixsteps; ix++ {
			x := float64(ix)*(xmax-xmin)*xstep + xmin
			ret += printdensity(mandelconverge(x, y))
		}
		ret += "\n"
	}
	return ret
}

func main() {
	println(mandel(-2.3, -2.0, 0.025/2., 0.05/2., 156., 80.))
}
