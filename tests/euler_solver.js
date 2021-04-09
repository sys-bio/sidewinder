
// Euler integrator.

class EulerIntegrator {
    constructor(step) {
        this.step = step;
    }
     // y is expected to be an array.
     // p are parameters and can be an array.
    calc_dydt(p, y, dydt, time, odeFunc) {
        dydt = odeFunc(p,y, dydt, time);
        for(var i=0; i<dydt.length; i++) {
        //  y1 = y0 + h * f(x0, y0)
          dydt[i] = this.step*dydt[i];
          //console.log('*** Euler dydt ',i,': ',dydt[i]);
        }
        return dydt;
    }
}