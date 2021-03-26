
// Modified From: https://www.npmjs.com/package/ode-rk4

class RK4Integrator {
 constructor( y0, deriv, t, dt, p ) {
  // Bind variables to this:
    this.deriv = deriv   // function which contains eqs to be integrated.
    this.y = y0  // array, Set to inital value
    this.n = this.y.length
    this.dt = dt // Time step
    this.t = t   // Time
    this.p = p;  // parameter array
  // Create a scratch array into which we compute the derivative:
    this._ctor = this.y.constructor
    this._w = new this._ctor( this.n )
    this._k1 = new this._ctor( this.n )
    this._k2 = new this._ctor( this.n )
    this._k3 = new this._ctor( this.n )
    this._k4 = new this._ctor( this.n )
    }
  // ***********************************
     step() {
     this.deriv( this.p, this.y, this._k1, this.t )

     for(var i=0; i<this.n; i++) {
      this._w[i] = this.y[i] + this._k1[i] * this.dt * 0.5
     }
     this.deriv(this.p, this._w, this._k2,  this.t + this.dt * 0.5 )

     for(var i=0; i<this.n; i++) {
       this._w[i] = this.y[i] + this._k2[i] * this.dt * 0.5
      }
     this.deriv( this.p, this._w, this._k3,  this.t + this.dt * 0.5 )
     for(var i=0; i<this.n; i++) {
       this._w[i] = this.y[i] + this._k3[i] * this.dt
     }
     this.deriv( this.p, this._w, this._k4,  this.t + this.dt)
     var dto6 = this.dt / 6.0
     for(var i=0; i<this.n; i++) {
       this.y[i] += dto6 * ( this._k1[i] + 2*this._k2[i] + 2*this._k3[i] + this._k4[i] )
       //console.log( 'RK4solver - y[',i,']',this.y[i]);
     }
     this.t += this.dt
     return this
    }
   // **********************************
    steps( n ) {
      for(var step=0; step<n; step++) {
         this.step()
      }
      return this
    }

}


