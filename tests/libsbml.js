
var libsbml = (function() {
  var _scriptDir = typeof document !== 'undefined' && document.currentScript ? document.currentScript.src : undefined;
  return (
function(libsbml) {
  libsbml = libsbml || {};






  return libsbml;
}
);
})();
if (typeof exports === 'object' && typeof module === 'object')
    module.exports = libsbml;
  else if (typeof define === 'function' && define['amd'])
    define([], function() { return libsbml; });
  else if (typeof exports === 'object')
    exports["libsbml"] = libsbml;
  