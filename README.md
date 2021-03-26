# sidewinder
A client side web application for building and simulating of biological reactions based on SBML.

Sidewinder is a Delphi, Object Pascal based, client-side browser application that makes use of TMS Web Core to convert Delphi to javascript code.

Build requirements:
- Delphi, https://www.embarcadero.com/products/delphi 
- TMS Web Core, https://www.tmssoftware.com/site/tmswebcoreintro.asp
- TMS FNC UI pack, https://www.tmssoftware.com/site/tmsfncuipack.asp

Note: For Web application to run you must manually copy the libsbml.wasm file to the ..\TMSWeb\Debug\SBML\ directory. Delphi currently does not copy it over at build time.