This project aims to create a simple library for automatic
recompilation and reloading of modules into running Haskell
applications. It aims to replace the old, very obsolete plugins
package with a modern library built around the GHC-API.

There is currently a working Example.hs, but surely there are many
things wrong.

Ultimately, this library needs to integrate with cabal and cabal
sandboxes.

Your help is desperately needed!

This library is currently named 'plugins-ng' to indicate that it is
the next generation of the plugins library. However, the name
'plugins' is misleading. 'plugin' implies that there is some sort of
interface/API specified for writing plugins which can be used in an
app. This library provides no such thing -- though it could be used by
higher level libraries like 'web-plugins' to provide dynamic reloading
capabilities. If you have a better name, please speak up!

