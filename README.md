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

The next step forward is to figure out how to use GHC API in
combination with Cabal. We would like to be able to use cabal to
fetch, install, and load libraries from hackage. Additionally, we want
to be able to integrate with cabal sandboxes.

There are two related packages on hackage:

http://hackage.haskell.org/package/scion
http://hackage.haskell.org/package/buildwrapper

These libraries do more than we need. But, hopefully than can serve as
a good reference.
