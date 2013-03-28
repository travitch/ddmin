This is an implementation of ddmin, one of the tools of [delta
debugging](http://www.st.cs.uni-saarland.de/dd/).  Briefly, delta debugging
attempts to minimize an input to some other tool that causes an error; ddmin
takes the input and tries to make it smaller while still causing the *same*
error.

This is an implementation in Haskell that tries to offer some flexible
regular-expression based failure matching criteria.

More documentation coming soon, hopefully.  I think this will also need
a bit of work to build on a newer Haskell Platform.
