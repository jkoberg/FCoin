
FCoin
=====

Enough of a bitcoin client implemetation to do address calculations.

Overview
--------

I wanted to generate some [vanity pool addresses](https://vanitypool.appspot.com/faq), but didn't want to trust the 
implementations of the address computation code that's out there.  So I needed to implement enough [ECDSA math](http://kakaroto.homelinux.net/2012/01/how-the-ecdsa-algorithm-works/) to compute the curve points, and the 
standard bitcoin address serialization and deserialization functions to communicate the points to the vanity pool website.

There's not much more of the implementation than that.  I started writing a MtGox API, then MtGox collapsed and I kind of 
lost intereset for a bit.

I was learning F# at the time, and found it to be a great language for this task. 

