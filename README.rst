====================================
Parallel File Processing With Erlang
====================================

This tiny project is about processing line-oriented records where order does
not matter. It tries to provide a sufficient solution written in Erlang for
distributing workload across multiple CPU cores [#WideFinder]_.


Copyright and License
---------------------

Copyright (c) 2011, Tobias Rodaebel

This software is released under the Apache License, Version 2.0. You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0


Building and Running
--------------------

For compiling the Erlang programs just enter::

  $ make

You can now run them as follows::

  $ erl -run serial start PATH


Benchmarking
------------

The project includes a very simple Python script `mkdata.py` to generate test
data. In order to generate 5*10^6 lines (~1.1 GB) of test data enter the
following command::

  $ python mkdata.py 5000000 > test.txt

And these are the results of running our programs on different hardware with
the same test data.
 
====== =========== ========= ============= =============
       CPython 3.2 pypy 1.5  Erlang R14B02 Erlang R14B02
       serial.py   serial.py serial.erl    parallel.erl
MBP         10.546     3.091        11.435         1.918
iMac        13.654     6.312        20.634         5.181
====== =========== ========= ============= =============

 - MBP  = MacBook Pro 2.3 GHz Intel Core i7 / SSD
 - iMac = iMac 3.06 GHz Intel Core 2 Duo / HD


Footnotes
---------

.. [#WideFinder] See Time Bray's `Wide Finder Project <http://www.tbray.org/ongoing/When/200x/2007/09/20/Wide-Finder>`_.
