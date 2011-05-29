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

And these are the results (in seconds) of running our programs on different
hardware with the same test data. For the first series the disk cache was
flushed before each run by rebooting the machine.
 
============ ============== ============== ==========
Machine      Erlang R14B03  Erlang R14B03  pypy 1.5
             serial.erl     parallel.erl   serial.py
MBP                  11.342          7.821      6.254
MBP (cached)         10.684          1.124      3.127
============ ============== ============== ==========

 - MBP  = MacBook Pro 2.3 GHz Intel Core i7 / SSD


Conclusion
----------

As of this writing, Erlang R1403 seems to be relatively inefficient when doing
normal file I/O. Buffering and parallel data processing helps to gain slightly
better results, though. But for anyone who wants to dive deeper into this
matter, I recommend Jay Nelson's talk "Process-Striped Buffering with
gen_stream" [#GenStream]_ he gave at the Erlang Factory 2011.


Footnotes
---------

.. [#WideFinder] See Time Bray's `Wide Finder Project <http://www.tbray.org/ongoing/When/200x/2007/09/20/Wide-Finder>`_.
.. [#GenStream] Jay Nelson on `Process-Striped Buffering with gen_stream <http://erlang-factory.com/conference/SFBay2011/speakers/JayNelson>`_.
