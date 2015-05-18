Play Iteratees Extras
=====================

This is an unofficial library for extra Play iteratees that you may find useful.

Since it is unofficial, don't expect any support, bug fixes or updates in any sort of timely manner.  Also don't expect any sort of backwards compatibility between releases.  But do expect to find some iteratees that you may find very useful in your projects.

Currently the library contains the following tools:

* Character encoding enumeratee for decoding streams of byte arrays
* Some useful combinator style iteratees for parsing streams of character arrays
* An error reporting enumeratee for including context to error messages when using combinators
* A full JSON parser, allowing streaming parsing of JSON without writing all the JSON into memory (using a JSON enumeratee parser)
