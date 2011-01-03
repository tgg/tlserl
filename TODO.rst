===================
TODO for ``tlserl``
===================

Bugs
====

* when a log crash, the logmgr crashes as well
* Check all argument types for CORBA calls

Misc
====

* decide which license to use. How is licensed Erlang itself? What about Erlang
  projects?

Features
========

Log
---
* **MISSING** ``destroy``
* **MISSING** ``set_record_attribute``
* **MISSING** ``get_record_attribute``
* **MISSING** ``set_records_attribute``
* **MISSING** ``copy``
* **MISSING** ``copy_with_id``
* **MISSING** ``flush``

LogMgr
------

Complete :-)

LogIterator
-----------

Nothing is written

Roadmap
=======

1. COMPLETE
2. Use catch. Make a real app, then go public
3. Write ETCL -> qlc:Q converter. The add persistence using Mnesia.
   See http://dukesoferl.blogspot.com/2009/08/metaprogramming-with-ctexpand.html
4. Handle iterator and record attributes
5. Implement record life, size, etc. logic
6. Add ``copy`` and remaining things
