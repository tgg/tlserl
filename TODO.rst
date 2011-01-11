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
* Store the name in State instead of recomputing it every time.
* Remove generated files
* Use same IDL as tlscli
* Open or create mnesia tables
* Package as an app
* Decide style
* Use mnesia:select


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

1. Implement ``write``, ``query`` and ``delete``. COMPLETED ON 2011-01-01
2. Mnesia-ification
3. Package as an app
   Possibly: write ETCL -> qlc:Q converter.
   See http://dukesoferl.blogspot.com/2009/08/metaprogramming-with-ctexpand.html
4. Handle iterator and record attributes
5. Implement record life, size, etc. logic
6. Add ``copy`` and remaining things
