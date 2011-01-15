===================
TODO for ``tlserl``
===================

Bugs
====

* when a log crash, the logmgr crashes as well => spawn
* Check all argument types for CORBA calls
* use disk_log instead?
* use error_logger

Misc
====

* decide which license to use. How is licensed Erlang itself? What about Erlang
  projects?
* Store the name in State instead of recomputing it every time.
* Use same IDL as tlscli
* Open or create mnesia tables
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
   Package as an app. COMPLETED ON 2011-01-16
   Licence then go public
3. Handle iterator and record attributes
   Possibly: write ETCL -> qlc:Q converter.
   See http://dukesoferl.blogspot.com/2009/08/metaprogramming-with-ctexpand.html
4. Implement record life, size, etc. logic
5. Add ``copy`` and remaining things
