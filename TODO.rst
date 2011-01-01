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
2. Add persistence using Mnesia, then go public
3. Handle iterator and record attributes
4. Implement record life, size, etc. logic
5. Add ``copy`` and remaining things
