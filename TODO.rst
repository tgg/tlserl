===================
TODO for ``tlserl``
===================

Bugs
====

* when a log crash, the logmgr crashes as well


Features
========

Log
---
* **MISSING** ``delete_records_by_id``
* **MISSING** ``destroy``
* **MISSING** ``write_recordlist``
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

Roadmap
=======

1. Implement ``delete_records_by_id`` and ``write_recordlist``
2. Add persistence using Mnesia
3. Handle iterator and record attributes
4. Implement record life, size, etc. logic
5. Add ``copy`` and remaining things
