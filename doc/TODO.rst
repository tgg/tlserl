===================
TODO for ``tlserl``
===================

Bugs and limitations
====================

* factory object reference is not stored in the oe_tlsbf table
* log processes are not supervised
* list_logs is broken after stopping and restarting orber on an existing
  log_mgr
* log setters are no-op
* log policies are not honoured

Ideas
=====
* use a single factory? start_log_mgr is not user friendly:
   - make it return the factory id as well as the object reference?
   - make it accept any name that will be used as the log process registered
     name?
* persistent IOR? What about imr?
* use disk_log instead?
* add error_logger info, warning and errors where appropriate

Misc
====

* Use mnesia:select?
* Check for race conditions (check for an id then creating it)
* Check all argument types for CORBA calls

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
2. Real app:

   * Mnesia-ification. COMPLETED ON 2011-04-07
   * Package as an app. Licence. COMPLETED ON 2011-01-16
   * Go public. COMPLETED ON 2011-04-07
3. Handle iterator and record attributes
   Possibly: write ETCL -> qlc:Q converter.

   See http://dukesoferl.blogspot.com/2009/08/metaprogramming-with-ctexpand.html
4. Implement record life, size, etc. logic
5. Add ``copy`` and remaining things


Dev notes
=========

1. Start orber::

    orber:jump_start(1234).

2. Create a new log factory::

     F='DsLogAdmin_BasicLogFactory':oe_create().

3. Create a new log using this factory::

     {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).

4. To retrieve the IOR associated to the log manager instance, use::

     corba:object_to_string(F).
