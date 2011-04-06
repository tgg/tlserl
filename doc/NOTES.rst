======
tlserl
======

``tlserl`` is a basic CORBA Telecom Log Service implementation
written in Erlang.

RUNNING
=======

High level
----------

1. Start orber::

   orber:jump_start(1234).

2. Start the dsLogAdminApp appliction::

   dsLogAdminApp:start().

3. Create a new log factory::

   F = dsLogAdminApp:start_logmgr().

4. Create a new log using this factory::

    {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).

5. To retrieve the IOR associated to the log manager instance, use::

    corba:object_to_string(F).


Low level
---------

1. Start orber::

    orber:jump_start(1234).

2. Create a new log factory::

    F='DsLogAdmin_BasicLogFactory':oe_create().

3. Create a new log using this factory::

    {L,Id}='DsLogAdmin_BasicLogFactory':create(F, 0, 0).

4. To retrieve the IOR associated to the log manager instance, use::

    corba:object_to_string(F).
